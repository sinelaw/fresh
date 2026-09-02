# Potential Improvements Backlog

This file accumulates UX, documentation, and feature improvement ideas discovered
during automated testing. These are NOT bugs — the application works as intended —
but they represent friction points that real users are likely to hit.

Each entry records: what confused us, what the correct behavior is, and what
change would make it self-evident without requiring users to read docs.

---

## UI / Discoverability

### IMP-001 — Hot Exit: No Indication That Restoration Happened
- **Observed:** On relaunch, files open with `[+]`/asterisk and no explanation.
- **Correct behavior:** Hot exit intentionally restored unsaved changes from the prior session.
- **Problem:** Users (and the test agent) see `[+]` and think something went wrong or the file is corrupted. Nothing tells them "we restored your previous session."
- **Suggested fix:** On first render after a hot-exit restore, show a dismissible status message or notification banner: *"Restored 2 unsaved buffers from your previous session. [`hot_exit` is on — disable in settings]"*
- **Effort:** Low — one status message in the restore path.
- **Discovered:** Run #1, 2026-05-26

---

### IMP-002 — Search Bar: No Hint for F3 / Shift+F3 Navigation
- **Observed:** The search bar shows `[x] Case Sensitive (Alt+C) | [ ] Whole Word (Alt+W) | [ ] Regex (Alt+R)` but no navigation hint.
- **Correct behavior:** After pressing Enter to jump to a match, `F3`/`Shift+F3` navigate next/previous. This is the correct workflow.
- **Problem:** Users (and the test agent) expect Enter to cycle through matches (VS Code behavior). Nothing in the UI signals that Enter closes the bar and F3 takes over.
- **Suggested fix:** Extend the hint line to: `[x] Case Sensitive (Alt+C)  [ ] Whole Word (Alt+W)  [ ] Regex (Alt+R)  |  Enter: jump · F3: next · Shift+F3: prev`
- **Effort:** Very low — add text to the search bar footer.
- **Discovered:** Run #1, 2026-05-26

---

### IMP-003 — Ctrl+H Terminal Compatibility Not Surfaced
- **Observed:** `Ctrl+H` is documented as Find & Replace but in terminals (including tmux) it is transmitted as ASCII `0x08` = Backspace. Pressing it silently deletes text.
- **Correct behavior:** `Ctrl+R` is the reliable Find & Replace shortcut; `Ctrl+H` is the *intended* shortcut but unreliable in terminals.
- **Problem:** Fresh markets "familiar VS Code/Sublime keybindings." VS Code users reach for Ctrl+H and destroy text with no warning.
- **Suggested fixes (pick one or combine):**
  1. Add `Ctrl+H` to the **Calibrate Keyboard** wizard's detection list, with a warning: *"Your terminal sends Ctrl+H as Backspace. The Find & Replace shortcut Ctrl+H may not work — use Ctrl+R instead."*
  2. Add a note to the keyboard reference doc next to the `Ctrl+H` entry: *"Note: many terminals transmit Ctrl+H as Backspace. If this doesn't open Find & Replace, use Ctrl+R."*
  3. On the first occurrence of a "delete previous word" action triggered by `0x08`, offer a one-time tooltip: *"Ctrl+H was received as Backspace. Did you mean Find & Replace? Use Ctrl+R."*
- **Effort:** Low–Medium.
- **Discovered:** Run #1, 2026-05-26

---

### IMP-004 — Menu Selection Highlight Too Subtle
- **Observed:** Navigating the menu bar with arrow keys works, but the selection highlight (`[48;5;25m` dark blue background) is nearly invisible in many terminal themes, making the menu appear unresponsive to keyboard input.
- **Correct behavior:** Arrow key navigation works correctly.
- **Problem:** Users (and the test agent's plain-text captures) cannot tell which menu item is highlighted. We initially reported this as "menu navigation doesn't work" before checking the ANSI output.
- **Suggested fix:** Use a higher-contrast selection color in the menu, or invert text color on selection, consistent with how the command palette highlights items.
- **Effort:** Low — theme/color change.
- **Discovered:** Run #1, 2026-05-26

---

### IMP-005 — Ctrl+W Diverges From VS Code Without Warning
- **Observed:** `Ctrl+W` selects the word under cursor. In VS Code/Sublime, `Ctrl+W` closes the current tab.
- **Correct behavior:** This is an intentional design choice in Fresh (documented in editing.md).
- **Problem:** VS Code users repeatedly pressing Ctrl+W to close tabs instead select words, which is invisible on some lines and confusing otherwise. There is no "close buffer" keyboard shortcut at all by default.
- **Suggested fixes:**
  1. Add a default `Ctrl+W` → "Close Buffer" keybinding, aliasing it alongside the word-select behavior (or make it context-sensitive: close if nothing is selected, select word if cursor is in a word).
  2. OR: In the "Getting Started" / welcome dashboard, call out this specific divergence from VS Code.
  3. OR: When a user presses Ctrl+W 3+ times in quick succession with no effect, show a hint: *"Ctrl+W selects the word under cursor. To close a buffer, use the command palette (Ctrl+P → 'Close Buffer')."*
- **Effort:** Medium (binding change) or Low (documentation/hint).
- **Discovered:** Run #1, 2026-05-26

---

### IMP-006 — "Reload with Encoding" Error Message Could Guide User Better
- **Observed:** `File > Reload with Encoding...` shows "Cannot reload: buffer has unsaved modifications (save first)" when the buffer is dirty.
- **Correct behavior:** The error is intentional — reloading with a different encoding would discard local edits.
- **Problem:** The error message is a dead end. The user knows they have unsaved changes but doesn't know what to do next.
- **Suggested fix:** Extend the message to: *"Cannot reload: buffer has unsaved modifications. Save first (Ctrl+S), or discard changes via Close Buffer → (d)iscard, then reopen."*
- **Effort:** Very low — improve error string.
- **Discovered:** Run #1, 2026-05-26

---

## Documentation Gaps

### IMP-007 — Session Persistence / Hot Exit Needs Prominent Mention on First Launch
- **Observed:** The Dashboard shows git/disk info but no mention of hot exit or session restore.
- **Suggested fix:** On the Dashboard (or a "first run" panel), add one line: *"Your editor state — open files, unsaved changes, and terminal sessions — is automatically saved on quit and restored on relaunch. Configure with `hot_exit` in settings."*
- **Effort:** Low.
- **Discovered:** Run #1, 2026-05-26

---

### IMP-008 — "Split Vertical" Command Creates Horizontal Layout
- **Observed:** The command palette entry "Split Vertical" creates a horizontal layout (two panes stacked, divided by a horizontal line).
- **Correct behavior:** This is consistent with many editors where "vertical split" = a vertical *divider* (side by side), but Fresh's "Split Vertical" creates a *horizontal divider* (stacked). The naming is the reverse of what VS Code users expect.
- **Suggested fix:** Rename to "Split Horizontally" and add a "Split Vertically" (side by side) command. Or add parenthetical descriptions: "Split Vertical (stacked)" vs "Split Horizontal (side by side)".
- **Effort:** Low — rename + add second variant.
- **Discovered:** Run #1, 2026-05-26

---

## Testing Infrastructure

### IMP-010 — "Toggle Line Wrap" Not Available in Command Palette
- **Observed:** "Toggle Line Numbers" is in the command palette, but "Toggle Line Wrap" is not.
- **Correct behavior:** Line wrap can be toggled via View menu (`Alt+V → Line Wrap`).
- **Problem:** Inconsistency — some view toggles are in the palette (line numbers) and others aren't (line wrap). Users reaching for Ctrl+P to toggle wrap won't find it.
- **Suggested fix:** Add "Toggle Line Wrap" to the command palette alongside "Toggle Line Numbers".
- **Effort:** Very low.
- **Discovered:** Run #3, 2026-05-26

---

### IMP-011 — Shift+F3 Key Binding Documentation May Be Inconsistent
- **Observed:** `docs/features/editing.md` documents `Shift+F3` = Find Previous. But the command palette shows "Find Previous" bound to `Ctrl+Shift+N`.
- **Correct behavior:** Unknown without testing in a proper terminal — both may exist simultaneously.
- **Problem:** If `Shift+F3` doesn't actually work (even outside tmux), users will be confused when the documented shortcut does nothing.
- **Suggested fix:** Test `Shift+F3` in a proper xterm/Kitty terminal. If it works, document both. If only `Ctrl+Shift+N` works, update editing.md.
- **Effort:** Low (just testing + doc update).
- **Discovered:** Run #3, 2026-05-26

---

### IMP-009 — No `--headless` or Scriptable Test Mode
- **Observed:** The TUI agent must use tmux to interact with Fresh. This works but is fragile — timing-dependent, ANSI parsing is complex, and key send errors produce hard-to-diagnose bugs (e.g., "S-Left S-Left" sent as literal text).
- **Suggested fix:** A `fresh --test-mode` or pipe-based command interface that accepts structured input (JSON events) and produces structured output (cursor position, buffer content, status) would make automated testing far more reliable.
- **Effort:** High — new subsystem. But would significantly improve the quality of Fresh's own e2e test suite as well.
- **Discovered:** Run #1, 2026-05-26

---

### IMP-012 — Package Manager: Install/Uninstall Buttons Not Reliably Keyboard-Accessible
- **Observed (Run #16):** In the Package Manager panel, selecting a package and pressing Tab reaches an "Enter Activate" position that shows `[ Install ]` brackets in the detail pane. However, pressing Enter at that position opens the Search field rather than triggering the Install action. The Tab cycle order is: Search → filter tabs (All/Installed/Plugins/Themes/Languages/Bundles) → Sync → list items → back to Search.
- **Correct behavior:** The package list and Install/Uninstall buttons should be fully keyboard-operable for accessibility. Currently, installing requires the "Package: Install from URL" command (palette workaround) and uninstalling requires manual `rm -rf` of the plugin directory.
- **No "Package: Uninstall" command exists in the command palette** — users cannot uninstall via keyboard alone without knowing the filesystem path.
- **Problem:** A fully mouse-free workflow (essential for a TUI editor's core audience) is blocked. Power users who rely entirely on keyboards cannot install or uninstall plugins.
- **Suggested fixes:**
  1. Fix the "Enter Activate" binding in the Package Manager so pressing Enter on an Install/Uninstall button actually triggers it.
  2. Add a `Package: Uninstall` (or `Package: Remove`) command to the command palette that operates on the currently-selected/installed package.
  3. Alternatively, expose Install/Uninstall as direct keybindings (e.g., `i` to install, `u` to uninstall) within the Package Manager panel, consistent with how other panels use single-key shortcuts.
- **Effort:** Medium — requires fixing focus/event routing in the Package Manager widget and adding a palette command.
- **Discovered:** Run #16, 2026-05-31

---

### IMP-013 — clangd LSP Does Not Auto-Start; `auto_start` Setting Exists But Not Documented Prominently
- **Observed (Run #18):** After installing clangd and adding `{"lsp": {"c": {"command": "clangd", "args": [], "enabled": true}}}` to `~/.config/fresh/config.json`, Fresh shows `LSP (off)` on launch and the LSP Status popup shows "○ clangd (not running)". The user must manually click "Start clangd (always)" to start the server.
- **Run #19 follow-up:** Found `auto_start` setting in config-schema.json with `default: false` and description: "Whether to auto-start this LSP server when opening matching files. If false (default), the server must be started manually via command palette." This is the INTENDED design — `enabled: true` means "not disabled" and `auto_start: true` means "launch immediately on file open."
- **Doc vs reality mismatch:** `docs/features/lsp.md` states "Install the server and Fresh will use it automatically" — this refers to the CONFIG being pre-built (no user setup needed), NOT to auto-starting the server process. The wording is misleading.
- **Status:** NOT A BUG — `auto_start` is a deliberate opt-in. However, the docs are misleading.
- **Problem:** Users who configure clangd per the documentation expect LSP to start automatically. The `auto_start: true` requirement is not mentioned in the main LSP docs.
- **Suggested fix:** (a) In `docs/features/lsp.md`, change "Fresh will use it automatically" to "Fresh has its configuration pre-built — just install the binary. To auto-start on file open, set `auto_start: true`." Or (b) Change the default to `auto_start: true` for built-in (pre-configured) servers only.
- **Effort:** Very low (docs clarification).
- **Discovered:** Run #18, 2026-06-03; resolved to docs issue in Run #19, 2026-06-03

---

### IMP-014 — Search/Replace Has No "Search in Selection" Option
- **Observed (Run #21):** The Find bar (`Ctrl+F`) has three options: Case Sensitive, Whole Word, Regex. There is no "Search in Selection" or "Find in Selected Text" toggle.
- **Correct behavior:** Searching always spans the entire buffer, regardless of any active text selection.
- **Problem:** Users who want to search/replace within a specific block of text (e.g., replace a variable name in one function only) cannot restrict the search to a selection. They must manually navigate to the region and use match count to avoid going out of bounds.
- **Suggested fix:** Add a 4th toggle to the search bar: `[ ] In Selection (Alt+S)`. When enabled, matches and replacements are constrained to the pre-existing selection. VS Code, Sublime Text, and Vim all support this.
- **Effort:** Medium — requires passing the selection range to the search engine and only highlighting/replacing within it.
- **Discovered:** Run #21, 2026-06-03

---

### IMP-015 — Workspace Trust dialog: Esc does not dismiss; File Explorer force-opens after trust restart
- **Observed (Run #22, v0.3.12):** (a) The SECURITY WARNING dialog ignores Escape — the only ways out are choosing an option + Enter. Users habitually press Esc to defer a decision; here it silently does nothing. (b) After confirming Trust, the post-restart session always opens the File Explorer, even if it was closed before the restart (default-mode restore otherwise preserves buffers + unsaved edits).
- **Suggested fix:** Esc should act as "decide later" (= Keep Restricted for this session, no persistence). Preserve sidebar visibility across the trust restart.
- **Severity:** Low (UX polish). The data-loss aspect of the trust restart is tracked separately as bug #2291.
- **Discovered:** Run #22, 2026-06-09

### IMP-016 — Keybinding editor Add dialog: Enter on Context field silently cancels
- **Observed (Run #22, v0.3.12):** In the Add Keybinding dialog, after typing the action name, Tab lands on the Context field. Pressing Enter there closes the dialog WITHOUT adding the binding — no error, no status message. The user believes the binding was added (it wasn't). Correct path is Tab once more to reach [Save].
- **Suggested fix:** Enter anywhere in the form (except while capturing a key) should submit, per standard form conventions; or show "binding not saved" feedback on cancel-close.
- **Severity:** Low-Medium (silent no-op; cost one full add cycle during testing).
- **Discovered:** Run #22, 2026-06-09

### IMP-017 — Workspace Trust "Blocked": tools fail with generic messages that don't mention trust
- **Observed (Run #23, v0.3.12):** In a folder set to **Block All Execution**, user-facing tools that depend on a subprocess fail with messages that give no hint that workspace trust is the cause. Git Blame shows "No blame information available (not a git file or error)" (it *is* a git file — the git subprocess was denied). Live Grep shows "No matches" (the rg/git-grep subprocess was denied — there ARE matches). The real reason is only in the log: `Process error: workspace trust is set to Blocked — no processes may run`. The status-bar word `Blocked` is the only on-screen hint, and it's easy to miss.
- **Suggested fix:** When a tool's subprocess is denied by trust enforcement, surface a specific status/toast like "Blocked by workspace trust — choose Trust or Keep Restricted to enable git/search". At minimum distinguish "blocked by trust" from "no results / not a git file".
- **Severity:** Low (UX clarity). Enforcement itself is correct; only the messaging is misleading.
- **Reference:** VS Code shows a "Restricted Mode" banner and explains which features are disabled rather than letting them silently no-op.
- **Discovered:** Run #23, 2026-06-10

### IMP-018 — "Send Selection to Terminal" steals focus, blocking rapid repeated sends
- **Observed (Run #31, v0.4.0):** The new "Send Selection to Terminal" command (#1871) moves keyboard focus to the terminal after every send (deliberate, commit 4b4d14946). Side effect: to send a SECOND selection the user must first manually refocus the editor (Alt+J / click). A common workflow — stepping through a script line-by-line, sending each line to the REPL/shell — therefore requires an extra refocus keystroke between every send.
- **Reference:** VS Code's "Terminal: Run Selected Text in Active Terminal" keeps focus in the editor, so you can select → run → select-next → run without leaving the editor. IntelliJ's "Send to console" behaves the same.
- **Suggested fix:** Either keep focus in the editor by default (matching VS Code), or add a config flag (e.g. `terminal.focus_on_send`, default to taste) so power users can disable the auto-focus for line-by-line workflows.
- **Severity:** Low (UX/workflow friction; feature itself works correctly). Buffer is never modified.
- **Discovered:** Run #31, 2026-06-10

### IMP-019 — "Clear Search Highlights" (#2152) benefit unreachable from keyboard/palette without a custom binding
- **Observed (Run #32, v0.4.0):** The new `clear_search` action / "Clear Search Highlights" palette command (#2152) is designed to clear active search highlights *without closing the find widget*. In practice that benefit is unreachable through stock UI: (1) the command has NO default keybinding; (2) invoking it via the command palette closes the find bar first ("Search cancelled."); (3) while the find INPUT is focused, a keybinding bound to it is swallowed by the input (verified: F8→clear_search bound, ignored while find bar focused). The action itself works (it clears persistent highlights left after Enter-closing the find bar) — only the "keep the find widget open" use case is impractical.
- **Suggested fix:** Give `clear_search` a sensible default keybinding (VS Code uses Escape-from-editor / a dedicated binding) and/or route it through find-bar key handling so it can clear highlights while the bar stays open.
- **Severity:** Low (the action functions; this is discoverability/ergonomics for one secondary use case). Mostly relevant to plugins (the PR's main consumer of `has_active_search()`).
- **Discovered:** Run #32, 2026-06-10

### IMP-020 — `LSP (off)` status pill documented as "dimmed" but renders identically to `LSP (on)`
- **Observed (Run #33, v0.4.0):** `docs/features/lsp.md` ("Disabling LSP") states that with `lsp_enabled:false` "the status bar shows a **dimmed** `LSP (off)` pill when servers are configured for the current language". The behavior is correct (no LSP auto-starts, pill reads `LSP (off)`), but ANSI capture (`tmux capture-pane -p -e`) of the status line shows the off pill drawn in the SAME default foreground as the `LSP (on)` pill — no SGR `[2m` (dim) attribute anywhere on the line, no distinct color. Only the literal word changes (`off` vs `on`). The whole status bar uses background `48;5;233`.
- **Suggested fix:** Either actually dim/recolor the `LSP (off)` pill (e.g. SGR 2 or a muted theme fg) so the disabled state is visually distinct as the docs promise, or drop the word "dimmed" from the docs.
- **Severity:** Trivial (cosmetic / doc-vs-render wording). The `lsp_enabled` feature itself is a comprehensive PASS.
- **Reference:** Fresh's own docs (docs/features/lsp.md). VS Code visually distinguishes a disabled/stopped language-status item from an active one.
- **Discovered:** Run #33, 2026-06-11

### IMP-021 — Review Diff: `v`+`d` discard of the lone `+` of a modification fails with "patch does not apply" (must select the whole `-`/`+` pair)
- **Observed (Run #38, v0.4.1):** With the #2317 fix in place, line-level discard works when the visual selection covers a pure addition OR the full `-`/`+` pair of a modification. But selecting ONLY the `+` line of an in-place modification (cursor on `+A powerful calculator.`, `v`, `d`) → `Patch failed: error: patch failed: README.md:1error: README.md: patch does not apply` and nothing is discarded. Reverse-applying just the `+` half of a modification is semantically ambiguous (the original `-` text isn't in the working tree to restore), so this may be expected git behavior rather than a defect — which is why it was NOT filed as a bug.
- **Suggested improvement:** When a visual selection lands on the `+` (or `-`) of a paired modification, either auto-expand the discard to the whole change-block (VS Code "Revert Selected Ranges" reverts the region to HEAD), or surface a clearer message ("select the full change to discard a modified line") instead of the raw `patch does not apply`.
- **Severity:** Low (workaround = select the full pair, which works). Stage (`v`+`s`) of a lone `+` succeeds because adding-the-line is unambiguous; discard is the asymmetric case.
- **Reference:** VS Code "Revert Selected Ranges" / `git checkout -p`.
- **Discovered:** Run #38, 2026-06-22

### IMP-022 — vi mode "Vim compatibility options" feature is not discoverable (no Settings toggle, no palette command, no docs)
- **Observed (Run #42, v0.4.1 @ eb3a349e6):** master added `b82b9b8b4` "feat: add Vim compatibility options to vi mode". As a black-box user there is no way to discover or configure these "options": the Settings → "Plugin: vi_mode" panel exposes only `ArrowKeys`, `AutoStart`, `SearchWordUnderCursor`; the command palette has no "compat"/"Vim compatibility" entry; and `docs/features/editing.md` §Vim Mode is a single sentence with no mention of compatibility behavior or options. The user-visible effect is purely a behavior change in motions (see #2437), with nothing to read or toggle.
- **Suggested improvement:** Document the vi "Vim compatibility" behavior/options in `docs/features/editing.md` (and CHANGELOG), and — if they are meant to be configurable — surface them as toggles in the "Plugin: vi_mode" Settings panel so users can find/adjust them.
- **Severity:** Low (discoverability/doc gap). Note batch candidate for the periodic "docs/UX polish" issue (R3).
- **Reference:** Fresh's own Settings UI exposes other plugin options; new user-facing features are normally in CHANGELOG + docs.
- **Discovered:** Run #42, 2026-06-22

### IMP-023 — vi mode: several standard Vim commands are unimplemented (no-ops / fall-through)
- **Observed (Run #44, v0.4.1 @ 3b8c2eca1):** while sweeping vi-compat motions, these standard Vim commands do nothing (and some silently fall through to other commands):
  - **`R` (Replace/overtype mode):** `R` keeps `-- NORMAL --`; the next key runs as an ordinary NORMAL command (observed `R`+`A` firing append-at-EOL; `R`+`x` deleting a char). No overtype mode at all.
  - **`gU`/`gu`/`g~` case operators:** no-ops — `gUw` left the word unchanged and `w` only moved the cursor (the `g`+`U` prefix was swallowed). NB the single-char `~` toggle DOES work; only the `g`-prefixed *operators* are missing.
  - (Related find-char repeat `;`/`,` no-op is filed as a behavioral bug in #2441, since pure `f`/`t` work.)
- **Suggested improvement:** implement these as part of the ongoing "Vim compatibility motions" work, or document the supported-motion subset so Vim users know what's available. Good candidate for ONE consolidated "vi mode: missing standard commands" issue once the full gap list is characterized (rather than one issue per command).
- **Severity:** Low–Medium (missing features, not broken behavior; a Vim user reaching for `R`/`gU` just sees nothing happen — `R`'s fall-through to `A` is mildly surprising). Not individually filed per R3.
- **Discovered:** Run #44, 2026-06-22

### IMP-024 — virtual space: a buffer under `virtual_space:"block"` config can never return to block mode after using the per-buffer toggle
- **Observed (Run #47, v0.4.3 @ 9f6135001):** with `editor.virtual_space:"block"` set in config, running "Toggle Virtual Space (Current Buffer)" moves the buffer to `on`, and from there the toggle only cycles `on` ↔ `off`. There is no palette command or per-buffer path back to `block` — the user must restart (or open a new buffer) to get their configured block-only behavior back. The toggle's description ("Turn virtual space on or off") matches the implementation, but a block-mode user who taps it once loses their mode silently.
- **Suggested improvement:** cycle through the buffer's configured mode (off → configured-mode → on → off), or add a "Set Virtual Space Mode (Current Buffer)" picker (off/on/block), or show the current mode in the status message so the loss is at least visible.
- **Severity:** Low (edge; block mode itself works fine). Batch candidate for the periodic "docs/UX polish" issue (R3).
- **Discovered:** Run #47, 2026-07-07

### IMP-025 — no editor-area right-click context menu (and `menu.terminal.*` i18n items unreachable in any menu)
- **Observed (Run #49, v0.4.3 @ 89d91e84d, via the SGR mouse harness):** right-clicking in the editor text area — with or without an active selection — produces literally NO screen change (verified by full-pane before/after diff). Right-clicking inside a terminal pane's grid likewise shows nothing. Context menus DO exist elsewhere: tab right-click (7 items, works great), File Explorer (`explorer.context.*`), orchestrator rows.
- **Related:** the i18n bundle ships `menu.terminal.open` / `menu.terminal.close` / `menu.terminal.send_selection` / `menu.terminal.toggle_keyboard_capture` ("Terminal" menu strings), but no Terminal menu ever renders — the menu bar is static (File/Edit/View/Selection/Go/LSP/Help) with a terminal focused too, and File/Edit contain no terminal items. Run #31's assumption that `menu.terminal.send_selection` lives on a right-click menu appears wrong — the strings are currently unreachable in the UI (the *commands* exist in the palette and work).
- **Suggested improvement:** an editor right-click menu (Cut/Copy/Paste/Send Selection to Terminal…) would match VS Code/Sublime muscle memory now that mouse support is otherwise rich (drag-select, double-click-drag, menus, popups); alternatively render the Terminal menu the i18n strings imply.
- **Severity:** Low (no doc promises an editor context menu; all commands reachable via palette/menus). Not filed per R3; batch candidate.
- **Discovered:** Run #49, 2026-07-07

### IMP-026 — File Explorer context menu papercuts (empty-space menu, New File status leak)
- **Observed (Run #50, v0.4.3 @ 4e945b494):** (1) right-clicking EMPTY explorer space shows the same full 10-item menu (Rename/Cut/Delete/Duplicate/Copy path… all acting on the *current selection*, possibly off-screen); VS Code shows a reduced New File/New Folder/Paste menu for empty space — target-dependent items on an untargeted click surprise. (2) "New File" creates `untitled_<unix-ts>.txt` then renames it, and the status message leaks the internal flow: `Renamed untitled_1783409193.txt to gamma50.txt` instead of "Created gamma50.txt". (3) New Directory's prefilled default is `New Folder <unix-ts>` — a timestamp default is odd next to VS Code's empty-input placeholder.
- **Severity:** Low, cosmetic/convention; menu itself works (see learning_db Run #50). Not filed per R3; batch candidate. NB the real keyboard-grab defect is filed as #2587.
- **Discovered:** Run #50, 2026-07-07

### IMP-027 — Review Diff: untracked NESTED-REPO directory renders a blank-named `?  +0 -0` child row (#2315 artifact, new trigger)
- **Observed (Run #51, v0.4.3 @ 4e945b494):** in an outer repo containing a nested git sub-repo (`vendored/`, untracked in the outer repo), Review Diff from an outer buffer shows `UNTRACKED ▾ vendored/ +0 -0` with a child row whose name is completely blank (`   ?   +0 -0`) and no content. This is the exact #2315 symptom (fixed for normal untracked dirs in 0.4.2) resurfacing for a directory git cannot expand — git never lists files inside a nested repository, so the expansion yields one empty placeholder. Expected: show `vendored/` as a single unexpandable row (VS Code shows one untracked entry), no blank child.
- **Severity:** Low (cosmetic; the dir header row is present and nothing is blocked). Noted in #2592's body as a related observation; not filed separately per R3. If maintainer touches #2315 territory again, fold this in.
- **Discovered:** Run #51, 2026-07-07

### IMP-028 — docs/features/lsp.md names a nonexistent palette command "Switch Rust Analyzer Mode" (actual: "Rust LSP: Configure Mode")
- **Observed (Run #55, v0.4.3 @ 4e945b494):** lsp.md §Rust LSP Mode Switching says `Use "Switch Rust Analyzer Mode" from the command palette`; searching the palette for that phrase finds nothing. The real command is **"Rust LSP: Configure Mode"** (source `rust-lsp`, desc "Switch rust-analyzer between full and reduced memory modes"). Extra papercut: fuzzy query "Switch Rust" ranks "Switch to Previous Tab" above it.
- **Severity:** Low doc mismatch. Noted inside #2598's body (footer). Batch into the next docs/UX polish issue per R3; if #2598 gets fixed the doc line may get corrected with it — check before batching.
- **Discovered:** Run #55, 2026-07-07

## IMP-029: Diagnostics UI polish batch (Run #56, v0.4.3 @ 11dccfad5) — candidates for one "diagnostics polish" issue
- Panel header stays "Diagnostics (Current File):" after `a` toggles to All Files (the toggle itself works — list gains other-file sections; status msg "Showing: All Files" is correct). Header never updates.
- Panel item count ("Diagnostics: 5 items") counts the UNFILTERED set while the filtered view shows 4 rows.
- Status-bar echoes (F8 stops) concatenate multiple messages at one position with NO separator ("…of `Point`missing `label` and `y`") and flatten multiline messages ("missing structure fields:- y- label").
- Gutter ● is always red `38;5;160`, even for warning-only lines — hover fusion and inline text ARE severity-coloured (203/226); the gutter can't distinguish E from W (VS Code does).
- Panel truncates rust-analyzer's multiline message to its first line ("missing structure fields:").
- Diagnostic related-information renders as a bare top-level file section ("mod.rs:" — a rustc std internals path) instead of nesting under the parent diagnostic (VS Code nests).
- Panel Up/Down preview on another file's row opens that file as a persistent tab (VS Code uses a transient preview); tab stays open after the panel closes.
- Signature help does not visually emphasize the active parameter inside the signature line (it IS shown as a separate line below, which advances on comma — different-but-workable presentation; nit only).

## IMP-030: docs/configuration/index.md formatter example uses an invalid string form that discards the whole config layer (Run #57, v0.4.3 @ 6ab255709)
- **Observed:** `docs/configuration/index.md:203` shows `"formatter": "prettier --write"` (plain string) as the project-config example. Using exactly that shape → warnings log `Failed to load layered config: Parse error: <path>: invalid type: string "...", expected struct FormatterConfig, using defaults` + `[⚠ 1]` badge — and the ENTIRE layered config is dropped to defaults (not just the one field). The correct struct form `{"command": ..., "args": [...]}` is documented only in `docs/plugins/development/language-packs.md:88-118` (which also documents the auto-appended file path).
- **Why it matters:** the main configuration guide is the first place a user looks; following it silently disables their whole project config (badge is easy to miss). Fix is a one-line doc change (or accept the string form by parsing it into command+args).
- **Severity:** Low-med doc bug (R3 → batch). Related behavioral nit: one bad field discarding the entire layer is harsh, but Run #53 already characterized broken-config handling (no-clobber + warning) as acceptable.
- **Also:** "Formatting not supported by LSP server" is the message when NO LSP is configured at all (Text buffer) — copy implies a server exists and lacks the capability.
- **Discovered:** Run #57, 2026-07-07

## IMP-031: Fold ellipsis renders mid-header when the fold-header line wraps (Run #58, v0.4.3 @ 6ab255709)
- **Observed:** with line wrap ON and a fold whose header line itself wraps (45-col window, `fn describe(p: &Point, verbose: bool) -> String {`), collapsing the fold appends the `...` ellipsis to the END OF THE FIRST VISUAL ROW of the header, while the header's own continuation row (`bool) -> String {`) still renders below it. Reads as if the ellipsis marks the fold point mid-signature. VS Code renders the full (wrapped) header and puts the `…` after the header's logical end.
- **Severity:** Low, cosmetic; the fold itself is correct (hidden range exact, unfold restores). Batch per R3.
- **Discovered:** Run #58, 2026-07-07.

## IMP-032: `next_pane`/`prev_pane` (#2562, v0.4.4) have no default keybinding AND no palette command — unreachable without manual keybinding config (Run #59b, v0.4.4 @ f545a75)
- **Observed:** the new v0.4.4 pane-cycling actions **`next_pane`** ("Next pane") and **`prev_pane`** ("Previous pane") — the headline of #2562 ("cycle through every split+tab pane as one flat list, distinct from Next/PrevSplit and Next/PrevWindow") — ship with **no default key** (blank key column in the Keybinding Editor) and **no command-palette entry** (palette search `pane` returns 0 results). A user who reads the 0.4.4 changelog cannot invoke the feature at all without first opening the Keybinding Editor and binding a key by internal action name.
- **Contrast:** the sibling split-cycling actions ARE discoverable — palette shows **Next Split** / **Previous Split** (with `prev_split` = `Alt+[`). So the flat-pane variant is strictly less reachable than the split variant it's meant to complement.
- **Correct behavior:** either give `next_pane`/`prev_pane` a sensible default binding, or (matching the split pair) expose **Next Pane** / **Previous Pane** palette commands. Same pattern as #2122 `move_to_paragraph_*`.
- **Severity:** Low–medium discoverability; the actions bind and load fine once you know the names. Not a bug. Batch per R3.
- **Discovered:** Run #59b, 2026-07-20. Runtime *cycling behavior* still UNVERIFIED (harness focus-detection limits → IMP-009). This directly informs the Run #60 candidate (d) NextPane/PrevPane — the black-box discoverability half is already done here.
---

## IMP-033: Ctrl+] terminal-mode-exit alias is unreliable in plain terminals (0x1D encoding) (Run #59, v0.4.4 @ f545a75ad)
docs/features/terminal.md documents `Ctrl+]` as "Exit terminal mode (same as Ctrl+Space)". In plain tmux (no Kitty Keyboard Protocol / CSI-u), Ctrl+] arrives as raw 0x1D and is forwarded to the shell rather than intercepted — the mode never changes (Ctrl+Space works). Same class as the Ctrl+H↔Backspace collision (#2109). Candidate for the periodic "docs/UX polish" batch: either note the terminal-compat caveat next to Ctrl+] in the docs, or add it to the Calibrate Keyboard detection list. Low severity; primary toggle (Ctrl+Space) is unaffected. (Pending PC-59a in confirmed_bugs.)

## IMP-034: `--no-restore` does not give a fully clean terminal (persisted scrollback reloads) (Run #59, v0.4.4)
`fresh --no-restore` is documented as "Don't restore the previous workspace" and is the recommended "clean slate". But terminal scrollback is persisted independently per-workspace (`~/.local/share/fresh/terminals/<enc-root>/fresh-terminal-N.txt`) and a freshly-opened terminal reloads the previous session's scrollback even under `--no-restore`. Plausibly intended (scrollback behaves like shell history), but a user reaching for `--no-restore` for a clean slate may be surprised to see a prior session's command history in a new terminal. Candidate doc clarification (state that terminal scrollback persists separately from workspace restore). Low severity. (Pending PC-59b in confirmed_bugs.)

## IMP-035 (Run #61): Run Agent/New Workspace dialog silently reuses the last custom agent command for future workspaces
The dialog remembers the last-used Agent — including `custom…` and its full command — across opens AND across both palette entry points. A user who once launched a custom command and later creates a "New Workspace" without glancing at the Agent row gets that command re-run in the new workspace's terminal. Visible in the dialog (so arguably by-design sticky defaults), but the blast radius of a remembered arbitrary command is bigger than a remembered dropdown choice; consider resetting Agent to `terminal` (or the last non-custom agent) for New Workspace, or showing the custom command in the collapsed view. Cost us a near-false-positive ("scrollback contamination") in Run #61.

## IMP-036 (Run #61): "Teach agent the Fresh CLI" unavailable when launching an agent in the CURRENT workspace
scripting.md says the launch dialog lets you "decide whether to teach it that it can drive the editor at all", but the toggle only exists in New-workspace mode's Advanced section; Run Agent → Current workspace offers no Advanced section and no toggle (claude/codex/opencode). Either the current-workspace launch always/never teaches (undocumented either way) or the option was lost in the unified-dialog refactor. Doc or UI should say which.
