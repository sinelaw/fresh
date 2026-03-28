# Diagnostics Panel Usability Evaluation

**Feature:** LSP Diagnostics Panel (command palette: "Show Diagnostics Panel" / "Toggle Diagnostics Panel")
**Version:** Fresh 0.2.18
**Test environment:** 120x40 terminal, pylsp with pyflakes (Python), tmux session
**Evaluation method:** Hands-on manual testing via tmux, nngroup heuristic evaluation

---

## Summary

The diagnostics panel displays LSP diagnostics in a split panel below the editor. It supports file grouping, severity indicators, cursor-synced preview, and jump-to-location. The core concept is sound and the preview-on-navigate behavior is excellent, but **three critical bugs** and several usability issues significantly degrade the experience.

**Overall rating: 3/10** (unusable for reliable workflows due to blocking bugs; strong potential if bugs are fixed)

---

## Bugs Found

### BUG 1: `isOpen` state desyncs when closing with Escape (Critical)

**Steps to reproduce:**
1. Open diagnostics panel via "Show Diagnostics Panel"
2. Press Escape to close (panel closes visually)
3. Run "Show Diagnostics Panel" again

**Expected:** Panel reopens.
**Actual:** Nothing happens. The plugin's `isOpen` flag is still `true` because Escape closes the Finder panel but doesn't call the plugin's `diagnostics_close()` handler. The `show_diagnostics_panel` function sees `isOpen === true` and only calls `provider.notify()` on an already-closed panel.

**Impact:** Users cannot reopen the panel after closing it with Escape. They must use "Toggle" twice (once to clear phantom state, once to reopen). This completely breaks the show/close cycle.

**Root cause:** The Finder's built-in Escape handler closes the panel view without notifying the plugin. The plugin registers `_finder_diagnostics_panel_close` for Escape in the mode, but the Finder's own Escape handling runs first and doesn't call `diagnostics_close()`.

### BUG 2: Custom mode keybindings ('a', 'r') do not work (Critical)

**Steps to reproduce:**
1. Open diagnostics panel
2. Focus is on the *Diagnostics* buffer
3. Press 'a' (should toggle all-files filter)
4. Press 'r' (should refresh)

**Expected:** 'a' toggles between "Current File" and "All Files"; 'r' refreshes.
**Actual:** Both keys show "Editing disabled in this buffer" error in the status bar. The keys are treated as text input attempts on a read-only buffer.

**Impact:** The all-files toggle and manual refresh are completely inaccessible. The status bar misleadingly shows "a: toggle filter" as a hint, but the feature doesn't work.

**Root cause:** The `diagnostics-extra` mode defined via `editor.defineMode()` is never activated when the panel opens. The Finder likely uses its own mode for the panel buffer and doesn't compose with the plugin's custom mode.

### BUG 3: Enter/jump-to-location navigates to wrong line (Major)

**Steps to reproduce:**
1. Open diagnostics panel with diagnostics present
2. Navigate to `[E] 17:5 undefined name 'undefined_variable'` (Item 2/2)
3. Press Enter

**Expected:** Cursor jumps to line 17, column 5 in the source file.
**Actual:** Cursor jumps to line 2, column 5. The column is correct but the line is wrong.

**Impact:** Jump-to-location, the primary action of the panel, doesn't land on the correct line. Observed across multiple test runs — the line number consistently maps incorrectly (appears to use the panel's internal line number rather than the diagnostic's source line).

---

## Heuristic Evaluation (Nielsen's 10 Usability Heuristics)

### 1. Visibility of System Status: 6/10

**Positives:**
- The status bar shows diagnostic count (`Diagnostics: 4 items`)
- Item position indicator (`Item 1/2`, `Item 2/2`) is helpful
- The `[W]` and `[E]` severity prefix tags are clear
- File grouping with `errors.py:` header provides context
- The editor status bar shows `E:2 W:1` counts at all times (outside the panel)

**Negatives:**
- No visual highlight on the currently selected diagnostic row (can't tell which row the cursor is on without looking at the status bar)
- The title says "Diagnostics (Current File):" but there's no indication of which file is the "current file" when the panel itself is focused
- "Diagnostics: 4 items" vs "Item 2/2" — the total count (4) doesn't match the selectable items (2). The extra 2 are the file header and help text, counted as "items" but not navigable targets

### 2. Match Between System and Real World: 7/10

**Positives:**
- `[E]` / `[W]` severity codes are standard and immediately understandable
- `line:column message` format is the universal diagnostic convention
- "Enter:select | Esc:close" uses familiar interaction vocabulary

**Negatives:**
- "Show Diagnostics Panel" vs "Toggle Diagnostics Panel" — users must understand the difference. "Show" implies idempotent open, but due to Bug #1, it behaves differently on first vs subsequent calls
- The term "select" for Enter is ambiguous — does it select? jump? Both?

### 3. User Control and Freedom: 3/10

**Positives:**
- Escape closes the panel (even though state desyncs)
- Split view allows editor and panel to coexist

**Negatives:**
- Cannot reopen after Escape close (Bug #1) — user is trapped in a broken state
- Cannot toggle between all-files and current-file views (Bug #2)
- Cannot refresh diagnostics manually (Bug #2)
- No keyboard shortcut to open/close the panel (must use command palette every time)
- After jumping to a diagnostic (Enter), focus moves to editor but no obvious way to return to the panel — user must know Alt+] (split navigation), which is not discoverable
- No way to resize the panel from within (ratio is fixed at 0.3)

### 4. Consistency and Standards: 5/10

**Positives:**
- Uses the same Finder abstraction as other panels (references, search results), so structural behavior is consistent
- Split pane layout follows VS Code's "Problems" panel convention
- `Ctrl+P` command palette access is standard

**Negatives:**
- VS Code's Problems panel is always available via a dedicated shortcut (`Ctrl+Shift+M`); here you must navigate the command palette each time
- The panel has no dedicated keybinding, unlike other features that have `Ctrl+` or `F-key` shortcuts
- "Show" and "Toggle" commands coexist but behave inconsistently due to state bug
- Severity icons `[E]`/`[W]` differ from the `●` inline markers in the gutter — inconsistent visual language

### 5. Error Prevention: 4/10

**Negatives:**
- No guardrail against the state desync — the system silently fails when "Show" is called on a phantom-open panel
- Pressing 'a' or 'r' shows "Editing disabled" — a confusing error message when the user expected a command, not a text edit
- The "No results" empty state when diagnostics exist (but in other files) gives no hint to try toggling to "All Files"

### 6. Recognition Rather Than Recall: 5/10

**Positives:**
- "Enter:select | Esc:close" help text is visible at all times
- Status bar shows "a: toggle filter | RET: ..." as a hint
- Severity tags `[E]`/`[W]` are self-documenting

**Negatives:**
- The "a: toggle filter" hint in the status bar is misleading since the feature doesn't work (Bug #2)
- No visible indication of which diagnostic is currently highlighted/selected
- No documentation within the panel about how to navigate back after jumping
- Split navigation (Alt+]) is not discoverable from the panel context

### 7. Flexibility and Efficiency of Use: 3/10

**Positives:**
- Preview-on-navigate (cursor sync) is excellent — you can scan diagnostics and see the source location without committing to a jump
- File grouping reduces visual noise when viewing multiple files

**Negatives:**
- No keyboard shortcut for the panel (must use command palette every time — 3+ keypresses minimum)
- Cannot filter by severity (errors only, warnings only)
- Cannot search/filter within the diagnostics list
- No "Next diagnostic" / "Previous diagnostic" navigation while the panel is open
- The all-files toggle is inaccessible (Bug #2)
- No way to copy a diagnostic message

### 8. Aesthetic and Minimalist Design: 7/10

**Positives:**
- Clean, uncluttered layout with clear hierarchy: title → file header → diagnostic entries → help
- The 30% split ratio is reasonable — enough space for several diagnostics without overwhelming the editor
- Severity indicators are concise (`[E]`, `[W]`) rather than verbose
- File grouping headers are visually distinct

**Negatives:**
- The `~` tilde filler for empty lines adds visual noise (19 lines of tildes for 3 diagnostics)
- With only 2-3 diagnostics, 70% of the panel is empty space — could auto-size
- The tab bar shows `*Diagnostics* ×` alongside file tabs, which clutters the tab strip
- No color differentiation between `[E]` and `[W]` in the panel itself (both appear in the same color in the tmux capture — though this may be theme-dependent)

### 9. Help Users Recognize, Diagnose, and Recover from Errors: 2/10

**Negatives:**
- "Editing disabled in this buffer" for 'a'/'r' keys gives no useful guidance — user doesn't know why a documented feature isn't working
- "No results" when diagnostics exist in other files — no suggestion to try "All Files" mode
- After the state desync (Bug #1), there is no error message at all — the panel simply fails to appear silently
- "No item selected" when pressing Enter on a non-diagnostic row — correct but could suggest navigating to a diagnostic first

### 10. Help and Documentation: 4/10

**Positives:**
- "Enter:select | Esc:close" inline help is always visible
- Status bar hints show available actions
- Command palette descriptions are helpful ("Open the diagnostics panel", "Toggle the diagnostics panel")

**Negatives:**
- The inline help omits 'a' and 'r' keybindings (they appear only in the truncated status bar)
- No indication of how to navigate between panel and editor splits
- No tooltip or hover information for diagnostic entries
- No help text explaining what "Current File" vs "All Files" means or how to switch

---

## Task Completion Analysis

| Task | Success | Notes |
|------|---------|-------|
| Open diagnostics panel | Partial | Works on first attempt; fails after Escape close |
| View diagnostics for current file | Yes | Displays correctly when panel opens |
| Navigate between diagnostics | Yes | Arrow keys work; preview sync is excellent |
| Jump to diagnostic location | Fail | Lands on wrong line (Bug #3) |
| Toggle all-files filter | Fail | 'a' key non-functional (Bug #2) |
| Refresh diagnostics | Fail | 'r' key non-functional (Bug #2) |
| Close panel | Partial | Escape works visually but corrupts state (Bug #1) |
| Reopen after closing | Fail | "Show" command silently fails after Escape |
| Return to panel after jump | Partial | Requires knowing Alt+] (undiscoverable) |
| Toggle panel on/off | Partial | Works if used consistently; breaks if mixed with "Show" |

**Core workflow success rate: 3/10 tasks fully succeed**

---

## Recommendations

### P0 — Must Fix (Blocking)

1. **Fix `isOpen` state tracking**: The Finder's close callback must call `diagnostics_close()` to sync the plugin state. Alternatively, have `show_diagnostics_panel` check if the panel is actually visible (not just the `isOpen` flag).

2. **Fix custom mode keybindings**: Ensure the `diagnostics-extra` mode is activated when the panel is focused. Either compose it with the Finder's panel mode, or register 'a'/'r' as Finder-level keybindings.

3. **Fix jump-to-location line mapping**: The Enter handler should use the diagnostic item's `location.line` property, not the panel's cursor line number.

### P1 — Should Fix (UX)

4. **Add a dedicated keyboard shortcut** (e.g., `Ctrl+Shift+M` like VS Code) for toggling the diagnostics panel without the command palette.

5. **Add visual highlight** on the currently selected diagnostic row (background color change).

6. **Auto-size the panel** height or provide a reasonable minimum to avoid 70% empty space with few diagnostics.

7. **Improve empty state**: When "Current File" shows no results but other files have diagnostics, show "No diagnostics in current file. Press 'a' for all files." instead of just "No results".

### P2 — Nice to Have

8. Add severity filtering (errors only, warnings only).
9. Add a "Copy diagnostic message" action.
10. Include 'a' and 'r' keybindings in the inline help text (not just status bar).
11. Show diagnostic source (e.g., "pyflakes", "pylint") alongside the message.
12. Consider keeping the panel open after jump (with focus on editor) so users can navigate back easily.
