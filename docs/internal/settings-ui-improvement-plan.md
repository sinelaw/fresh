# Settings UI Improvement Plan

## Overview

This plan systematically UX tests the Settings UI dialogs against UI design principles and NNGroup best practices, identifies gaps, and defines fixes in priority order. Two rounds of testing have been completed; this document reflects the current state after significant fixes were applied.

## Architecture Context

Key files:

| File | Purpose |
|------|---------|
| `crates/fresh-editor/src/view/settings/input.rs` | Input routing: text editing, dropdown, navigation, entry dialog |
| `crates/fresh-editor/src/view/settings/entry_dialog.rs` | EntryDialogState: focus management, items, buttons |
| `crates/fresh-editor/src/view/settings/render.rs` | All rendering including entry dialog, buttons, help text |
| `crates/fresh-editor/src/view/settings/items.rs` | SettingControl enum, build_item_from_value |
| `crates/fresh-editor/src/view/settings/state.rs` | SettingsState, dialog stack, config layer management |
| `crates/fresh-editor/src/view/controls/map_input/mod.rs` | MapState, get_display_value() |
| `crates/fresh-editor/src/types.rs` | LspServerConfig, LspLanguageConfig |

---

## Testing Methodology

- Three parallel test agents using tmux with `capture-pane -p -e` (ANSI color capture)
- Evaluated against NNGroup's 10 usability heuristics
- Tested at all 4 dialog nesting levels
- ANSI escape code analysis for highlight/focus consistency

### Dialog Hierarchy

```
Level 1: Main Settings panel (sidebar + content)
Level 2: Edit Value dialog (e.g., LSP language entry — shows array of servers)
Level 3: Edit/Add Item dialog (e.g., individual LSP server — shows all fields)
Level 4: Inline editing / sub-dialogs (text edit, JSON code block, nested map Add Value)
```

---

## Fixed Issues (Verified in Round 2)

These bugs from Round 1 are now confirmed fixed:

| ID | Description | Status |
|----|-------------|--------|
| C1 | Text input not rendering in Edit Item dialog | **FIXED** — Characters appear immediately, rendered in white (`38;5;15`) on dark bg (`48;5;16`) |
| H3 | Down-arrow skips Command field | **FIXED** — Navigation is fully symmetric. Complete order: Command → Enabled → Name → Args → Auto Start → Root Markers → Advanced |
| H4 | Ctrl+S doesn't work in entry dialogs | **FIXED** — Ctrl+S saves and closes Edit Item from any field, including during text editing |
| M1 | Name field opens wrong dialog type | **FIXED** — Name field is now inline text edit with bracket-style input `[ ]` |
| M3 | LSP entries display `[1 items]` instead of command | **FIXED** — Entries show command names (e.g., `python → pylsp`, `rust → rust-analyzer`). Long names truncated with `...` |
| M4 | Parent dialog not dimmed when child opens | **FIXED** — Progressive dimming implemented. Active dialog: bright cyan borders (`38;5;14`). Parent dimmed: gray text (`38;5;59`), dark teal borders (`38;5;29`) |

---

## Current Bug List (Prioritized)

### Critical Bugs

#### C2: Text fields trap keyboard — no way to exit edit mode
**Tested (Round 2):** Navigate to Command field in Edit Item dialog, press Enter to edit, type text (renders correctly now). Press Escape.
**Expected:** Exit text edit mode, return to field navigation.
**Actual:** Escape does nothing. Field stays in edit mode (cursor `[7m]` still active, label still blue `38;5;25`). Down/Up/Tab all captured and do nothing. The ONLY way out is Ctrl+S (which saves and closes the entire dialog).
**Impact:** Users cannot navigate away from a text field once they start editing. They must Ctrl+S to escape, which also closes the dialog — preventing them from editing other fields.
**NNGroup violation:** User Control and Freedom — "Users often perform actions by mistake. They need a clearly marked 'emergency exit'."

#### C3: Enter on existing array item in Edit Value (Level 2) closes dialog
**Tested (Round 2):** In Level 2 Edit Value dialog showing `-> pylsp [x]`, navigate to array item and press Enter.
**Expected:** Opens Edit Item dialog (Level 3) for that server entry.
**Actual:** Dialog closes, back at Settings (Level 1).
**Workaround:** Press Enter on the "Value:" header label instead.

### High Priority Bugs

#### H1: Tab only toggles to Save button, never reaches Delete/Cancel
**Tested (Round 2):** In Edit Item dialog, Tab acts as binary toggle:
- Odd presses: Focus jumps to Save button only (bold+reverse `[1;7m]`)
- Even presses: Focus jumps back to current field
Tab NEVER reaches Delete or Cancel buttons. Shift+Tab is identical to Tab.
**Workaround:** Down arrow navigates sequentially through all fields AND all three buttons (Save → Delete → Cancel → wraps to first field).
**NNGroup violation:** Consistency and Standards — Tab should cycle through all interactive controls per platform conventions.

#### H2: [+] Add new buttons not keyboard-focusable in Level 3 sub-sections
**Tested (Round 2):** In Edit Item dialog, Args/Env/Language Id Overrides/Root Markers show `[+] Add new` buttons. Down arrow navigation skips all of them.
**Note:** At Level 1 settings, `[+] Add new` buttons (e.g., under Keybindings) ARE reachable. Bug is specific to Level 3 context.
**Additional:** When pressing Enter on a section like Args, an "expanded sub-edit mode" activates where Down no longer navigates away and Tab does nothing. Only Esc escapes.
**NNGroup violation:** Keyboard Accessibility (WCAG 2.1 Level A) — All visible interactive elements must be keyboard-reachable.

#### H5: Individual Root Marker/TextList items not keyboard-accessible
**Tested (Round 2):** Root Markers shows items (pyproject.toml, setup.py, etc.) with [x] delete buttons. Cannot focus individual markers or delete buttons via keyboard.

#### H6 (NEW): Text field auto-enters edit mode — accidental modifications
**Tested (Round 2):** When navigating to a text field (Command, Name), it auto-enters edit mode without pressing Enter. Any accidental keystrokes while navigating modify the field value with no undo.
**NNGroup violation:** Error Prevention — "Eliminate error-prone conditions or present users with a confirmation option."

#### H7 (NEW): Status bar is static — doesn't reflect current mode
**Tested (Round 2):** Edit Item footer always shows `Enter:Edit  Ctrl+S:Save  Esc:Cancel` regardless of mode.
- In text editing mode: no change (should show "Esc:Stop editing")
- In expanded sub-section: no change
- In dropdown: no change
The only edit-mode indicators are subtle: cursor inverse video (`[7m]`) and label color change (white→blue `38;5;25`).
**NNGroup violation:** Visibility of System Status — "The design should always keep users informed about what is going on."

### Medium Priority Bugs

#### M2: Numeric spinner typed input leaks to adjacent fields
**Status:** Not re-tested in Round 2. Likely still present.

#### M5: No position indicator in long lists
**Tested (Round 2):** Scrollbar IS present (thumb: `48;5;3` yellow, track: `48;5;15` white in main panel; thumb: `38;5;70` green, track: `38;5;58` olive in dialogs). However there is no numeric position indicator ("X of Y items").
**NNGroup violation:** Recognition Rather Than Recall — Users can't see total items or their position.

#### M6: Crash on very small terminal
**Status:** Not re-tested in Round 2. Likely still present.

#### M7 (NEW): No Page Up/Down or Home/End in long lists
**Tested (Round 2):** LSP list has 40+ entries. Only Up/Down one-at-a-time navigation. No Page Up/Down for jumping, no Home/End for first/last.
**NNGroup violation:** Flexibility and Efficiency of Use — Power users need accelerators.

#### M8 (NEW): Section header scrolls away, losing context
**Tested (Round 2):** When scrolling deep into the LSP list, the `> Lsp:` header scrolls off-screen. User loses context about which section they're in.

#### M9 (NEW): No confirmation when discarding changes via Esc
**Tested (Round 2):** Modified Command field, pressed Esc. Dialog closed immediately, changes discarded. No "Discard changes?" confirmation.
**NNGroup violation:** Error Prevention — Destructive actions should require confirmation.

#### M10 (NEW): No search/filter within map lists
**Tested (Round 2):** While `/` searches settings sections, there is no way to filter within a long map list (e.g., find "python" within 40+ LSP entries).

### Low Priority Issues

#### L1: Complex types rendered as raw JSON
Process Limits shows `{ "max_memory_percent": 50, ... }` as raw JSON textarea. Except/Only Features show `null`. Should have structured controls.

#### L2: All fields shown flat, no collapsible sections
Add Item form shows 12 fields alphabetically with no grouping. Most users only need Command, Args, Enabled, Name.

#### L3 (UPDATED): Left panel uses different highlight style
Left sidebar: blue background (`48;5;25`) with white text, no `>` arrow. Right panel: dark gray background (`48;5;16`) with `>` arrow and bold white. Intentional differentiation but mildly inconsistent.

#### L4: No command validation on save
Can enter nonexistent commands with no feedback.

---

## What Works Well

- **Text input rendering:** Characters appear immediately in all text fields (C1 fix confirmed)
- **Ctrl+S save shortcut:** Works from any field in entry dialogs, including during text editing
- **Focus indicators:** `>` prefix with `>●` markers. Consistent `48;5;16` dark bg + `38;5;231` bright white across contexts
- **Button focus:** Bold+reverse video (`[1;7m]`) with `>` arrow prefix. Delete button uses red (`38;5;160`)
- **Esc cascade:** Clean unwinding through all nesting levels with correct focus restoration
- **Progressive parent dimming:** Each nesting level dims parent content darker. Clear visual hierarchy
- **Focus return:** After closing child dialog, focus returns to exact spawning element
- **LSP display field:** Shows command names (pylsp, rust-analyzer) with proper truncation
- **Scrollbar:** Present in both main panel and dialogs, moves with viewport position
- **Checkbox toggle:** Immediate visual feedback, arrow keys still navigate away
- **JSON code block editing:** Process Limits editor works correctly, Esc properly exits
- **Responsive layout:** Sidebar→tab bar adaptation at smaller terminal sizes
- **Column alignment:** Map entries use fixed-width key column for clean alignment
- **[Enter to edit] hint:** Appears inline with focused items in map views, `DIM` styled

---

## Reproduction Steps

Each bug below includes full steps to reproduce from a clean state:

```bash
# Prerequisites (run once)
cargo build
mkdir -p /tmp/fresh-test && echo 'print("hello")' > /tmp/fresh-test/test.py

# Launch fresh in tmux (reusable for all tests)
tmux kill-session -t fresh 2>/dev/null
tmux new-session -d -s fresh -x 160 -y 50 \
  "TERM=xterm-256color ./target/debug/fresh /tmp/fresh-test/test.py"
sleep 2

# Helper: navigate to Settings > LSP > first language entry > Edit Item dialog
navigate_to_lsp_edit_item() {
  tmux send-keys -t fresh C-p && sleep 0.5
  tmux send-keys -t fresh "Open Settings" && sleep 0.5
  tmux send-keys -t fresh Enter && sleep 1
  tmux send-keys -t fresh "/" && sleep 0.3
  tmux send-keys -t fresh "lsp" && sleep 0.3
  tmux send-keys -t fresh Enter && sleep 0.5
  tmux send-keys -t fresh Enter && sleep 0.5
  tmux send-keys -t fresh Down && sleep 0.2
  tmux send-keys -t fresh Enter && sleep 0.5
}

# Capture helper (with ANSI colors)
cap() { tmux capture-pane -t fresh -p -e; }
```

### C2: Text field traps keyboard (cannot exit edit mode)

```bash
navigate_to_lsp_edit_item
# Focus should land on Command field (first editable)
# Press Enter to edit (or field auto-enters edit mode)
tmux send-keys -t fresh Enter && sleep 0.3
tmux send-keys -t fresh "test" && sleep 0.3
# Verify text renders (should show "astro-lstest" or similar)
cap | grep "test"
# Now try to exit edit mode
tmux send-keys -t fresh Escape && sleep 0.3
cap  # Check: is cursor still active? Is label still blue?
# Try Down arrow
tmux send-keys -t fresh Down && sleep 0.3
cap  # Check: did focus move to next field?
# EXPECTED: Esc exits edit mode, Down navigates to next field
# ACTUAL: Nothing works. Only Ctrl+S exits (by saving and closing)
```

### C3: Enter on array item closes Edit Value dialog

```bash
# Navigate to Level 2 only (Edit Value)
tmux kill-session -t fresh 2>/dev/null
tmux new-session -d -s fresh -x 160 -y 50 \
  "TERM=xterm-256color ./target/debug/fresh /tmp/fresh-test/test.py"
sleep 2
tmux send-keys -t fresh C-p && sleep 0.5
tmux send-keys -t fresh "Open Settings" && sleep 0.5
tmux send-keys -t fresh Enter && sleep 1
tmux send-keys -t fresh "/" && sleep 0.3
tmux send-keys -t fresh "lsp" && sleep 0.3
tmux send-keys -t fresh Enter && sleep 0.5
tmux send-keys -t fresh Enter && sleep 0.5
# Now in Level 2. Navigate to array item
tmux send-keys -t fresh Down && sleep 0.2
cap | grep ">"  # should show "-> server [x]" focused
tmux send-keys -t fresh Enter && sleep 0.5
cap
# EXPECTED: Edit Item dialog opens for the server
# ACTUAL: Dialog closes, back at Settings Level 1
```

### H1: Tab only toggles to Save

```bash
navigate_to_lsp_edit_item
for i in $(seq 1 6); do
  tmux send-keys -t fresh Tab && sleep 0.2
  echo "=== Tab $i ===" && cap | grep -E "> \[|>●"
done
# EXPECTED: Tab reaches Save, Delete, Cancel in sequence
# ACTUAL: Tab toggles between current field and Save only
```

### H2: [+] Add new unreachable in Level 3

```bash
navigate_to_lsp_edit_item
# Navigate through all fields with Down
for i in $(seq 1 20); do
  tmux send-keys -t fresh Down && sleep 0.1
done
cap | grep "> .*Add new"
# EXPECTED: Focus lands on [+] Add new at some point
# ACTUAL: Focus never reaches any [+] Add new button
```

---

## Implementation Plan

### Phase 1: Critical Fix (C2)

**C2 fix (text field keyboard trap):** The text field edit mode captures all input including Escape. Need to make Escape exit edit mode by calling `dialog.stop_editing()` and returning `InputResult::Consumed` before the navigation handler sees the event. Check `input.rs` `handle_entry_dialog_text_editing()` — the Esc branch calls `dialog.stop_editing()` but either:
1. The `editing_text` flag is not being checked correctly (field auto-enters edit mode so the flag may be wrong)
2. The `stop_editing()` call is not clearing the edit state properly
3. The event is still propagating after being consumed

Also investigate why text fields auto-enter edit mode on navigation (H6) — this may be the root cause. If fields don't auto-enter edit mode, Escape handling becomes simpler.

### Phase 2: Entry Dialog Navigation (C3, H1, H2)

**C3 fix:** In `state.rs`, Enter on an ObjectArray item within Edit Value should open a nested Edit Item dialog, not trigger save/close.

**H1 fix:** Differentiate Tab from Down in `input.rs`. Tab should toggle between fields region and buttons region (cycling through all buttons). Down should navigate sequentially.

**H2 fix:** Include composite control sub-items ([+] Add new, individual list items) in the `focus_next()`/`focus_prev()` navigation order within `entry_dialog.rs`.

### Phase 3: Status & Feedback (H5, H6, H7, M9)

**H5 fix:** Make TextList items individually focusable with Delete key support.

**H6 fix:** Don't auto-enter edit mode on text fields when navigating. Require explicit Enter/Space to start editing.

**H7 fix:** Update status bar text dynamically based on current mode:
- Normal: `↑↓:Navigate  Tab:Fields/Buttons  Enter:Edit  Ctrl+S:Save  Esc:Cancel`
- Text editing: `Type to edit  Esc:Stop editing  Ctrl+S:Save`
- Dropdown open: `↑↓:Select  Enter:Confirm  Esc:Cancel`

**M9 fix:** Show confirmation dialog when Esc would discard modifications.

### Phase 4: Efficiency (M2, M5, M7, M8, M10)

**M2:** Fix spinner input routing.
**M5:** Add "X of Y" position indicator near scrollbar.
**M7:** Add Page Up/Down (or Ctrl+U/D) and Home/End support.
**M8:** Pin section header while scrolling.
**M10:** Add inline filter/search for map lists.

### Phase 5: Polish (L1-L4)

**L1:** Structured controls for ProcessLimits, LspFeature enums.
**L2:** Collapsible "Advanced" sections in entry dialogs.
**L3-L4:** Cosmetic consistency fixes.

---

## Verification Checklist

After each phase, re-test with tmux using `capture-pane -p -e`:

### Phase 1
- [ ] **C2:** Escape exits text edit mode, returns to field navigation
- [ ] **C2:** Down/Up arrows work after exiting text edit mode
- [ ] **C2:** Tab works after exiting text edit mode

### Phase 2
- [ ] **C3:** Enter on array item opens Edit Item dialog
- [ ] **H1:** Tab cycles through all buttons (Save, Delete, Cancel)
- [ ] **H1:** Shift+Tab reverses through buttons
- [ ] **H2:** [+] Add new reachable via keyboard in Args, Env, Root Markers

### Phase 3
- [ ] **H5:** Individual TextList items focusable, deletable via Delete key
- [ ] **H6:** Text fields don't auto-enter edit mode on navigation
- [ ] **H7:** Status bar updates to reflect text editing / dropdown modes
- [ ] **M9:** Esc shows confirmation when changes would be lost

### Phase 4
- [ ] **M5:** Position indicator visible (e.g., "15 of 42")
- [ ] **M7:** Page Up/Down jumps through long lists
- [ ] **M8:** Section header stays visible while scrolling

### Phase 5
- [ ] **L1:** ProcessLimits has structured number/checkbox controls
- [ ] **L2:** Advanced section is collapsible in Add Item
