# Settings UI Improvement Plan

## Overview

This plan focuses on systematically **UX testing** the Settings UI dialogs against the UI design principles, identifying gaps, and then fixing them. The approach is: test first to map the full gap landscape, then implement fixes in priority order.

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

## Known Issues (Reported Bugs)

These specific issues were reported and serve as starting points for investigation, but the UX testing should not be limited to only these:

### Bug 1: Text input broken in Add Item dialog
Navigate to Add Item dialog > Command field > press Enter to edit > type text. **Expected:** text appears. **Actual:** field stays empty.

### Bug 2: Tab key behavior contradicts status bar
Status bar shows "Tab:Fields/Buttons" implying toggle between regions, but Tab navigates sequentially through every field. 11+ fields makes reaching Save tedious.

### Bug 3: No visible button focus indicator
After tabbing to buttons, both `[ Save ]` and `[ Cancel ]` look identical — no highlight on the focused one.

### Bug 4: Enter on boolean fields fires instead of saving
When tabbing through fields to reach Save, Enter accidentally toggles boolean checkboxes (Enabled, Auto Start).

### Issue 5: Array items show `[1 items]` without command preview
LSP entries show `python  [1 items]` instead of `python  pylsp`. The `x-display-field: "/command"` doesn't propagate through the array wrapper.

### Issue 6: Too many fields in Add Item form
11+ fields (Args, Auto Start, Command, Enabled, Env, Except Features, Initialization Options, Language Id Overrides, Name, Only Features, Process Limits, Root Markers). Most users only need Command, Args, Enabled.

### Issue 7: Complex types rendered as raw JSON
Process Limits shows `{ "max_memory_percent": 50, ... }`. Features show `null`. Should have structured controls.

### Bug 8: No keyboard shortcut to save from entry dialog
No Ctrl+S or Ctrl+Enter works in entry dialog. Must Tab through all fields to reach Save.

### Issue 9: No autocomplete or validation for Command field
Can enter nonexistent commands with no feedback.

---

## Part 1: UX Testing Plan — Audit Against UI Principles

The UI principles define specific behavioral requirements. Each test below maps a principle to the current behavior in the Settings dialog and its sub-dialogs (entry dialogs, add item forms, nested dialogs).

### Test Infrastructure

```bash
# Build and launch
cargo build
mkdir -p /tmp/fresh-test && echo 'print("hello")' > /tmp/fresh-test/test.py
tmux new-session -d -s fresh -x 160 -y 50 \
  "TERM=xterm-256color ./target/debug/fresh /tmp/fresh-test/test.py"

# Helper: capture and inspect
alias cap="tmux capture-pane -t fresh -p"

# Cleanup
tmux kill-session -t fresh
```

---

### Test Category A: Focus Management ("Where Am I?" Rule)

**Principle:** Focus must always be visually undeniable — via color inversion or `> [ ] <` brackets.

#### Test A1: Main settings panel focus indicator
```bash
# Open settings, navigate items
tmux send-keys -t fresh C-p && sleep 0.5
tmux send-keys -t fresh "Open Settings" && sleep 0.3
tmux send-keys -t fresh Enter && sleep 1
# Move through items with Down
for i in $(seq 1 5); do tmux send-keys -t fresh Down; sleep 0.2; done
cap | head -30
```
**Check:** Is the focused item clearly distinguishable? Does it use `>` prefix, color inversion, or highlight background?

#### Test A2: Entry dialog field focus indicator
```bash
# Navigate to LSP, open an entry
tmux send-keys -t fresh "/" && sleep 0.3
tmux send-keys -t fresh "lsp" && sleep 0.3
tmux send-keys -t fresh Enter && sleep 0.5
tmux send-keys -t fresh Enter && sleep 0.5  # open first LSP entry
# Navigate fields
for i in $(seq 1 4); do tmux send-keys -t fresh Down; sleep 0.2; done
cap
```
**Check:** Is each focused field clearly marked? Can user always tell which field has focus?

#### Test A3: Button focus indicator (Bug 3)
```bash
# From entry dialog, Tab to buttons
for i in $(seq 1 20); do tmux send-keys -t fresh Down; sleep 0.1; done
cap | grep -E "Save|Cancel|Delete"
```
**Check:** When focus is on a button, is it visually distinct from unfocused buttons? Expected: `>` prefix and/or REVERSED style. Gap: Buttons may look identical regardless of focus.

#### Test A4: Focus indicator in nested sub-dialogs
```bash
# From an entry dialog, navigate to a Map or ObjectArray field, open it
# (e.g., Env field, or Language Id Overrides)
# Press Enter to open nested dialog
tmux send-keys -t fresh Enter && sleep 0.5
cap
```
**Check:** Does the nested dialog maintain clear focus indication? Does the parent dialog visually dim?

**Principle gap:** The spec says "The child dialog steals focus completely. The parent dialog visually dims or loses its active border color." Verify this happens.

---

### Test Category B: Tab/Focus Cycle Rules

**Principle:** Tab moves forward, Shift+Tab backward. Reaching last element wraps to first. Reading order: Left-to-Right, Top-to-Bottom, ending at footer actions.

#### Test B1: Tab behavior in entry dialog (Bug 2)
```bash
# From Add Item dialog, record focus position after each Tab
for i in $(seq 1 15); do
  tmux send-keys -t fresh Tab; sleep 0.2
  echo "=== Tab $i ===" && cap | grep ">"
done
```
**Check:** Does Tab navigate sequentially through every field? The status bar says "Tab:Fields/Buttons" implying it should toggle between regions. Gap: Tab is sequential (same as Down).

#### Test B2: Shift+Tab reverse navigation
```bash
# From Add Item dialog buttons, Shift+Tab back
for i in $(seq 1 5); do
  tmux send-keys -t fresh BTab; sleep 0.2
  echo "=== ShiftTab $i ===" && cap | grep ">"
done
```
**Check:** Does Shift+Tab navigate backward correctly? Does it wrap from first item to last button?

#### Test B3: Focus wrap-around (strict loop)
```bash
# Tab past last button — should wrap to first field
# ShiftTab from first field — should wrap to last button
```
**Check:** Is there a strict loop? Principle requires: last element + Tab = first element.

#### Test B4: Disabled/read-only controls skip behavior
```bash
# In Edit dialog (existing entry), Key field is read-only
# Tab through — does it skip the read-only Key field?
cap | grep -E "Key|read"
```
**Check:** Principle says disabled controls "MUST BE COMPLETELY SKIPPED by Tab and Shift+Tab." Currently read-only items are sorted to the top and `first_editable_index` skips them — verify this works.

---

### Test Category C: Composite Control Navigation

**Principle:** Tab moves focus TO the composite control, Tab again moves PAST it. Internal navigation uses arrows.

#### Test C1: ObjectArray (list of items) internal navigation
```bash
# Navigate to an ObjectArray field (e.g., Args in LSP entry)
# Tab to it, then use Up/Down within it
tmux send-keys -t fresh Down && sleep 0.2
tmux send-keys -t fresh Down && sleep 0.2
cap
```
**Check:** Can user navigate within the list with arrows? Does Tab exit the list to the next field?

#### Test C2: TextList internal navigation
```bash
# Navigate to Args (TextList), press Enter to edit, add items
tmux send-keys -t fresh Enter && sleep 0.3
tmux send-keys -t fresh "arg1" && sleep 0.3
tmux send-keys -t fresh Enter && sleep 0.3  # should add item
cap
```
**Check:** Enter inside TextList should add items. Tab should exit editing mode.

#### Test C3: Map control internal navigation
```bash
# Navigate to Env (HashMap) in entry dialog
# Verify expand/collapse with Enter, internal navigation with arrows
```
**Check:** Arrow keys navigate within map entries. Tab exits the map control.

---

### Test Category D: Text Input & Edit Mode

**Principle:** Enter on a text field starts editing. In edit mode, Enter inserts newline (for multi-line) or submits. Tab/Esc exits edit mode.

#### Test D1: Text input activation and typing (Bug 1)
```bash
# Navigate to Command field, press Enter to edit, type text
tmux send-keys -t fresh Enter && sleep 0.3
tmux send-keys -t fresh "test-server" && sleep 0.5
cap | grep "test-server"
```
**Check:** Text should appear in the field. Gap: Field remains empty — keystrokes go elsewhere. This is Bug 1.

#### Test D2: JSON editor activation
```bash
# Navigate to Initialization Options (JSON field), press Enter
tmux send-keys -t fresh Enter && sleep 0.3
tmux send-keys -t fresh '{"foo": 1}' && sleep 0.5
cap
```
**Check:** JSON editor should show typed content. Enter should insert newlines. Tab/Esc should exit.

#### Test D3: Text editing exit behavior
```bash
# While editing a text field, press Tab — should exit editing
tmux send-keys -t fresh Tab && sleep 0.3
cap
```
**Check:** Tab exits editing mode. Esc also exits. Focus moves to next field.

---

### Test Category E: Global Hotkeys & Actions

**Principle:** Esc universally aborts current context. Ctrl+S should save from anywhere. Mnemonics optional.

#### Test E1: Esc behavior cascade
```bash
# Test Esc at each level:
# 1. In text editing mode → exits editing
# 2. In dropdown → closes dropdown
# 3. In entry dialog → closes dialog
# 4. In main settings → prompts if unsaved changes
```
**Check:** Esc follows the cascade correctly at each level.

#### Test E2: Ctrl+S from entry dialog (Bug 8)
```bash
# From any field in Add Item dialog
tmux send-keys -t fresh C-s && sleep 0.5
cap
```
**Check:** Should save and close the entry dialog. Gap: Ctrl+S is only handled at the main settings level, not inside entry dialogs.

#### Test E3: Ctrl+Enter save (existing but unverified)
```bash
# input.rs:365-367 has Ctrl+Enter saving in entry dialog navigation
# Verify it works in terminal
tmux send-keys -t fresh C-Enter && sleep 0.5
cap
```
**Check:** Some terminals may not pass Ctrl+Enter. Verify actual terminal behavior.

---

### Test Category F: Higher-Order Organization

**Principle:** Forms exceeding a single screen should use Tabs, Collapsible Sections, or Wizards.

#### Test F1: Add Item form field count (Issue 6)
```bash
# Open Add Item for LSP entry, count visible fields
cap | grep -c ">"  # rough field count
cap  # inspect all fields
```
**Check:** How many fields are visible? Are they all needed? Gap: 11+ fields shown (Args, Auto Start, Command, Enabled, Env, Except Features, Initialization Options, Language Id Overrides, Name, Only Features, Process Limits, Root Markers). Principle says to use collapsible sections for optional/advanced fields.

#### Test F2: Scrolling for long forms
```bash
# Verify the form scrolls properly when it exceeds viewport
# Check scroll indicators
cap | grep -E "▲|▼|scroll"
```
**Check:** Are scroll indicators visible? Does scrolling follow focused item?

---

### Test Category G: Visual Hierarchy & Information Density

**Principle:** Complex types should have structured controls, not raw JSON. Display fields should show meaningful previews.

#### Test G1: LSP entry preview display (Issue 5)
```bash
# In main settings, LSP section
cap | grep -E "python|rust|items"
```
**Check:** Each language should show command name (e.g., `python  pylsp`). Gap: Shows `python  [1 items]` because the display_field `/command` doesn't propagate through array wrappers.

#### Test G2: Complex type rendering (Issue 7)
```bash
# In Add Item dialog, check Process Limits display
cap | grep -A2 "Process Limits"
# Check Except Features display
cap | grep -A2 "Except Features"
```
**Check:** Process Limits shows raw JSON `{"max_memory_percent": 50, ...}` instead of structured fields. Features show `null`.

---

### Test Category H: Dialog Mechanics

**Principle:** Dialogs completely isolate background state. Visual hierarchy with borders and padding.

#### Test H1: Modal isolation
```bash
# With entry dialog open, try clicking/typing outside it
# Verify no background state changes
```
**Check:** Entry dialog should consume all input. Background should not change.

#### Test H2: Terminal resize handling
```bash
# Resize terminal while dialog is open
tmux resize-window -t fresh -x 80 -y 25
sleep 0.5
cap
```
**Check:** Dialog should resize gracefully. Principle: if terminal too small, show `[Terminal too small]` warning.

#### Test H3: Nested dialog visual hierarchy
```bash
# Open entry dialog, then open nested dialog (e.g., from ObjectArray or Map field)
cap
```
**Check:** Parent dialog should dim. Child dialog should have clear borders and focus trap.

---

## Part 2: Gap Summary (Expected Findings)

Based on code analysis, the expected gaps between current behavior and UI principles:

| # | Principle | Current Behavior | Gap Severity |
|---|-----------|-----------------|--------------|
| 1 | Text input must work in edit mode | Keystrokes lost in Add Item dialog | **Critical** |
| 2 | Tab toggles between regions (per status bar hint) | Tab navigates sequentially like Down | **High** |
| 3 | Focus must be visually undeniable | Buttons lack visible focus indicator (or hard to reach) | **High** |
| 4 | Ctrl+S saves from anywhere | Only works at main settings level, not in entry dialogs | **Medium** |
| 5 | Display fields show meaningful previews | Array values show `[1 items]` instead of first element's field | **Medium** |
| 6 | Collapsible sections for long forms | All 11+ fields shown flat, no grouping | **Medium** |
| 7 | Structured controls for complex types | Raw JSON for objects, `null` for optional arrays | **Low** |
| 8 | Parent dialog dims when child opens | Not verified — may or may not work | **Low** |
| 9 | Disabled controls skipped by Tab | Read-only items sorted first with `first_editable_index` — likely works | **Verify** |

---

## Part 3: Implementation Plan (After Testing)

### Phase 1: Critical Bug Fixes
**Target: Bugs 1, 2, 3, 8**

1. **Bug 1 — Text input broken:** Investigate `start_editing()` flow in `entry_dialog.rs`. Likely issue: `editing_text` flag not set, or `KeyCode::Char(' ')` at `input.rs:340` intercepting character input. Fix: ensure `start_editing()` sets the flag and text mode routes `Char` events to `insert_char()`.

2. **Bug 2 — Tab toggle:** Change `input.rs:310-314` so Tab toggles between fields/buttons regions. Keep Up/Down for sequential field navigation. Update help text at `render.rs:3051`.

3. **Bug 3 — Button focus:** Verify the existing `>` indicator and `REVERSED` style at `render.rs:2985-2998` render correctly. Fix layout math if `x += 2` pushes buttons off-screen. This may be a non-issue once Bug 2 is fixed (users can actually reach buttons).

4. **Bug 8 — Ctrl+S in entry dialog:** Add Ctrl+S handling at the top of `handle_entry_dialog_input()` in `input.rs`, before the editing_text/dropdown routing. Update help text.

### Phase 2: Display & Preview Fixes
**Target: Issue 5**

1. Fix `get_display_value()` in `controls/map_input/mod.rs` to handle array values by applying `display_field` to the first element. Show `command_name (+N)` for multi-element arrays.

### Phase 3: Organization & UX
**Target: Issues 6, 7**

1. **Collapsible sections:** Add `collapsed_sections: HashMap<String, bool>` to `EntryDialogState`. Use `SettingItem::section` field to group items. Render section headers as `[+]/[-] Section Name`. Skip collapsed items in focus navigation and rendering.

2. **Structured complex types:** Verify schema generation for ProcessLimits produces nested Object properties. If not, add schema-level fixes to break complex types into individual controls.

### Phase 4: Polish
**Target: Issue 9, dialog visual hierarchy**

1. Command validation on save (warning, not error)
2. Parent dialog dimming when nested dialog is open

---

## Part 4: Test Execution Strategy

Tests can be run in parallel using separate tmux sessions:

```bash
# Session 1: Focus management tests (A1-A4)
tmux new-session -d -s test-focus -x 160 -y 50 \
  "TERM=xterm-256color ./target/debug/fresh /tmp/fresh-test/test.py"

# Session 2: Tab/navigation tests (B1-B4)
tmux new-session -d -s test-nav -x 160 -y 50 \
  "TERM=xterm-256color ./target/debug/fresh /tmp/fresh-test/test.py"

# Session 3: Text input tests (D1-D3)
tmux new-session -d -s test-input -x 160 -y 50 \
  "TERM=xterm-256color ./target/debug/fresh /tmp/fresh-test/test.py"
```

Each session can be driven independently, testing different aspects simultaneously.

---

## Verification Checklist

After each fix, re-run the corresponding tests:

- [ ] **A1-A4:** Focus indicator visible at all levels (settings, entry dialog, buttons, nested)
- [ ] **B1-B4:** Tab toggles fields/buttons, Shift+Tab reverses, wrap-around works, read-only skipped
- [ ] **C1-C3:** Composite controls (ObjectArray, TextList, Map) have proper internal/external navigation
- [ ] **D1-D3:** Text input works in entry dialog, JSON editor works, Tab/Esc exits editing
- [ ] **E1-E3:** Esc cascades correctly, Ctrl+S saves from entry dialog, Ctrl+Enter verified
- [ ] **F1-F2:** Collapsible sections for advanced fields, scroll indicators present
- [ ] **G1-G2:** LSP entries show command preview, complex types have structured controls
- [ ] **H1-H3:** Modal isolation works, terminal resize handled, nested dialog hierarchy clear
