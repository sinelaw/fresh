# Settings UI Usability Report

Tested the Settings UI for basic form/webform usability: Tab cycling, Up/Down navigation, Enter/Escape for accepting/canceling, focus visibility on individual elements, and change persistence across all control types (toggles, dropdowns, numbers, text lists, maps).

## Critical Issues

### 1. Rulers TextList: Escape destroys saved data (DATA LOSS BUG)

The Rulers TextList in Editor > Display has a critical bug where entering and exiting editing mode **deletes previously saved items**.

**Steps to reproduce:**
1. Add "80" to Rulers TextList, save settings with Ctrl+S
2. Reopen settings, navigate to Rulers — "80" is visible
3. Press Enter on Rulers to enter editing mode — "80" still visible
4. Press Escape to exit editing mode — "80" **DISAPPEARS**

This is a data loss bug. The Custom Ignore Patterns TextList (File Explorer) does NOT have this problem — its items survive Enter/Escape cycling. The bug appears specific to the Rulers control, possibly related to the integer-typed nature of ruler values vs string-typed ignore patterns.

### 2. Shift+Tab (BackTab) does not work for backward panel navigation

Tab cycles forward through panels: Categories → Settings → Footer → Categories. However, **Shift+Tab (BackTab) is not handled** in `handle_categories_input` or `handle_settings_input`. Only the Footer handles BackTab (to move to the previous button). This means there is no way to navigate backward between the three main panels using Shift+Tab, which is a fundamental form navigation expectation.

**Expected:** Shift+Tab should cycle backward: Categories → Footer → Settings → Categories (reverse of Tab).

### 3. Footer Tab order skips Layer and Reset buttons

The footer has 5 buttons in order: `[Edit]` `[Layer/User]` `[Reset]` `[Save]` `[Cancel]`. However, `footer_button_index` always defaults to 2 (Save) when entering the footer. Tab cycles: **Save(2) → Cancel(3) → Edit(4) → wrap to Categories**. The **Layer(0) and Reset(1) buttons are never visited by Tab**.

To reach Layer and Reset, you must use Left arrow/BackTab from Save, navigating backward. This is asymmetric and violates the standard form convention that Tab should visit all focusable elements.

**Expected:** Tab should cycle through all 5 footer buttons, or at minimum the initial focus position should be Edit(4) so forward Tab visits all buttons before wrapping.

### 4. No Left arrow navigation from Settings back to Categories

Enter/Right on a category item navigates from Categories to the Settings panel. However, pressing Left in the Settings panel calls `handle_control_decrement()` (for number/dropdown controls) rather than navigating back to Categories. There is no symmetric keyboard navigation back.

The only way to return to Categories from the Settings panel is to Tab through Footer first (Settings → Tab → Footer → Tab → Tab → Tab → Categories), requiring 4+ key presses.

**Expected:** Left arrow (when not in an editing mode and not on a decrement-able control) should navigate back to the Categories panel, mirroring Right/Enter which enters the Settings panel.

### 5. Up/Down arrows do nothing in number editing mode

When a number input is in editing mode (after pressing Enter on a number field), Up/Down arrow keys are not handled — they fall through to the `_ => {}` catch-all and are silently consumed. In most OS number input controls, Up/Down increment/decrement the value while editing.

**Expected:** Up should increment and Down should decrement the value in number editing mode, consistent with standard OS number spinners (Windows, macOS, web `<input type="number">`).

## Moderate Issues

### 6. Entry dialog focus indicator inconsistency

When navigating within entry dialogs (e.g., editing a Language's configuration), items use a `>` prefix as the focus indicator. However, when Down is pressed past the last item, focus transitions to the entry dialog's footer buttons (Save/Delete/Cancel), which use REVERSED text styling instead of `>`. The `>` marker disappears entirely from the screen.

In a terminal with good color support this is somewhat visible via inverse video, but the sudden disappearance of the `>` indicator is confusing, especially in low-color environments or when colorblind.

**Expected:** Entry dialog buttons should use the same `>` prefix indicator as items for consistency.

### 7. Confirmation dialog not triggered after layer cycling

When the user cycles through settings layers (User → Project → Session → User) using the Layer button in the footer, pending modifications from the original layer appear to be discarded without warning. After cycling back and pressing Escape, the settings closed without showing the "Unsaved Changes" confirmation dialog, even though `●` modification indicators were still visible.

**Expected:** Pending changes should be preserved or the user should be warned before layer cycling discards them.

### 8. No wrapping in categories list

Up/Down navigation in the Categories sidebar stops at the first/last category and does not wrap around. Most well-designed list navigation wraps from the last item back to the first (and vice versa) to enable efficient circular navigation.

## Minor Issues

### 9. Search hint text partially hidden

The footer hint text changes contextually (e.g., "↑↓:Navigate  Tab:Next  Enter:Edit  /:Search  E..."), but the Layer button label sometimes causes truncation. The "E" at the end appears to be a clipped character from the footer layout.

### 10. No Home/End for category navigation

Home and End keys are not handled in the Categories panel for jumping to the first/last category.

## Save Persistence Test Results

Tested changing values, saving with Ctrl+S, closing and reopening settings to verify persistence:

| Control Type | Test Setting | Change | Persisted? |
|---|---|---|---|
| **Toggle** | Highlight Matching Brackets | `[x]` → `[ ]` | Yes - shows `[ ]` with `●` and "(user)" after reopen |
| **Dropdown** | Accept Suggestion On Enter | `on` → `smart` | Yes - shows `[smart ▼]` with `●` and "(user)" after reopen |
| **Number** | Quick Suggestions Delay Ms | `10` → `50` | Yes - shows `[ 50 ]` with `●` and "(user)" after reopen |
| **TextList** | Custom Ignore Patterns | Added `*.bak` | Yes - shows `[*.bak] [x]` after reopen |
| **TextList** | Rulers | Added `80` | **BUGGY** — Saved via Ctrl+S while in editing mode, value was persisted, but entering/exiting editing mode destroys the saved value |

## What Works Well

- **Toggle controls**: Enter and Space both toggle correctly, with immediate visual feedback (`[x]` / `[ ]`) and a `●` modification indicator.
- **Dropdown controls**: Open with Enter, navigate with Up/Down, confirm with Enter, cancel with Escape (restoring the original value). All work as expected.
- **Number controls**: Left/Right increment/decrement when not editing, Enter opens editing mode, Escape cancels (restores original), Enter confirms. Very clean.
- **TextList controls (string-typed)**: Typing, Enter to add items, Up/Down to navigate between items, Delete to remove items. Items persist correctly through save/reopen cycles.
- **Map/Entry dialog controls**: Opening nested entry dialogs for complex data (like language configurations), navigating between fields with Up/Down, Tab to switch between items and buttons. Well-structured.
- **Search**: `/` activates search, real-time filtering works, results show category paths, Enter jumps to result, Escape cancels. Clean implementation.
- **Confirmation dialog**: Shows changed values, Tab/Left/Right navigate between Save/Discard/Cancel buttons, clear keyboard hints. Well done.
- **Section headers**: Correctly skipped during Up/Down navigation — focus jumps directly between editable controls.
- **Ctrl+S**: Global save shortcut works from any panel, even while in editing mode.
- **Scrolling**: Settings panel scrolls to keep focused items visible as you navigate with Up/Down.
- **Layer cycling**: User/Project/Session toggle works correctly via footer button.
- **Modified indicators**: `●` markers and "(user)" suffixes clearly show user-overridden values.
