# LSP Completion Popup Behavior Issues

## Test Environment
- Editor: fresh (debug build)
- LSP: clangd 18.1.3
- Test file: C file with structs, functions, and various identifier types
- Compared against VS Code expected behavior

## Summary

The completion popup is **overly modal** - it silently consumes many keys that should either dismiss the popup and perform their normal action, or should not be treated as type-to-filter characters. The root causes are in `crates/fresh-editor/src/view/popup/input/completion.rs`:

1. **Line 49-54**: The `Char(c)` handler matches ALL characters (with empty or SHIFT modifiers), including non-word characters like `;`, `(`, `)`, `{`, `}`, `=`, `+`, `-`, `,`, space, etc. These should NOT be type-to-filter characters.

2. **Line 63 / Line 117**: The catch-all `_ => InputResult::Consumed` silently swallows all unhandled keys (Left/Right arrows, Delete, Ctrl+key combos, etc.) instead of closing the popup and forwarding the key to the normal editor handler.

---

## Issue 1: Left/Right arrow keys are swallowed (CRITICAL)

**Steps**: Type `calc`, wait for popup, press Left or Right arrow
**Expected (VS Code)**: Popup closes, cursor moves left/right
**Actual**: Key is silently consumed. Popup stays open, cursor doesn't move.
**Root cause**: Falls through to `_ => InputResult::Consumed` catch-all
**Code**: `completion.rs:63` and `completion.rs:117`

## Issue 2: Space is treated as type-to-filter (CRITICAL)

**Steps**: Type `calc`, wait for popup, press Space
**Expected (VS Code)**: Popup closes, space is inserted
**Actual**: Space is inserted via type-to-filter, popup stays open showing all items (because space is a word boundary, so the filtered prefix becomes empty)
**Root cause**: Space (`' '`) matches `Char(c) if event.modifiers.is_empty()`
**Code**: `completion.rs:49-54`

## Issue 3: Non-word characters treated as type-to-filter (CRITICAL)

All of these characters are handled as type-to-filter when they should dismiss the popup and be inserted normally:

| Character | Tested | Behavior |
|-----------|--------|----------|
| `;` | Yes | Inserted via type-to-filter, popup stays open |
| `(` | Yes | Inserted via type-to-filter, popup stays open |
| `)` | Expected same | Same pattern |
| `{` | Expected same | Same pattern |
| `}` | Yes | Inserted via type-to-filter, popup stays open |
| `=` | Yes | Inserted via type-to-filter, popup stays open |
| `+` | Yes | Inserted via type-to-filter, popup stays open |
| `-` | Expected same | Same pattern |
| `,` | Yes | Inserted via type-to-filter, popup stays open |
| `/` | Expected same | Same pattern |
| `*` | Expected same | Same pattern |
| `&` | Expected same | Same pattern |
| `<` | Expected same | Same pattern |
| `>` | Expected same | Same pattern |
| `!` | Expected same | Same pattern |
| `~` | Expected same | Same pattern |
| `#` | Expected same | Same pattern |

**Root cause**: `Char(c)` pattern matches ALL characters, not just word characters
**Code**: `completion.rs:49-54`

**Note on `(`**: In VS Code, typing `(` after a function completion accepts the completion AND inserts `(`. This is the "commit character" feature. For example, typing `calc` to see `calculate_sum`, then pressing `(` should result in `calculate_sum(`.

## Issue 4: Ctrl+key combinations are swallowed (HIGH)

**Steps**: Type `calc`, wait for popup, press Ctrl+P / Ctrl+S / Ctrl+F
**Expected (VS Code)**: Popup closes, the Ctrl+key action executes (open command palette / save / find)
**Actual**: Key is silently consumed. No action occurs.
**Tested**: Ctrl+P (command palette), Ctrl+S (save), Ctrl+F (find) - all swallowed
**Root cause**: Ctrl+key combos don't match `Char(c)` (requires empty/SHIFT modifiers), fall through to `_ => InputResult::Consumed`
**Code**: `completion.rs:63` / `completion.rs:117`

## Issue 5: Delete key is swallowed (MEDIUM)

**Steps**: Type `calc`, wait for popup, press Delete
**Expected**: Popup closes, character ahead of cursor is deleted (or at minimum, Delete should work like Backspace in reverse)
**Actual**: Key is silently consumed. Nothing happens.
**Root cause**: Falls through to catch-all
**Code**: `completion.rs:63` / `completion.rs:117`

## Issue 6: Shift+Tab is swallowed (LOW)

**Steps**: Type `calc`, wait for popup, press Shift+Tab
**Expected**: Either navigate to previous completion item, or close popup
**Actual**: Key is silently consumed. Nothing happens.
**Root cause**: `Tab if event.modifiers.is_empty()` only handles plain Tab. Shift+Tab falls through to catch-all.
**Code**: `completion.rs:37` (Tab handler guard) → falls to `completion.rs:63`

## Issue 7: Stale/late completion popup appears at wrong time (MEDIUM)

**Steps**: Type `int x = ` (with trailing space) - type the whole thing quickly
**Expected**: No completion popup after the space (cursor is at a non-word position)
**Actual**: A completion popup appears showing ALL items with no filter, even though the cursor is after a space/operator
**Root cause**: The completion request was triggered when typing a word character (like `x`), but the LSP response arrived after the user had already typed past that point. When the response arrives, `refilter_completion_popup` / the initial response handler doesn't check whether the cursor is still in a valid completion context.
**Code**: `lsp_requests.rs:32-170` (completion response handler) - should verify cursor is still at/within a word

## Issue 8: Undo while popup is open is impossible (MEDIUM)

**Steps**: Type `calc`, wait for popup, try to undo (Ctrl+Z or command palette undo)
**Expected (VS Code)**: Popup closes, last action is undone
**Actual**: Since Ctrl+Z is swallowed by the popup (Issue 4), undo is impossible while the popup is open. The user must first press Escape, then undo. This is extra friction.
**Note**: Using command palette is also blocked since Ctrl+P is swallowed (Issue 4).

---

## Interactions Tested (Complete List)

### With popup OPEN:

| # | Interaction | Status | Notes |
|---|-------------|--------|-------|
| 1 | Type alphabetic chars (a-z, A-Z) | OK | Type-to-filter works correctly |
| 2 | Type underscore `_` | OK | Correctly treated as word char for filtering |
| 3 | Type digits (0-9) | OK | Correctly treated as word char for filtering |
| 4 | Backspace | OK | Removes filter char, re-filters list |
| 5 | Backspace past trigger point | OK | Popup properly dismissed |
| 6 | Tab to accept | OK | Accepts selected completion |
| 7 | Enter to accept | OK | Accepts with default `On` config |
| 8 | Up/Down arrows | OK | Navigate completion list |
| 9 | PageUp/PageDown | OK | Scroll completion list |
| 10 | Home/End | OK | Jump to first/last item |
| 11 | Escape | OK | Closes popup |
| 12 | Ctrl+C | OK | Copies selected item |
| 13 | Left arrow | BUG | Swallowed (Issue 1) |
| 14 | Right arrow | BUG | Swallowed (Issue 1) |
| 15 | Space | BUG | Type-to-filter instead of dismiss (Issue 2) |
| 16 | Semicolon `;` | BUG | Type-to-filter instead of dismiss (Issue 3) |
| 17 | Open paren `(` | BUG | Type-to-filter instead of accept+insert (Issue 3) |
| 18 | Close paren `)` | BUG | Type-to-filter instead of dismiss (Issue 3) |
| 19 | Equals `=` | BUG | Type-to-filter instead of dismiss (Issue 3) |
| 20 | Plus `+` | BUG | Type-to-filter instead of dismiss (Issue 3) |
| 21 | Comma `,` | BUG | Type-to-filter instead of dismiss (Issue 3) |
| 22 | Close brace `}` | BUG | Type-to-filter instead of dismiss (Issue 3) |
| 23 | Ctrl+P (palette) | BUG | Swallowed (Issue 4) |
| 24 | Ctrl+S (save) | BUG | Swallowed (Issue 4) |
| 25 | Ctrl+F (find) | BUG | Swallowed (Issue 4) |
| 26 | Delete key | BUG | Swallowed (Issue 5) |
| 27 | Shift+Tab | BUG | Swallowed (Issue 6) |
| 28 | Filter to zero matches | OK | Popup properly dismissed |

### With popup CLOSED:

| # | Interaction | Status | Notes |
|---|-------------|--------|-------|
| 29 | Typing word chars | OK | Triggers quick suggestions after delay |
| 30 | Dot trigger (`p.`) | OK | Immediate completion for struct members |
| 31 | Escape then continue typing | OK | Normal editing resumes |
| 32 | Quick typing past trigger | BUG | Stale popup appears (Issue 7) |

---

## Recommended Fixes (Priority Order)

### Fix 1: Classify characters as word vs non-word in type-to-filter

In `completion.rs`, change the `Char(c)` handler to only type-to-filter for word characters:

```rust
KeyCode::Char(c) if (event.modifiers.is_empty() || event.modifiers == KeyModifiers::SHIFT)
    && (c.is_alphanumeric() || c == '_') =>
{
    ctx.defer(DeferredAction::PopupTypeChar(c));
    InputResult::Consumed
}
```

For non-word characters, close the popup and forward the key:
```rust
KeyCode::Char(_) => {
    ctx.defer(DeferredAction::ClosePopup);
    InputResult::Ignored  // Let normal input handling process this key
}
```

### Fix 2: Handle Left/Right arrows - close popup and move cursor

```rust
KeyCode::Left | KeyCode::Right => {
    ctx.defer(DeferredAction::ClosePopup);
    InputResult::Ignored  // Let normal input handling move the cursor
}
```

### Fix 3: Don't swallow Ctrl+key combos

Instead of the catch-all consuming everything, close popup and forward Ctrl combos:
```rust
// For Ctrl+key combinations (except Ctrl+C which is handled above):
_ if event.modifiers.contains(KeyModifiers::CONTROL) => {
    ctx.defer(DeferredAction::ClosePopup);
    InputResult::Ignored
}
```

### Fix 4: Handle Delete key

```rust
KeyCode::Delete => {
    ctx.defer(DeferredAction::ClosePopup);
    InputResult::Ignored
}
```

### Fix 5: Discard stale completions

In the completion response handler (`lsp_requests.rs`), check if the cursor is still at a valid completion position before showing the popup.

### Fix 6: Consider commit characters

Some non-word characters (like `(` for functions) should accept the completion AND insert the character, rather than just dismissing. This is VS Code's "commit character" feature.
