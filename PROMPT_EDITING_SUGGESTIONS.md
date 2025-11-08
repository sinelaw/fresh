# Suggestions: Advanced Editing Actions in Prompts

## Problem Statement

Currently, basic editing actions like copy/paste/cut and word-based navigation/deletion work in normal editing mode but not in prompt mode (command palette, git grep, open file, etc.). Users expect these actions to work consistently across all text input contexts.

## Current Architecture

### What Works
- **Prompt mode**: Basic operations (insert char, backspace, delete, left/right, home/end)
- **Normal mode**: Full editing suite (copy/paste, word operations, selections)

### Key Components
1. **`src/prompt.rs`**: Prompt struct with basic string manipulation
2. **`src/word_navigation.rs`**: Word boundary detection (works on Buffer, not strings)
3. **`src/keybindings.rs`**: Context-specific keybindings (Normal vs Prompt)
4. **`src/editor.rs`**: Action handlers

### The Gap
- Prompt context doesn't have keybindings for Ctrl+Backspace, Ctrl+Delete, Ctrl+C/X/V, etc.
- Prompt struct lacks methods for word operations and clipboard
- Word navigation functions work on Buffer, not plain strings

## Suggested Solutions (Without Code Duplication)

### ✅ **Recommended: Option 1 - String-Based Word Navigation + Prompt Methods**

This approach adds minimal code and reuses existing logic where possible.

#### Step 1: Add string-based word navigation helpers

In `src/word_navigation.rs`, add lightweight helpers for strings:

```rust
/// Find word start in a string at byte position
pub fn find_word_start_in_str(s: &str, pos: usize) -> usize {
    let bytes = s.as_bytes();
    let pos = pos.min(bytes.len());
    let mut new_pos = pos;

    // Move to current/previous word character
    if new_pos >= bytes.len() || !is_word_char(bytes[new_pos]) {
        if new_pos > 0 {
            new_pos -= 1;
        }
    }

    // Scan backward to word start
    while new_pos > 0 && is_word_char(bytes[new_pos.saturating_sub(1)]) {
        new_pos -= 1;
    }

    new_pos
}

/// Find word end in a string at byte position
pub fn find_word_end_in_str(s: &str, pos: usize) -> usize {
    let bytes = s.as_bytes();
    let pos = pos.min(bytes.len());
    let mut new_pos = pos;

    // Scan forward to word end
    while new_pos < bytes.len() && is_word_char(bytes[new_pos]) {
        new_pos += 1;
    }

    new_pos
}
```

#### Step 2: Add word operations to Prompt

In `src/prompt.rs`, add methods:

```rust
impl Prompt {
    /// Delete from cursor to end of word (Ctrl+Delete)
    pub fn delete_word_forward(&mut self) {
        let word_end = find_word_end_in_str(&self.input, self.cursor_pos);
        if word_end > self.cursor_pos {
            self.input.drain(self.cursor_pos..word_end);
        }
    }

    /// Delete from start of word to cursor (Ctrl+Backspace)
    pub fn delete_word_backward(&mut self) {
        let word_start = find_word_start_in_str(&self.input, self.cursor_pos);
        if word_start < self.cursor_pos {
            self.input.drain(word_start..self.cursor_pos);
            self.cursor_pos = word_start;
        }
    }

    /// Get selected text (for copy/cut) - full input for now
    /// In future, could support selection ranges
    pub fn get_text(&self) -> String {
        self.input.clone()
    }

    /// Clear input (for cut)
    pub fn clear(&mut self) {
        self.input.clear();
        self.cursor_pos = 0;
    }

    /// Set input (for paste)
    pub fn set_input(&mut self, text: String) {
        self.input = text;
        self.cursor_pos = self.input.len();
    }

    /// Insert text at cursor (for paste)
    pub fn insert_str(&mut self, text: &str) {
        self.input.insert_str(self.cursor_pos, text);
        self.cursor_pos += text.len();
    }
}
```

#### Step 3: Add prompt-specific actions to Action enum

In `src/keybindings.rs`, add to the `Action` enum:

```rust
pub enum Action {
    // ... existing actions ...

    // Prompt editing actions
    PromptDeleteWordForward,
    PromptDeleteWordBackward,
    PromptCopy,
    PromptCut,
    PromptPaste,
}
```

#### Step 4: Add keybindings for prompt context

In `src/keybindings.rs`, in the `default_bindings()` function:

```rust
// Prompt context bindings (around line 780)
let mut prompt_bindings = HashMap::new();
// ... existing bindings ...

// Word operations
prompt_bindings.insert(
    (KeyCode::Backspace, KeyModifiers::CONTROL),
    Action::PromptDeleteWordBackward,
);
prompt_bindings.insert(
    (KeyCode::Delete, KeyModifiers::CONTROL),
    Action::PromptDeleteWordForward,
);

// Clipboard operations
prompt_bindings.insert(
    (KeyCode::Char('c'), KeyModifiers::CONTROL),
    Action::PromptCopy,
);
prompt_bindings.insert(
    (KeyCode::Char('x'), KeyModifiers::CONTROL),
    Action::PromptCut,
);
prompt_bindings.insert(
    (KeyCode::Char('v'), KeyModifiers::CONTROL),
    Action::PromptPaste,
);
```

#### Step 5: Handle actions in Editor

In `src/editor.rs`, add to `handle_action()`:

```rust
Action::PromptDeleteWordForward => {
    if let Some(prompt) = self.prompt_mut() {
        prompt.delete_word_forward();
    }
    self.update_prompt_suggestions();
}
Action::PromptDeleteWordBackward => {
    if let Some(prompt) = self.prompt_mut() {
        prompt.delete_word_backward();
    }
    self.update_prompt_suggestions();
}
Action::PromptCopy => {
    if let Some(prompt) = &self.prompt {
        self.clipboard = prompt.get_text();
        self.set_status_message("Copied".to_string());
    }
}
Action::PromptCut => {
    if let Some(prompt) = self.prompt_mut() {
        self.clipboard = prompt.get_text();
        prompt.clear();
        self.set_status_message("Cut".to_string());
    }
    self.update_prompt_suggestions();
}
Action::PromptPaste => {
    if let Some(prompt) = self.prompt_mut() {
        let text = self.clipboard.clone();
        prompt.insert_str(&text);
    }
    self.update_prompt_suggestions();
}
```

### Alternative Options Considered

#### ❌ **Option 2: Unified TextInput Abstraction**

**Pros**: Most elegant, fully eliminates duplication
**Cons**: Requires significant refactoring, higher risk of breaking changes

Create a `TextInput` trait/struct that both `Prompt` and `Buffer` can use. However, this would require:
- Refactoring existing prompt code
- Abstracting differences between single-line and multi-line
- Risk of over-engineering for limited benefit

#### ❌ **Option 3: Reuse Buffer Events for Prompt**

**Pros**: Maximum code reuse
**Cons**: Overkill for simple single-line input, performance overhead

Create a temporary Buffer for each prompt and use the existing event system. Issues:
- Prompts are simple strings, using Buffer adds complexity
- Event sourcing overhead (undo/redo) not needed for prompts
- Would still need separate keybindings

## Implementation Priority

1. **Phase 1** (High Value): Word deletion (Ctrl+Backspace/Delete)
   - Most commonly missed feature
   - Minimal code addition

2. **Phase 2** (Medium Value): Copy/Paste
   - Useful for reusing previous inputs
   - Requires clipboard integration

3. **Phase 3** (Future): Selection support
   - Shift+Left/Right to select
   - More complex, requires selection state in Prompt

## Testing Strategy

1. **Unit tests**: Add to `src/prompt.rs` testing new methods
2. **E2E tests**: Add to `tests/e2e/prompt_editing.rs` (see example below)
3. **Test coverage**:
   - Word deletion at word boundaries
   - Word deletion in middle of word
   - Copy/paste entire input
   - All operations update suggestions correctly

## Benefits of Recommended Approach

✅ **Minimal code duplication**: String helpers are tiny, single-purpose
✅ **Clear separation**: Prompt remains simple, doesn't need Buffer complexity
✅ **Easy to test**: Each method has clear inputs/outputs
✅ **Low risk**: Existing code unchanged, only additions
✅ **Extensible**: Easy to add more operations later (selection, etc.)

## Alternative Consideration: Selection Support

For full copy/paste parity with normal editing, prompts would need:
- Selection state (anchor position)
- Visual indication of selection
- Shift+Left/Right keybindings

This could be Phase 3, adding:
```rust
pub struct Prompt {
    // ... existing fields ...
    /// Selection anchor (None = no selection)
    pub selection_anchor: Option<usize>,
}
```

However, this adds complexity and may not be needed for 80% of use cases.

## Summary

**Recommended**: Option 1 (String-based helpers + Prompt methods)
- Adds ~100 lines of code total
- No refactoring of existing code
- Solves the immediate user need
- Easy to extend in future
