# Vi Mode for Fresh

Vi-style modal editing implemented as a TypeScript plugin with minimal core changes.

## Design Philosophy

1. **Plugin-based** - Vi logic lives in `plugins/vi_mode.ts`, not Rust
2. **Composable** - Operators (`d`, `c`, `y`) work with any motion
3. **Minimal core** - Rust provides atomic actions, plugin handles mode logic
4. **Community-friendly** - TypeScript changes don't require recompilation

## How It Works

### Atomic Actions (Preferred)

For common operator+motion combinations, we use **atomic Rust actions** that perform the entire operation in one step. This avoids async timing issues.

```typescript
const atomicOperatorActions = {
  d: {
    move_word_right: "delete_word_forward",
    move_word_left: "delete_word_backward",
    move_line_end: "delete_to_line_end",
    move_line_start: "delete_to_line_start",
  },
  y: {
    move_word_right: "yank_word_forward",
    move_word_left: "yank_word_backward",
    move_line_end: "yank_to_line_end",
    move_line_start: "yank_to_line_start",
  },
};
```

### Selection-Based Fallback

For motions without atomic actions, we use selection-based approach:

```typescript
function applyOperatorWithMotion(operator: string, motionAction: string) {
  editor.executeAction(selectAction);  // e.g., select_up, select_down
  editor.executeAction("cut");         // or "copy" for yank
}
```

This works because selections are synchronous within a single plugin action.

### Batch Actions API

For efficient count prefix support (e.g., `3dw`), we use `executeActions` which executes multiple actions in a single Rust call without roundtrips:

```typescript
// Execute "move_down" 5 times efficiently
editor.executeActions([{ action: "move_down", count: 5 }]);

// Execute multiple actions in sequence
editor.executeActions([
  { action: "delete_word_forward", count: 3 },
  { action: "move_right" }
]);
```

## Current Status

### Working

| Feature | Commands |
|---------|----------|
| Movement | `h` `j` `k` `l`, `w` `b` `e`, `0` `$`, `gg` `G`, `Ctrl-f` `Ctrl-b` |
| Count prefix | `3j`, `5w`, `3dw`, `2dd`, `10x` - works with motions, operators, and more |
| Operators | `d` `c` `y` + motions (`dw`, `cw`, etc.) |
| Line ops | `dd` `cc` `yy`, `D` `C` |
| Char ops | `x` `X` `s` |
| Find char | `f` `t` `F` `T`, `;` `,` |
| Visual mode | `v` (char), `V` (line) - select with motions, then `d`/`c`/`y` |
| Text objects | `iw` `aw` (word), `i"` `a"` `i'` `a'` (quotes), `i(` `a(` `i{` `a{` `i[` `a[` (brackets) |
| Insert | `i` `a` `I` `A` `o` `O` |
| Search | `/` `n` `N` |
| Other | `u` `Ctrl-r` (undo/redo), `p` `P` (paste), `%` (bracket match) |

### Not Implemented

| Feature | Priority | Notes |
|---------|----------|-------|
| Visual block | Medium | `Ctrl-v` - column/block selection |
| `.` repeat | Medium | Repeat last change - big productivity boost |
| `:` command mode | Medium | `:w`, `:q` - Fresh has `Ctrl+S`, `Ctrl+Q` already |
| Registers | Low | `"a`, `"b`, named registers |
| Macros | Low | `q`, `@` |

## Files

| File | Purpose |
|------|---------|
| `plugins/vi_mode.ts` | Plugin implementation (~900 lines) |
| `src/input/buffer_mode.rs` | Plugin mode key handling |
| `src/services/plugins/runtime.rs` | Core APIs for plugins |
| `tests/e2e/vi_mode.rs` | E2E tests |

## Usage

1. Open command palette (`Ctrl+P`)
2. Run "Toggle Vi mode"
3. Status bar shows current mode (`-- NORMAL --`, `-- INSERT --`)
