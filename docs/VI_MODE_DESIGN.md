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
| Visual mode | `v` (char), `V` (line), `Ctrl-v` (block) - select with motions, then `d`/`c`/`y` |
| Text objects | `iw` `aw` (word), `i"` `a"` `i'` `a'` (quotes), `i(` `a(` `i{` `a{` `i[` `a[` (brackets) |
| Insert | `i` `a` `I` `A` `o` `O` |
| Search | `/` `n` `N` |
| Colon cmds | `:w` `:q` `:wq` `:q!` `:e` `:sp` `:vs` `:bn` `:bp` `:<line>` and more |
| Repeat | `.` - repeat last change (works with `x`, `dd`, `dw`, `cw`, insert, etc.) |
| Other | `u` `Ctrl-r` (undo/redo), `p` `P` (paste), `%` (bracket match) |

### Not Implemented

| Feature | Priority | Notes |
|---------|----------|-------|
| Registers | Low | `"a`, `"b`, named registers |
| Macros | Low | `q`, `@` |

## Colon Command Mode

Press `:` in normal mode to enter command mode. Type a command and press Enter to execute.

### Supported Commands

| Command | Aliases | Description |
|---------|---------|-------------|
| `:w` | `:write` | Save current file |
| `:q` | `:quit` | Close buffer (fails if modified) |
| `:q!` | | Force close buffer (discard changes) |
| `:wq` | `:x` | Save and close buffer |
| `:wa` | `:wall` | Save all buffers |
| `:qa` | `:qall` | Close all buffers |
| `:qa!` | | Force close all buffers |
| `:wqa` | `:xa` | Save all and quit |
| `:e` | `:edit` | Reload current file |
| `:e <file>` | | Open file |
| `:sp` | `:split` | Horizontal split |
| `:vs` | `:vsplit` | Vertical split |
| `:only` | | Close other splits |
| `:new` | | New buffer in horizontal split |
| `:vnew` | | New buffer in vertical split |
| `:enew` | | New buffer in current split |
| `:bn` | `:bnext` | Next buffer |
| `:bp` | `:bprev` | Previous buffer |
| `:bd` | `:bdelete` | Close buffer |
| `:ls` | `:buffers`, `:files` | List buffers |
| `:tabnew` | `:tabe` | New tab/buffer |
| `:tabn` | `:tabnext` | Next tab |
| `:tabp` | `:tabprev` | Previous tab |
| `:cn` | `:cnext` | Next diagnostic |
| `:cp` | `:cprev` | Previous diagnostic |
| `:copen` | `:cope` | Open diagnostics panel |
| `:<number>` | | Go to line number |
| `:set nu` | `:set number` | Show line numbers |
| `:set nonu` | `:set nonumber` | Hide line numbers |
| `:noh` | `:nohlsearch` | Clear search highlight |
| `:pwd` | | Print working directory |
| `:f` | `:file` | Show file info |
| `:version` | | Show version |
| `:help` | `:h` | Show help |

### Not Supported

- `:!command` - Shell commands (use terminal)
- `:s/old/new/` - Substitute (use Ctrl+H for search/replace)
- `:g/pattern/` - Global command
- Range prefixes (e.g., `:%`, `:1,10`)

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
