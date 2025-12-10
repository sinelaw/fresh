# Built-in Terminal for Fresh

## Architecture Overview

Fresh's terminal is implemented as a special buffer type backed by `alacritty_terminal` for VT100/ANSI emulation and `portable-pty` for cross-platform PTY management. Terminals can be displayed in any split and support two modes:

- **Terminal mode**: Live interactive shell, input goes to PTY
- **Scrollback mode**: Read-only buffer view with editor navigation/selection

---

## Incremental Scrollback Streaming

The terminal uses an **incremental streaming architecture** that avoids O(n) work on mode switches and session restore. The key insight is that scrollback history is append-only.

### File Structure

Each terminal maintains a single **backing file** containing rendered text:

```
~/.local/share/fresh/terminals/{encoded_workdir}/fresh-terminal-{id}.txt
```

The backing file structure:

```
┌─────────────────────────────────────────┐
│ Scrollback history (append-only)        │  ← grows incrementally as lines
│ Line 1                                  │    scroll off the top of screen
│ Line 2                                  │
│ ...                                     │
│ Line N                                  │
├─────────────────────────────────────────┤
│ Visible screen (rewritable tail)        │  ← present only in scrollback mode
│ Screen line 0                           │    (~50 lines, rewritten each switch)
│ ...                                     │
│ Screen line 49                          │
└─────────────────────────────────────────┘
```

### Data Flow

**During terminal operation (PTY read loop):**

```
PTY output bytes
    │
    ▼
state.process_output()  ──►  TerminalState (in-memory grid)
    │
    ▼
check: history_size increased?
    │
   YES ──►  append new scrollback lines to backing file
            (one line at a time, as they scroll off screen)
```

**Exit terminal mode (enter scrollback mode):**

```
1. Append visible screen (~50 lines) to backing file
2. Load backing file as read-only buffer (lazy load, instant)
```

**Re-enter terminal mode:**

```
1. Truncate backing file to scrollback-only (remove visible screen tail)
2. Resume live terminal rendering from TerminalState
```

**Quit while in terminal mode:**

```
1. Append visible screen to backing file (ensure complete state)
2. Save session as normal
```

**Session restore:**

```
1. Load backing file directly (lazy load, instant)
2. User starts in scrollback mode viewing last session state
3. Raw log replay only if user re-enters terminal mode (deferred)
```

### Performance Characteristics

| Operation | Before | After |
|-----------|--------|-------|
| Mode switch | ~500ms (replay + full_content_string) | ~5ms (append 50 lines) |
| Session restore | ~1000ms (replay 2x) | ~10ms (lazy load) |
| PTY read overhead | ~0 | ~0.1ms per scroll (append one line) |

### State Tracking

```rust
pub struct TerminalState {
    term: Term<NullListener>,
    parser: Processor,
    cols: u16,
    rows: u16,
    dirty: bool,
    terminal_title: String,

    // Incremental streaming state
    synced_history_lines: usize,      // lines already written to backing file
    backing_file_history_end: u64,    // byte offset where scrollback ends
}
```

### Key Methods

```rust
impl TerminalState {
    /// Append any new scrollback lines to the backing file.
    /// Called after process_output() in the PTY read loop.
    pub fn flush_new_scrollback<W: Write>(&mut self, writer: &mut W) -> io::Result<usize>;

    /// Append visible screen content to the backing file.
    /// Called when exiting terminal mode.
    pub fn append_visible_screen<W: Write>(&self, writer: &mut W) -> io::Result<()>;

    /// Get byte offset where scrollback ends (for truncation on mode re-entry).
    pub fn backing_file_history_end(&self) -> u64;
}
```

### Terminal Resize Handling

When terminal is resized:
- Old scrollback lines remain as-is (rendered at old width)
- New scrollback lines are rendered at new width
- The editor's line wrapping handles display of mixed-width lines
- No O(n) rewrite of history required

This is a feature: original output is preserved at original width rather than being re-wrapped or truncated.

---

## Raw Log File (Optional)

For re-entering terminal mode after session restore, a raw log of PTY bytes is maintained:

```
~/.local/share/fresh/terminals/{encoded_workdir}/fresh-terminal-{id}.log
```

This file:
- Contains raw VTE escape sequences exactly as received from PTY
- Enables rebuilding full `TerminalState` via replay
- Only needed if user wants to resume live terminal after restore
- Can be disabled if terminals always start fresh on session restore

---

## Mode Switching

### Terminal Mode → Scrollback Mode (Ctrl+Space)

1. Append visible screen to backing file
2. Update `backing_file_history_end` to current file position minus screen size
3. Load backing file as read-only buffer (lazy load)
4. Set `editing_disabled = true`
5. User can navigate, select, copy, search

### Scrollback Mode → Terminal Mode (Ctrl+Space)

1. Truncate backing file to `backing_file_history_end`
2. Set `editing_disabled = false`
3. Resume live rendering from `TerminalState`
4. Scroll view to cursor position (bottom of terminal)

---

## Session Persistence

### Session Save

```rust
struct TerminalSession {
    pub id: u64,
    pub shell: String,
    pub cwd: PathBuf,
    pub cols: u16,
    pub rows: u16,
    pub backing_path: PathBuf,
    pub log_path: PathBuf,  // for optional live terminal resume
}
```

Before saving session:
- If in terminal mode, append visible screen to backing file
- This ensures backing file always contains complete state

### Session Restore

1. Load backing file directly as read-only buffer
2. User sees last session state immediately (lazy load)
3. If user presses Ctrl+Space to enter terminal mode:
   - Spawn new PTY with same shell/cwd
   - Optionally replay raw log to restore `TerminalState`
   - Or start fresh (simpler, recommended default)

---

## Integration with Existing Buffer System

The backing file integrates with Fresh's existing file-backed buffer architecture:

- Files > 1MB use lazy loading (`BufferData::Unloaded`)
- Chunks loaded on-demand as user scrolls
- Full 200K line scrollback (~15MB) loads instantly
- Search, selection, copy all work via normal buffer mechanisms

This is why the incremental streaming approach works: we're not building a new system, we're leveraging the existing efficient buffer infrastructure.

---

## Implementation Checklist

### Core Changes

- [ ] Add `synced_history_lines` and `backing_file_history_end` to `TerminalState`
- [ ] Implement `flush_new_scrollback()` method
- [ ] Implement `append_visible_screen()` method
- [ ] Update PTY read loop to call `flush_new_scrollback()` after `process_output()`
- [ ] Pass backing file writer to PTY read thread

### Mode Switch Changes

- [ ] `sync_terminal_to_buffer()`: append screen + lazy load (no replay)
- [ ] `enter_terminal_mode()`: truncate backing file
- [ ] On quit: ensure visible screen is appended before session save

### Session Restore Changes

- [ ] Load backing file directly (skip log replay)
- [ ] Defer log replay to `enter_terminal_mode()` if needed
- [ ] Consider removing log replay entirely (fresh terminal on restore)

### Cleanup

- [ ] Remove `full_content_string()` method (no longer needed)
- [ ] Remove `replay_terminal_log_into_state()` from restore path
- [ ] Update tests for new architecture

---

## Known Issues

### Critical

1. **Read-only mode accepts input**: Text is inserted into buffer in scrollback mode. Fix: ensure `editing_disabled` is respected.

2. **Keybindings don't work in scrollback mode**: All keys typed as text. Fix: ensure `KeyContext::Normal` is set on mode exit.

### High Priority

3. **View doesn't scroll to cursor on resume**: After scrolling in scrollback mode, resuming terminal mode leaves view at wrong position. Fix: scroll to bottom on mode entry.

### Medium Priority

4. **Inconsistent display between modes**: Line numbers and layout differ. Consider unifying visual presentation.

5. **Status message truncated on narrow terminals**: "Terminal mode disabled..." too long for 80 columns.

---

## Technical Details

### Dependencies

```toml
alacritty_terminal = "0.25"  # VT100/ANSI terminal emulation
portable-pty = "0.9"         # Cross-platform PTY management
```

### alacritty_terminal Capabilities Used

- `Term::grid()` - access to scrollback via negative line indices
- `grid.history_size()` - track scrollback growth
- `grid[Line(-n)]` - read scrollback lines
- `Term::selection` - native selection support
- `Term::selection_to_string()` - copy selected text
- `Term::scroll_display()` - scroll through history

### Scrollback Access

```rust
let grid = term.grid();
let history_size = grid.history_size();

// Scrollback lines: Line(-history_size) to Line(-1)
// Visible screen: Line(0) to Line(rows-1)

for i in (1..=history_size).rev() {
    let line = Line(-(i as i32));
    let row_data = &grid[line];
    // ... write line to backing file
}
```

---

## References

- [alacritty_terminal docs](https://docs.rs/alacritty_terminal/latest/alacritty_terminal/)
- [portable-pty docs](https://docs.rs/portable-pty/latest/portable_pty/)
- [Zed Terminal Architecture](https://deepwiki.com/zed-industries/zed/3.3-terminal)
