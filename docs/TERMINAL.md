# Built-in Terminal Research for Fresh

## Executive Summary

Adding a built-in terminal to Fresh is a significant undertaking with two main architectural choices: **plugin-based** or **core feature**. Based on research, a **hybrid approach** is recommended - implementing core terminal infrastructure in Rust, exposed via the plugin API.

---

## 1. Plugin vs Core Feature Analysis

### Plugin-Based Approach

**Pros:**
- Faster iteration without modifying core
- Can ship incrementally
- Users can customize/replace
- Follows Fresh's existing architecture

**Cons:**
- Current plugin API is batch-oriented (spawn process → capture output)
- No streaming I/O support for real-time terminal output
- Can't allocate a proper PTY from JavaScript/TypeScript
- Virtual buffers don't support ANSI escape sequences or cursor positioning

**Verdict:** Current plugin API is insufficient for a real terminal. Would only support "run command, show output" workflows.

### Core Feature Approach

**Pros:**
- Full PTY access with proper terminal emulation
- Streaming I/O with proper performance
- Real interactive shell support (vim, htop, etc.)
- Can properly handle resize events (SIGWINCH)

**Cons:**
- More complex implementation
- Increases core codebase size
- Cross-platform PTY differences (Unix vs Windows ConPTY)

**Verdict:** Required for a proper interactive terminal.

### Recommended: Hybrid Approach

Implement terminal infrastructure in Rust core, expose via plugin API:
```rust
// New plugin commands
PluginCommand::CreateTerminal { split_id, shell, cwd }
PluginCommand::WriteToTerminal { terminal_id, data }
PluginCommand::ResizeTerminal { terminal_id, cols, rows }
PluginCommand::CloseTerminal { terminal_id }
```

This gives plugins control while core handles the hard parts (PTY, escape sequences).

---

## 2. Technical Challenges

### A. Terminal Emulation (Hardest Part)

**VT100/ANSI Escape Sequences:**
- Need to parse ~100+ different escape sequences
- State machine complexity (Paul Williams' ANSI parser)
- Partial sequence handling is tricky - incomplete sequences at buffer boundaries can cause exponential parsing slowdowns
- Alternate screen buffer (used by vim, less, htop)
- 24-bit color support, cursor styles, bracketed paste mode

**Recommended Solution:** Use `alacritty_terminal` crate
- Battle-tested (powers Alacritty, Zed uses it)
- Provides: Grid, Term, event_loop, selection handling
- Table-driven parser with excellent performance (~100MB/s throughput)
- Already handles all the edge cases

```toml
[dependencies]
alacritty_terminal = "0.25"
```

### B. PTY Management

**The Problem:**
- Unix uses traditional PTY (master/slave file descriptors)
- Windows uses ConPTY (different API, compatibility quirks)
- Need to handle process spawning, environment variables, shell detection

**Recommended Solution:** Use `portable-pty` crate (from WezTerm)
- Abstracts Unix PTY vs Windows ConPTY
- Trait-based design for runtime implementation selection
- Production-tested in WezTerm

```toml
[dependencies]
portable-pty = "0.9"
```

### C. Resize Handling (SIGWINCH)

**The Flow:**
1. User resizes split containing terminal
2. Editor calculates new terminal dimensions (cols × rows)
3. Call `TIOCSWINSZ` ioctl on PTY master
4. Kernel sends `SIGWINCH` to shell's foreground process group
5. Shell/program queries new size via `TIOCGWINSZ`, redraws

**Challenges:**
- Race conditions between resize and redraw
- Nested PTYs (tmux inside Fresh terminal) may not propagate correctly
- Need to handle resize during high-throughput output

**In Fresh Context:**
- Split resize already triggers re-layout
- Need to hook into split dimension changes
- `alacritty_terminal` provides `Term::resize()` method

### D. Input Handling

**Raw vs Cooked Mode:**
- Terminal needs raw mode for proper key handling
- But Fresh is already in raw mode (crossterm)
- Challenge: routing input to the right place (editor vs terminal)

**Special Keys:**
- `Ctrl+C` → should send SIGINT to terminal process, not exit Fresh
- `Ctrl+Z` → should send SIGTSTP to terminal process
- `Ctrl+D` → EOF signal
- Arrow keys → escape sequences (`\x1b[A`, `\x1b[B`, etc.)
- Mouse events if terminal mouse mode is enabled

**Focus Management:**
- When terminal split is focused, keyboard input goes to PTY
- When editor split is focused, normal editor keybindings apply
- Need clear visual indicator of which split has focus
- Escape hatch: keybinding to switch focus (e.g., `Ctrl+\`)

### E. Output Performance

**The Problem:**
- Programs can output megabytes of data quickly (e.g., `find /`)
- Need to parse escape sequences and update grid without blocking UI
- High-throughput output can overwhelm rendering

**Solutions:**
- Process output in chunks on background thread
- Throttle screen updates (e.g., max 60fps)
- `alacritty_terminal`'s event_loop handles this pattern
- Batch grid updates, only render visible portion

### F. Integration with Fresh's Split System

**Current Fresh Architecture:**
- Tree-based split model (`SplitNode::Leaf` / `SplitNode::Split`)
- Each leaf displays a buffer with independent viewport/cursors
- `SplitViewState` tracks per-split state

**Terminal Integration Options:**

1. **New SplitNode variant:**
   ```rust
   enum SplitNode {
       Leaf(SplitLeaf),
       Split { ... },
       Terminal(TerminalPane),  // New!
   }
   ```

2. **Special buffer type:**
   - Create "terminal buffer" that renders from `alacritty_terminal::Term` grid
   - Fits better with existing architecture
   - Virtual buffer with custom render pipeline

**Recommended:** Option 2 - Terminal as special buffer type
- Less invasive to split system
- Can reuse existing split management (resize, focus, close)
- Terminal content rendered via custom view transform

---

## 3. How Other Editors Solved This

### Zed (Most Similar to Fresh)
- Three-tier architecture: Terminal (PTY) → TerminalView (rendering) → TerminalPanel (workspace)
- Uses `alacritty_terminal` directly
- Terminals can be center panes or docked panels
- Source: `crates/terminal/`, `crates/terminal_view/`

### VS Code
- Uses xterm.js (JavaScript terminal emulator)
- Struggles with ConPTY on Windows - shell integration sequences get misplaced
- Shell integration requires special escape sequences

### Helix
- No built-in terminal yet - users use external terminal in split
- Plugin system in development (PR #8675)
- Waiting for plugin system before adding terminal

### Neovim
- Built-in `:terminal` command
- Terminal buffer is a special buffer type
- Uses libvterm internally
- Terminal mode with distinct keybindings

---

## 4. Dependency Impact Analysis

### New Dependencies Required

Adding terminal support would introduce significant new dependencies:

#### alacritty_terminal (~0.25)
Transitive dependencies include:
- `vte` - VT parser state machine
- `vte_generate_state_changes` - Proc macro for parser tables
- `unicode-width` - Character width calculation
- `parking_lot` - Synchronization primitives
- `log` - Logging (already in Fresh)
- `bitflags` - Bit flag types (already in Fresh)
- `serde` - Serialization (already in Fresh)

#### portable-pty (~0.9)
Transitive dependencies include:
- `libc` - Unix system calls (already in Fresh)
- `winapi` / `windows-sys` - Windows API bindings (Windows only)
- `filedescriptor` - Cross-platform file descriptor handling
- `anyhow` - Error handling
- `shell-words` - Shell argument parsing
- `signal-hook` - Signal handling

### Dependency Concerns

**1. Version Conflicts:**
- `alacritty_terminal` may pin specific versions of shared deps
- Potential conflicts with existing `parking_lot`, `log`, `bitflags` versions
- May require careful version alignment in `Cargo.toml`

**2. Build Time Impact:**
- `vte_generate_state_changes` is a proc macro that generates large state tables
- First build will be slower; incremental builds less affected
- Estimate: +15-30 seconds to clean build

**3. Binary Size:**
- Terminal emulation adds ~500KB-1MB to binary size
- State tables and grid management are not trivial
- Consider: feature flag to make terminal optional?

**4. Platform-Specific Dependencies:**
- Windows: `windows-sys` crate pulls in large Windows API bindings
- macOS/Linux: Minimal additional platform deps
- Cross-compilation becomes more complex

**5. Maintenance Burden:**
- `alacritty_terminal` follows Alacritty's release cycle
- Breaking changes possible (not semver 1.0 yet)
- May need to track upstream for security fixes

### Mitigation Strategies

**Feature Flag Approach:**
```toml
[features]
default = []
terminal = ["alacritty_terminal", "portable-pty"]
```
- Users who don't need terminal can build without it
- Reduces binary size and build time for minimal installs
- Plugin API could gracefully degrade when terminal feature disabled

**Vendoring Consideration:**
- Could vendor specific version of `alacritty_terminal`
- Pros: Stability, no surprise breakage
- Cons: Miss security updates, maintenance burden

**Minimal Implementation Alternative:**
- Could implement basic terminal without `alacritty_terminal`
- Use only `portable-pty` + custom simple parser
- Support basic output but not full terminal emulation (no vim/htop)
- Much smaller dependency footprint

---

## 5. Implementation Roadmap

### Phase 1: Core Infrastructure
- Add `alacritty_terminal` and `portable-pty` dependencies
- Create `TerminalManager` service (similar to `LspManager`)
- Implement PTY spawning with proper shell detection
- Basic input/output piping

### Phase 2: Rendering Integration
- Create `TerminalBuffer` type (or special virtual buffer)
- Implement render pipeline from `Term` grid → Fresh tokens
- Handle ANSI colors → Fresh theme colors mapping
- Cursor rendering in terminal mode

### Phase 3: Split Integration
- Terminal buffers displayable in any split
- Proper focus handling (editor vs terminal mode)
- Resize propagation to PTY
- Visual focus indicators

### Phase 4: Input & Keybindings
- Route keyboard input to focused terminal
- Terminal-mode keybindings (send Ctrl+C to process, not Fresh)
- Escape hatch keybinding to unfocus terminal
- Mouse support (if terminal enables mouse mode)

### Phase 5: Polish & Features
- Multiple terminal support
- Shell integration (cwd tracking, command detection)
- Scrollback buffer
- Copy/paste support
- Find in terminal output

---

## 6. Known Issues and Bugs (Current Implementation)

Testing conducted via tmux automation on 2025-12-09.

### Critical Issues

#### 1. "Read-only" Mode is Not Read-Only
**Severity:** Critical
**Steps to reproduce:**
1. Open a terminal (`Open Terminal` from command palette)
2. Run some commands
3. Press `Ctrl+Space` to exit terminal mode
4. Status bar shows "Terminal mode disabled - read only (Ctrl+Space to resume)"
5. Type any text (e.g., "gg")

**Expected:** Text input should be rejected in read-only mode
**Actual:** Text is inserted into the buffer, modifying it

#### 2. Keybindings Don't Work in "Read-Only" Terminal Buffer Mode
**Severity:** Critical
**Steps to reproduce:**
1. Exit terminal mode with `Ctrl+Space`
2. Press `Escape`, `u` (undo), or navigation keys like `gg`

**Expected:** Editor keybindings should work (undo, navigation, etc.)
**Actual:** All keys are typed as text into the buffer instead of executing keybindings

#### 3. View Doesn't Scroll to Cursor When Resuming Terminal Mode
**Severity:** High
**Steps to reproduce:**
1. Generate scrollback output: `for i in {1..50}; do echo "Line $i"; done`
2. Press `Shift+PageUp` to enter scrollback mode
3. Scroll up with `PageUp`
4. Press `Ctrl+Space` to resume terminal mode

**Expected:** View should scroll to show current shell prompt (cursor position)
**Actual:** View stays at the scrolled position; user cannot see what they're typing

### Design Issues

#### 4. Inconsistent Display Between Terminal Mode and Exit Mode
**Severity:** Medium
**Description:** Exiting and re-entering terminal mode should be seamless with identical rendering. Currently:
- In terminal mode: Shows live terminal output without line numbers
- After `Ctrl+Space`: Shows line numbers, different content layout
- After `Shift+PageUp` scrollback: Shows line numbers, proper content

The `Ctrl+Space` exit mode and `Shift+PageUp` scrollback mode behave differently.

#### 5. Status Bar Message Truncated on 80-Column Screens
**Severity:** Low
**Description:** On the default 80x24 layout, the status text "Terminal mode disabled - read only (Ctrl+Space to resume)" is truncated to "Terminal mode dis..." because the status bar runs out of space.
**Impact:** The "read only" marker and the "Ctrl+Space to resume" hint disappear, and e2e assertions looking for "disabled" do not match the shortened string.
**Recommendation:** Shorten the status text (e.g., "Terminal read-only (Ctrl+Space)") or reserve more status-bar space for status messages on narrow terminals; as a stopgap in tests, run the e2e harness with a wider terminal (e.g., 120 columns) to avoid truncation.

### Working Features (Verified)

The following features work correctly:
- Basic command execution (echo, ls, pwd, cat)
- Tab completion
- Command history (arrow keys)
- Ctrl+C interrupt (sends SIGINT)
- Ctrl+U (clear line)
- Interactive TUI programs: vim, htop, less
- Alternate screen buffer handling (vim/htop restore screen on exit)
- Scrollback viewing via `Shift+PageUp`
- PageUp/PageDown navigation in scrollback mode
- Terminal resize (stty size reports correct dimensions)

### Recommendations

1. **Fix read-only mode**: The buffer should truly be read-only when terminal mode is disabled, or rename the mode to avoid confusion.

2. **Unify exit modes**: Consider merging `Ctrl+Space` exit and `Shift+PageUp` scrollback into a single consistent mode.

3. **Auto-scroll on resume**: When resuming terminal mode, automatically scroll to show the cursor/prompt position.

4. **Ensure keybindings work**: When not in terminal mode, standard editor keybindings should function normally.

5. **Persist full session output to disk**: Today, scrollback is populated from the emulator’s in-memory history at the moment we call `sync_terminal_to_buffer`, so very old output can be evicted. To capture the entire session, append PTY output bytes in the reader loop to the terminal’s backing file (or a dedicated log) as they arrive. Keep rendering from the emulator for live view, but use the ever-growing log for deep history and postmortem scrollback. Consider rotation/limits and graceful handling of disk errors.

---

## 7. Scrollback + Logging Plan (Current Work)

- PTY is now required; the headless fallback has been removed. Run the e2e harness in a real terminal (tmux works) so `portable-pty` can allocate a PTY. Add a note in PTY-dependent tests that they must run with a PTY.
- Each terminal session writes raw PTY bytes to per-project storage under `$XDG_DATA_HOME/fresh/terminals/{encoded_workdir}/fresh-terminal-{id}.log` from the moment it is spawned. This log captures everything emitted by the PTY (escape sequences included) and is kept for session restore.
- When leaving terminal mode, we replay the log through a fresh `alacritty_terminal::Term` to render a plain-text snapshot into a separate backing file in the same directory; the read-only buffer reloads from that rendered file while the raw log remains intact for full-fidelity capture and session restore.
- Terminal buffers keep line numbers hidden and line wrapping disabled both live and when terminal mode is toggled off, so the read-only view matches the live terminal layout.
- Follow-ups: decide on log retention/rotation and whether users can opt out or relocate the log files, and whether to offer a separate “replay from log” view for very long sessions.

---

## 8. E2E Test Status

Command: `cargo test --test e2e_tests terminal -- --nocapture` (80x24 harness, requires PTY support).

- Result: 25 passed, 0 failed, 2 ignored.
- Changes: terminal tabs render as `*Terminal N*` instead of backing-file names.
- Remaining gap: the long "Terminal mode disabled - read only (Ctrl+Space to resume)" string is still truncated at 80 columns; using a wider harness (e.g., 120 columns) avoids truncation until the message is shortened.

---

## 9. Key Decisions Needed

1. **Plugin API vs Core Only?**
   - Expose terminal creation to plugins?
   - Allow plugins to interact with terminal output?

2. **Default Shell Detection:**
   - Use `$SHELL` on Unix, `cmd.exe`/`powershell` on Windows?
   - User configurable?

3. **Keybinding Strategy:**
   - How to escape from terminal mode?
   - Which keys are captured vs passed through?

4. **Where Terminals Can Live:**
   - Any split? Dedicated panel? Both?
   - Persist across sessions?

5. **Scrollback Size:**
   - How much history to keep?
   - Memory implications for long-running sessions

6. **Feature Flag:**
   - Should terminal be optional at compile time?
   - What's the default?

---

## 10. References

- [VS Code Terminal Advanced](https://code.visualstudio.com/docs/terminal/advanced)
- [Zed Terminal Architecture](https://deepwiki.com/zed-industries/zed/3.3-terminal)
- [Helix FAQ](https://helix-editor.vercel.app/help/faq)
- [alacritty_terminal crate](https://docs.rs/alacritty_terminal)
- [portable-pty crate](https://docs.rs/portable-pty/latest/portable_pty/)
- [xterm.js Parser Hooks](https://xtermjs.org/docs/guides/hooks/)
- [Entering text in the terminal is complicated](https://jvns.ca/blog/2024/07/08/readline/)
- [SIGWINCH handling](https://unix.stackexchange.com/questions/580362/how-are-terminal-information-such-as-window-size-sent-to-a-linux-program)
- [VT100 ANSI Parser](https://vt100.net/emu/dec_ansi_parser)
- [ConPTY VS Code Issues](https://github.com/microsoft/terminal/issues/12806)
