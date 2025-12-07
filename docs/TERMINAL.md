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

## 6. Key Decisions Needed

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

## 7. References

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
