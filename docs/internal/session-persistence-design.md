# Session Persistence Design (tmux-style)

> **Status**: Experimental (implemented in v0.1.99+)
>
> This feature is functional but considered experimental. The implementation may have edge cases and the API may change. See [user documentation](../features/session-persistence.md) for usage.

This document describes the design for tmux-style session persistence in Fresh, enabling users to detach from and reattach to editor sessions while background processes continue running.

## Implementation Status

| Feature | Status | Notes |
|---------|--------|-------|
| Detach/Reattach | Done | `Ctrl+Shift+D` or command palette |
| Layout Persistence | Done | Via existing workspace system |
| Buffer State | Done | Cursor positions, unsaved changes preserved |
| Shell Continuity | Done | Terminal processes survive detach |
| CLI (`list`, `kill`, `attach`) | Done | See `fresh --help` |
| Named Sessions | Done | `--session <name>` flag |
| Cross-platform IPC | Done | Unix sockets + Windows named pipes |
| Multi-Client | Not implemented | Only one client at a time |
| Resurrection (crash recovery) | Not implemented | Server crash loses state |

## Overview

Fresh adopts a **client-server architecture** where:
- **Server (daemon)**: Holds editor state, manages buffers, owns PTYs, runs in background
- **Client (UI)**: Lightweight renderer that connects to server, sends input, receives display updates

This enables:
1. Detach/reattach without losing state
2. Shell continuity (long-running tasks survive UI closure)
3. Session resurrection after server crash (via disk serialization)
4. Multi-client support (multiple terminals viewing same session)

---

## UX Requirements

### Must-Have (MVP)

| Requirement | Description |
|-------------|-------------|
| **Detach/Reattach** | Close terminal without killing editor. `fresh attach` restores exactly where you left off |
| **Layout Persistence** | Split panes, tab groups, window sizes restored |
| **Buffer State** | Open files, unsaved changes (dirty buffers), cursor positions preserved |
| **Shell Continuity** | Integrated terminal panes continue running in background |

### Nice-to-Have (Post-MVP)

| Requirement | Description |
|-------------|-------------|
| **Multi-Client** | Two terminals attach to same session (mirroring) |
| **Session CLI** | `fresh ls`, `fresh kill <id>`, `fresh new -s <name>` |
| **Resurrection** | State serialized to disk, survives server crash/reboot (processes lost) |

---

## Current Architecture Analysis

Fresh's existing architecture is well-suited for this change:

### Existing Strengths

1. **Workspace persistence already exists** (`src/workspace.rs`)
   - Saves: split layout, open files, cursor positions, search history, bookmarks, terminal sessions
   - Atomic writes (temp file + rename)
   - Per-working-directory sessions

2. **Async bridge pattern proven** (`src/services/async_bridge.rs`)
   - Tokio runtime in background thread
   - Main thread synchronous (rendering + input)
   - `mpsc` channels bridge async→sync
   - 70+ message types for LSP, files, plugins, terminals

3. **Terminal emulation ready** (`alacritty_terminal` + `portable-pty`)
   - Incremental scrollback streaming to backing files
   - Mode switching (terminal ↔ scrollback)
   - Session restore with replay

4. **Trait-based abstractions**
   - `FileSystem` trait (local/remote implementations)
   - `ProcessSpawner` trait (local/remote)
   - Easy to add IPC-based implementations

### Architecture Gap

Currently Fresh is a single process:

```
┌─────────────────────────────────────────────────────────┐
│                    Fresh Process                         │
│  ┌──────────────┐  ┌──────────────┐  ┌───────────────┐  │
│  │ Terminal I/O │  │ Editor State │  │ Tokio Runtime │  │
│  │ (ratatui)    │◄─►│ (buffers)    │◄─►│ (LSP, etc)    │  │
│  └──────────────┘  └──────────────┘  └───────────────┘  │
└─────────────────────────────────────────────────────────┘
```

Target architecture (dual-socket):

```
┌─────────────────────────────┐        ┌─────────────────────────────────┐
│       Client Process        │        │        Server Process           │
│  ┌───────────────────────┐  │        │  ┌──────────────┐  ┌─────────┐  │
│  │ Terminal stdin/stdout │  │  Data  │  │ Input Parser │  │ Tokio   │  │
│  │ (raw byte relay)      │◄─┼────────┼─►│ (bytes→events)│◄─►│ Runtime │  │
│  └───────────────────────┘  │ Socket │  └──────────────┘  └─────────┘  │
│  ┌───────────────────────┐  │        │  ┌──────────────┐               │
│  │ Resize/Control        │◄─┼────────┼─►│ Editor Core  │               │
│  │ (SIGWINCH handler)    │  │Control │  │ (ratatui)    │               │
│  └───────────────────────┘  │ Socket │  └──────────────┘               │
└─────────────────────────────┘        │  ┌──────────────┐               │
                                       │  │ PTY Manager  │               │
                                       │  │ (terminals)  │               │
                                       │  └──────────────┘               │
                                       └─────────────────────────────────┘
```

**Data Socket**: Pure byte stream, no framing. stdin→server, server→stdout.
**Control Socket**: JSON messages for resize, ping/pong, title, quit.

---

## Technical Architecture

### Design Principle: Ultra-Light Client

The client should be as simple as possible - ideally a "dumb pipe" that relays bytes between the terminal and server. All complexity lives server-side.

**Client responsibilities (absolute minimum):**
- Connect to server socket
- Set terminal to raw mode
- Send initial terminal size
- Relay: stdin → socket, socket → stdout
- Detect resize, notify server
- Restore terminal on exit

**Server responsibilities (everything else):**
- Send terminal setup sequences (alternate screen, mouse mode, cursor style)
- Parse raw input bytes into key/mouse events
- All editor logic, buffer management, LSP, plugins
- Render via ratatui, capture ANSI output
- Send ANSI bytes to client

This means the server sends escape sequences like `\x1b[?1049h` (alternate screen) and `\x1b[?1000h` (mouse enable) which flow through the client to the terminal. The client doesn't interpret them.

### Component Responsibilities

#### Server (Daemon)

- Holds all editor state (`Editor` struct)
- Manages text buffers and undo/redo
- Owns PTY masters for integrated terminals
- Runs LSP clients
- Executes plugins
- **Parses raw input bytes** (using crossterm's byte-level parser)
- **Sends terminal setup/teardown sequences**
- Renders to virtual terminal buffer
- Sends raw ANSI output to connected clients

#### Client (Dumb Relay)

- Establishes IPC connection to server
- Sets terminal to raw mode
- Sends terminal size on connect and resize
- **Relays stdin bytes → server (minimal or no parsing)**
- **Relays server bytes → stdout (no parsing)**
- Handles SIGWINCH (resize signal)
- Restores terminal on disconnect

### IPC Mechanism

| Platform | Implementation |
|----------|----------------|
| **Linux/macOS** | Unix domain socket at `/run/user/$UID/fresh/fresh-$SESSION.sock` or `$XDG_RUNTIME_DIR/fresh/` |
| **Windows** | Named pipe at `\\.\pipe\fresh-$SESSION` |

**Library**: [`interprocess`](https://crates.io/crates/interprocess) crate (handles both Unix sockets and named pipes)

---

## Ultra-Light Client Architecture

The goal is a client so simple it's nearly trivial - under 100 lines of core logic.

### Minimal Client Implementation

```rust
fn client_main() -> io::Result<()> {
    let socket = connect_to_server()?;

    // 1. Send terminal size (only structured data we ever send)
    let size = get_terminal_size()?;
    socket.write_all(&size.to_bytes())?;

    // 2. Set raw mode
    terminal::enable_raw_mode()?;

    // 3. Bidirectional byte relay
    let result = relay_bidirectional(&mut stdin(), &mut stdout(), &socket);

    // 4. Restore terminal (server sends cleanup sequences before disconnect)
    terminal::disable_raw_mode()?;

    result
}

fn relay_bidirectional(stdin: &mut Stdin, stdout: &mut Stdout, socket: &Socket) -> io::Result<()> {
    // Use poll/select or two threads to relay bytes in both directions
    // stdin → socket (user input)
    // socket → stdout (server output)
}
```

### The Resize Problem

Terminal resize (SIGWINCH) can **only** be detected client-side. This is the one piece of complexity we can't fully eliminate. Several approaches:

#### Approach 1: Escape Byte Framing (Recommended)

Use a single escape byte in the client→server stream to distinguish control messages from input:

```
Client → Server byte stream:
  0x00 0x00        = literal 0x00 byte (escaped null)
  0x00 0x01 W1 W2 H1 H2  = resize event (width/height as u16 LE)
  0x00 0x02        = detach request
  0x01-0xFF        = raw input byte (passed through)
```

**Tradeoffs:**
| Pros | Cons |
|------|------|
| Single socket, simple | Client must escape 0x00 bytes in input |
| Minimal framing overhead | Slightly more client logic |
| Works with any terminal | Need to handle partial reads |

**Client complexity:** ~20 extra lines for escaping and control messages

#### Approach 2: Dual Socket (Data + Control)

Separate connections for data and control:

```
┌─────────┐     data socket (pure bytes, zero framing)     ┌─────────┐
│         │ ←─────────────────────────────────────────────→│         │
│ Client  │                                                │ Server  │
│         │ ←── control socket (resize, detach, quit) ────→│         │
└─────────┘                                                └─────────┘
```

**Tradeoffs:**
| Pros | Cons |
|------|------|
| Data path is pure byte relay | Two sockets to manage |
| Zero overhead on hot path | More connection setup |
| Cleaner separation | Control socket needs minimal protocol |

**Client complexity:** Slightly more setup, but relay loop is simpler

#### Approach 3: Reconnect on Resize

Treat resize as a reconnection event:

```rust
fn client_main() -> io::Result<()> {
    loop {
        let socket = connect_to_server()?;
        socket.write_all(&get_terminal_size()?.to_bytes())?;

        terminal::enable_raw_mode()?;
        let reason = relay_until_signal(&socket)?;
        terminal::disable_raw_mode()?;

        match reason {
            Signal::Resize => continue,    // Reconnect with new size
            Signal::Disconnect => break,   // Exit
        }
    }
    Ok(())
}
```

**Tradeoffs:**
| Pros | Cons |
|------|------|
| Simplest client logic | Brief flicker on resize |
| No framing protocol needed | Server must handle reconnects |
| Easy to implement | Full redraw on every resize |

**Client complexity:** Minimal - no protocol, just reconnect

#### Approach 4: Server-Side Size Query (Limited)

Server periodically queries terminal size via XTWINOPS escape sequence:

```
Server sends: \x1b[18t  (request terminal size)
Terminal responds: \x1b[8;rows;colst
```

**Tradeoffs:**
| Pros | Cons |
|------|------|
| Zero client resize logic | Not all terminals support it |
| Pure byte relay client | Response mixed with user input |
| | Requires server to parse input stream |
| | Adds latency to resize detection |

**Client complexity:** Zero extra, but unreliable

#### Approach 5: Polling (Fallback)

Client periodically checks terminal size and sends if changed:

```rust
fn resize_poll_loop(socket: &Socket) {
    let mut last_size = get_terminal_size();
    loop {
        sleep(Duration::from_millis(250));
        let size = get_terminal_size();
        if size != last_size {
            send_resize(socket, size);
            last_size = size;
        }
    }
}
```

**Tradeoffs:**
| Pros | Cons |
|------|------|
| Works without signals | Up to 250ms resize lag |
| Cross-platform | Wastes CPU polling |
| Simple implementation | Still needs framing for resize message |

### Recommendation

**Use Approach 2: Dual Socket (Data + Control)**

Reasons:
- **Hot path stays pure**: Data socket is zero-overhead byte relay, no scanning for escape bytes
- **Control path is explicit**: Resize, heartbeat, title changes go through structured channel
- **Debuggable**: Can hexdump data socket cleanly, control messages are readable JSON
- **Robust**: No byte-stuffing edge cases, no reconnection race conditions

The slight increase in client complexity (~20 extra lines for control socket) is worth the architectural cleanliness.

**Rejected alternatives:**
- **Escape byte framing**: Requires scanning every input byte for `0x00`. Adds CPU overhead on large pastes, complicates debugging.
- **Reconnect on resize**: Causes visual flicker, race conditions during reconnect, jarring UX especially over SSH.

### Client Complexity Comparison

Summary of what the client does and doesn't do:

| Aspect | Ultra-Light Client | Traditional Client |
|--------|-------------------|-------------------|
| **Lines of code** | ~80-100 | ~500+ |
| **Terminal setup** | Raw mode only | Raw mode, mouse, alt screen |
| **Input handling** | Relay raw bytes | Parse into events |
| **Output handling** | Write raw bytes | Parse ANSI or render ops |
| **Protocol** | Escape byte or none | Structured messages |
| **Dependencies** | interprocess, libc | crossterm, serde, bincode |
| **State** | None | Terminal state, pending msgs |
| **Failure modes** | Socket disconnect | Many |

The ultra-light approach pushes complexity to the server where it can be properly managed, tested, and doesn't affect the user's terminal if something goes wrong.

### Server-Side Input Parsing

The server receives raw bytes and must parse them into events. Crossterm provides this:

```rust
use crossterm::event::{Event, KeyEvent, MouseEvent};

// Server-side: parse raw bytes into events
fn parse_input(bytes: &[u8]) -> Vec<Event> {
    let mut parser = crossterm::event::EventParser::new();
    let mut events = Vec::new();

    for &byte in bytes {
        if let Some(event) = parser.advance(byte) {
            events.push(event);
        }
    }

    events
}
```

This keeps the existing input handling code in Fresh mostly unchanged - it still receives `KeyEvent` and `MouseEvent`, just parsed server-side instead of client-side.

### Protocol Design (Dual Socket)

Two sockets per client connection:

#### Socket A: Data Stream (Hot Path)

Pure byte relay, no framing:

```
Client → Server: Raw stdin bytes (keyboard, mouse, paste)
Server → Client: Raw ANSI bytes (ratatui output)
```

```rust
// Client data relay - this is the entire hot path
loop {
    select! {
        n = stdin.read(&mut buf) => data_socket.write_all(&buf[..n])?,
        n = data_socket.read(&mut buf) => stdout.write_all(&buf[..n])?,
    }
}
```

On Linux, consider using `splice()` for zero-copy relay.

#### Socket B: Control Channel (Cold Path)

Low-bandwidth JSON messages for out-of-band communication:

```json
// Client → Server
{ "type": "resize", "cols": 120, "rows": 40 }
{ "type": "ping" }
{ "type": "detach" }

// Server → Client
{ "type": "pong" }
{ "type": "set_title", "title": "fresh - main.rs" }
{ "type": "bell" }
{ "type": "quit", "reason": "user_quit" }
{ "type": "error", "message": "..." }
```

### Connection Handshake

The handshake happens on the control socket before data flows:

```
Client                              Server
   |                                   |
   |------ connect data socket ------->|
   |------ connect control socket ---->|
   |                                   |
   |====== HANDSHAKE (control) ========|
   |                                   |
   |-- ClientHello ------------------->|
   |   { version, term_size, env }     |
   |                                   |
   |<----------------- ServerHello ----|
   |       { version, session_id }     |
   |              OR                   |
   |<--------------- VersionMismatch --|
   |       { server_version, action }  |
   |                                   |
   |====== DATA FLOW BEGINS ===========|
```

#### ClientHello (Required Fields)

```json
{
  "type": "hello",
  "protocol_version": 1,
  "client_version": "0.15.0",
  "cols": 120,
  "rows": 40,
  "env": {
    "TERM": "xterm-256color",
    "COLORTERM": "truecolor",
    "LANG": "en_US.UTF-8",
    "LC_ALL": null
  }
}
```

**Why environment matters:**
- `TERM`: Server must generate escape codes compatible with client's terminal
- `COLORTERM`: Detect truecolor support (24-bit color vs 256-color vs 16-color)
- `LANG`/`LC_ALL`: UTF-8 support detection

The server initializes its ratatui backend using these values for this specific client context.

#### ServerHello

```json
{
  "type": "hello",
  "protocol_version": 1,
  "server_version": "0.15.0",
  "session_id": "home_user_myproject"
}
```

#### Version Mismatch Handling

If client and server protocol versions differ:

```json
{
  "type": "version_mismatch",
  "server_version": "0.14.0",
  "client_version": "0.15.0",
  "action": "restart_server",
  "message": "Server is outdated. Restart to upgrade?"
}
```

Client can then offer to:
1. Kill the old server (triggers session checkpoint)
2. Start new server with updated binary
3. Reconnect

This prevents subtle protocol bugs when users upgrade Fresh.

### Rendering Strategy

Server captures ratatui output and sends raw bytes:

```rust
// Server side: capture ANSI output
struct CapturingBackend {
    buffer: Vec<u8>,
    size: (u16, u16),
}

impl Backend for CapturingBackend {
    fn draw<'a, I>(&mut self, content: I) -> io::Result<()>
    where I: Iterator<Item = (u16, u16, &'a Cell)> {
        // Write ANSI sequences to self.buffer instead of stdout
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(()) // No-op, we're capturing
    }
}

// After render
let output = backend.take_buffer();
client_socket.write_all(&output)?;
```

Client simply writes received bytes to stdout - no parsing, no interpretation.

### Terminal Restoration

When the connection ends, the terminal must be restored (disable raw mode, show cursor, restore main screen). Two approaches:

#### Server-Initiated Restoration (Preferred)

Server sends cleanup sequences before closing connection:

```rust
// Server, before disconnect
fn graceful_disconnect(client: &mut Client) {
    // Send terminal restoration sequences
    let cleanup = concat!(
        "\x1b[?1049l",     // Exit alternate screen
        "\x1b[?25h",       // Show cursor
        "\x1b[?1000l",     // Disable mouse
        "\x1b[0m",         // Reset attributes
    );
    let _ = client.socket.write_all(cleanup.as_bytes());
    let _ = client.socket.flush();
    // Then close socket
}
```

**Advantage**: Even if client crashes during write, terminal is likely restored.

#### Client-Side Restoration (Fallback)

Client always calls `disable_raw_mode()` on exit:

```rust
// Client uses RAII or explicit cleanup
fn client_main() {
    enable_raw_mode()?;
    let _guard = scopeguard::guard((), |_| {
        let _ = disable_raw_mode();
    });
    // ... relay loop ...
}
```

**For robustness**: Do both. Server sends sequences, client also restores locally.

### Handling Client Crashes

If client crashes mid-session:
- Server detects socket close, removes client from list
- Server continues running (no data loss)
- User's terminal may be in bad state → user runs `reset` command
- User runs `fresh attach` to reconnect

If server crashes:
- Client detects socket close
- Client restores terminal (disable raw mode)
- Client exits with error message
- User runs `fresh` to start new server (with session restore from checkpoint)

### Clipboard Integration (OSC 52)

**The Problem:** A daemonized server is detached from the GUI session. Clipboard libraries (`arboard`, `copypasta`) will fail because there's no X11/Wayland/macOS GUI context.

**The Solution:** Use OSC 52 escape sequences for clipboard operations.

#### How OSC 52 Works

```
Server generates:    \x1b]52;c;BASE64_TEXT\x07
     ↓
Server writes to data socket
     ↓
Client relays to stdout (no parsing)
     ↓
Terminal emulator receives and sets system clipboard
```

#### Implementation

```rust
// Server: when user yanks text
fn yank_to_clipboard(text: &str) {
    let encoded = base64::encode(text);
    let osc52 = format!("\x1b]52;c;{}\x07", encoded);
    // Write to client's data socket - flows through to terminal
    client.data_socket.write_all(osc52.as_bytes())?;
}
```

#### Caveats

1. **Terminal support required**: Most modern terminals support OSC 52, but some have it disabled by default for security
   - Alacritty: Enabled by default
   - iTerm2: Enabled by default
   - WezTerm: Enabled by default
   - tmux: Requires `set-option -g set-clipboard on`
   - Some terminals: May prompt user for permission

2. **Size limits**: Some terminals limit OSC 52 payload size (~100KB typical)

3. **Paste (read clipboard)**: OSC 52 read is less widely supported and has security implications. For paste, rely on terminal's bracketed paste mode which sends clipboard contents as input.

#### Fallback Strategy

1. **Primary**: OSC 52 (works through relay)
2. **Fallback**: If client detects it's running locally (not over SSH), it can optionally handle clipboard directly
3. **User notification**: If OSC 52 fails (terminal doesn't support), show status message "Yanked to editor clipboard (terminal clipboard unavailable)"

---

## Daemonization

### Unix (Linux/macOS)

Classic double-fork with `setsid()`:

```rust
fn daemonize() -> io::Result<()> {
    // First fork
    match unsafe { libc::fork() } {
        -1 => return Err(io::Error::last_os_error()),
        0 => {} // Child continues
        _ => std::process::exit(0), // Parent exits
    }

    // Create new session
    if unsafe { libc::setsid() } == -1 {
        return Err(io::Error::last_os_error());
    }

    // Second fork (prevent acquiring controlling terminal)
    match unsafe { libc::fork() } {
        -1 => return Err(io::Error::last_os_error()),
        0 => {} // Child continues
        _ => std::process::exit(0), // Parent exits
    }

    // Redirect stdio to /dev/null
    let devnull = std::fs::File::open("/dev/null")?;
    unsafe {
        libc::dup2(devnull.as_raw_fd(), 0);
        libc::dup2(devnull.as_raw_fd(), 1);
        libc::dup2(devnull.as_raw_fd(), 2);
    }

    Ok(())
}
```

**Library alternative**: [`daemonize`](https://crates.io/crates/daemonize) crate

### Windows

Use `CreateProcess` with `DETACHED_PROCESS`:

```rust
fn spawn_detached_server() -> io::Result<()> {
    use std::os::windows::process::CommandExt;

    const DETACHED_PROCESS: u32 = 0x00000008;
    const CREATE_NEW_PROCESS_GROUP: u32 = 0x00000200;

    std::process::Command::new(std::env::current_exe()?)
        .arg("--server")
        .creation_flags(DETACHED_PROCESS | CREATE_NEW_PROCESS_GROUP)
        .spawn()?;

    Ok(())
}
```

### Windows Client: Console Mode Setup

Windows Console handling requires explicit setup in the client:

```rust
fn setup_windows_console() -> io::Result<()> {
    use windows_sys::Win32::System::Console::*;

    let stdin_handle = unsafe { GetStdHandle(STD_INPUT_HANDLE) };
    let stdout_handle = unsafe { GetStdHandle(STD_OUTPUT_HANDLE) };

    // 1. Enable Virtual Terminal Processing (ANSI support)
    let mut stdout_mode: u32 = 0;
    unsafe { GetConsoleMode(stdout_handle, &mut stdout_mode) };
    unsafe { SetConsoleMode(
        stdout_handle,
        stdout_mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING | DISABLE_NEWLINE_AUTO_RETURN
    ) };

    // 2. Set raw input mode
    let mut stdin_mode: u32 = 0;
    unsafe { GetConsoleMode(stdin_handle, &mut stdin_mode) };
    unsafe { SetConsoleMode(
        stdin_handle,
        ENABLE_VIRTUAL_TERMINAL_INPUT  // Parse ANSI input sequences
            & !ENABLE_LINE_INPUT       // No line buffering
            & !ENABLE_ECHO_INPUT       // No echo
            & !ENABLE_PROCESSED_INPUT  // Don't process Ctrl+C as signal
    ) };

    Ok(())
}
```

#### Critical: Ctrl+C Handling on Windows

On Windows, `Ctrl+C` is normally a **signal** that kills the process, not a byte (`0x03`) sent to stdin.

The client **must** intercept this:

```rust
fn setup_ctrl_c_handler(data_socket: Arc<Socket>) -> io::Result<()> {
    use windows_sys::Win32::System::Console::*;

    unsafe extern "system" fn handler(ctrl_type: u32) -> i32 {
        match ctrl_type {
            CTRL_C_EVENT | CTRL_BREAK_EVENT => {
                // Send Ctrl+C byte to server instead of dying
                // (Need to access socket via global or thread-local)
                DATA_SOCKET.write_all(&[0x03]).ok();
                1 // Handled, don't terminate
            }
            _ => 0 // Not handled
        }
    }

    unsafe { SetConsoleCtrlHandler(Some(handler), 1) };
    Ok(())
}
```

Alternatively, disabling `ENABLE_PROCESSED_INPUT` (as shown above) prevents Windows from treating Ctrl+C specially, and it arrives as a normal `0x03` byte.

#### Windows Server: ConPTY

The server uses ConPTY for terminal emulation (already handled by `portable-pty` crate). Key points:

- ConPTY is Windows 10 1809+ only (Fall 2018)
- Earlier Windows versions would need legacy console API (not recommended to support)
- Server doesn't need a console window since it's using ConPTY for integrated terminals

---

## Session Lifecycle

### Starting Fresh

```
$ fresh [files...]
    │
    ▼
Check for existing server for this working directory
    │
    ├─► Server exists? ──► Connect as client
    │
    └─► No server? ──► Spawn server daemon
                            │
                            ▼
                       Server ready
                            │
                            ▼
                       Connect as client
```

### Detaching

```
User presses Ctrl+B D (or closes terminal)
    │
    ▼
Client sends Detach message
    │
    ▼
Server acknowledges, removes client from list
    │
    ▼
Client exits cleanly
    │
    ▼
Server continues running (holds state, PTYs)
```

### Reattaching

```
$ fresh attach [-s session_name]
    │
    ▼
Discover server socket (by working directory or explicit name)
    │
    ▼
Connect to server
    │
    ▼
Send terminal size
    │
    ▼
Server sends full screen render
    │
    ▼
User is exactly where they left off
```

### Quitting

```
User presses Ctrl+Q (Quit command)
    │
    ▼
Server checks for unsaved changes
    │
    ├─► Unsaved? ──► Prompt save/discard/cancel
    │
    ▼
Server serializes session to disk
    │
    ▼
Server sends Quit message to all clients
    │
    ▼
Server exits
```

---

## PTY Ownership

The server owns all PTY masters. This is critical for shell continuity.

### Current Terminal Architecture

```rust
// src/app/terminal.rs (simplified)
pub struct TerminalState {
    term: Term<NullListener>,       // alacritty_terminal state
    parser: Processor,              // VTE parser
    // ...
}
```

### Required Changes

PTY creation moves to server:

```rust
// Server holds PTY masters
pub struct ServerTerminalManager {
    terminals: HashMap<TerminalId, OwnedTerminal>,
}

pub struct OwnedTerminal {
    pty_master: Box<dyn MasterPty + Send>,
    pty_reader: Box<dyn std::io::Read + Send>,
    child: Box<dyn Child + Send>,
    state: TerminalState,
}
```

When client detaches:
- PTY keeps running (server holds master)
- Output buffered in `TerminalState`
- Scrollback continues to backing file

When client reattaches:
- Server sends recent scrollback
- Live output resumes

---

## Session Discovery

How clients find servers:

### Socket Path Convention

```
Unix:  /run/user/$UID/fresh/$ENCODED_WORKDIR.sock
       $XDG_RUNTIME_DIR/fresh/$ENCODED_WORKDIR.sock  (fallback)

Windows: \\.\pipe\fresh-$ENCODED_WORKDIR
```

Where `$ENCODED_WORKDIR` uses the existing `encode_path_for_filename()` function from `workspace.rs`.

### Session Registry

For `fresh ls` and named sessions, maintain a registry file:

```
~/.local/share/fresh/sessions/registry.json
```

```json
{
  "sessions": {
    "home_user_myproject": {
      "socket_path": "/run/user/1000/fresh/home_user_myproject.sock",
      "working_dir": "/home/user/myproject",
      "name": null,
      "pid": 12345,
      "started_at": 1706300000,
      "last_client_at": 1706301000
    },
    "custom-name": {
      "socket_path": "/run/user/1000/fresh/custom-name.sock",
      "working_dir": "/home/user/project2",
      "name": "custom-name",
      "pid": 12346,
      "started_at": 1706302000,
      "last_client_at": 1706303000
    }
  }
}
```

### CLI Commands

```bash
# List sessions
$ fresh ls
  SESSION          WORKING DIRECTORY           STATUS
  myproject        /home/user/myproject        attached (1 client)
  custom-name      /home/user/project2         detached

# Attach to session (auto-detect by CWD or explicit)
$ fresh attach                    # Attach to session for CWD
$ fresh attach -s custom-name     # Attach to named session
$ fresh attach /path/to/project   # Attach to session by path

# Create named session
$ fresh -s my-session             # Start with session name

# Kill session
$ fresh kill custom-name
$ fresh kill --all
```

---

## Crash Recovery (Resurrection)

### Periodic Checkpointing

Server periodically saves state to disk (reusing existing `Session` serialization):

```rust
impl Server {
    fn checkpoint(&self) -> Result<()> {
        // Reuse existing session serialization
        let session = self.editor.to_session();
        session.save()?;

        // Additionally save:
        // - Dirty buffer contents (not just metadata)
        // - Terminal scrollback already in backing files

        Ok(())
    }
}
```

Checkpoint triggers:
- Every 30 seconds if dirty
- On every file save
- Before quit

### On Server Crash

Next `fresh` invocation for same working directory:
1. Detects no running server
2. Finds session file from previous run
3. Loads session (split layout, open files, cursor positions)
4. Opens files, marks restored unsaved buffers as dirty
5. Terminal states: shows scrollback in scrollback mode (processes lost)

### What Survives vs What's Lost

| Survives | Lost |
|----------|------|
| Split layout | Running terminal processes |
| Open files | LSP state (reconnects) |
| Unsaved changes (if checkpointed) | Undo history (optional: save EventLog) |
| Cursor positions | Plugin runtime state |
| Search history | Active completions |
| Bookmarks | |

---

## Server Lifecycle Management

### The Zombie Problem

Users accumulate orphaned servers over time:
- Close laptop, SSH dies, client dies, server keeps running
- Detach and forget about a session
- After 6 months: 50 zombie servers consuming 500MB+ RAM

### Server Idle Timeout (TTL)

Servers should auto-terminate after prolonged inactivity:

```rust
// Server configuration
struct ServerConfig {
    /// Shutdown after no clients connected for this duration
    /// Default: 24 hours. Set to 0 to disable.
    idle_timeout: Duration,

    /// Checkpoint interval when idle (before shutdown)
    checkpoint_before_shutdown: bool,
}

// Server idle tracking
impl Server {
    fn on_client_disconnect(&mut self) {
        if self.clients.is_empty() {
            self.idle_since = Some(Instant::now());
        }
    }

    fn on_client_connect(&mut self) {
        self.idle_since = None;
    }

    fn check_idle_timeout(&mut self) -> bool {
        if let Some(idle_since) = self.idle_since {
            if idle_since.elapsed() > self.config.idle_timeout {
                // Checkpoint and exit
                self.checkpoint()?;
                return true; // Signal shutdown
            }
        }
        false
    }
}
```

### Configuration

```json
// ~/.config/fresh/config.json
{
  "server": {
    "idle_timeout_hours": 24,
    "checkpoint_on_idle_shutdown": true
  }
}
```

### Behavior

1. Last client disconnects → start idle timer
2. New client connects → cancel timer
3. Timer expires (default 24h) → checkpoint session to disk → `exit(0)`
4. Next `fresh` invocation → resurrects from disk checkpoint

This balances convenience (don't lose sessions) with resource management (don't hoard RAM forever).

### Manual Cleanup

```bash
# List all sessions with idle time
$ fresh ls
  SESSION          WORKING DIRECTORY           IDLE
  myproject        /home/user/myproject        attached
  old-project      /home/user/old              3 days
  forgotten        /home/user/ancient          2 weeks

# Kill idle sessions
$ fresh kill --idle 7d     # Kill sessions idle > 7 days
$ fresh kill --all         # Kill all sessions
```

---

## Implementation Status

**Current Status**: MVP functional on Unix (Linux/macOS)

### What Works
- Editor state (buffers, cursor positions, unsaved changes) persists across detach/reattach
- Server runs as daemon, survives terminal close
- CLI commands: `--attach`, `--list-sessions`, `--kill`, `--server`, `--session-name`
- Auto-start server when attaching to non-existent session
- 44 unit and integration tests

### Known Limitations
- Windows named pipes not implemented (stubs exist)
- OSC 52 clipboard not yet implemented
- Terminal panes (PTY) persistence needs testing/verification

### Files Created
- `crates/fresh-editor/src/server/mod.rs` - Server module exports
- `crates/fresh-editor/src/server/ipc.rs` - Unix socket IPC
- `crates/fresh-editor/src/server/protocol.rs` - JSON protocol messages
- `crates/fresh-editor/src/server/daemon.rs` - Daemonization
- `crates/fresh-editor/src/server/runner.rs` - Basic server runner
- `crates/fresh-editor/src/server/editor_server.rs` - Full editor integration
- `crates/fresh-editor/src/server/input_parser.rs` - Server-side input parsing
- `crates/fresh-editor/src/server/capture_backend.rs` - Headless ratatui backend
- `crates/fresh-editor/src/server/tests.rs` - Integration tests
- `crates/fresh-editor/src/client.rs` - Ultra-light client (~290 lines)

---

## Implementation Phases

### Phase 1: IPC Infrastructure ✅ COMPLETE

**Goal**: Basic client-server communication working

Deliverables:
- [x] Unix socket listener/connector (data + control sockets)
- [ ] Windows named pipe listener/connector
- [x] Control channel JSON protocol
- [x] Handshake with version check and environment negotiation
- [x] Heartbeat ping/pong

### Phase 2: Ultra-Light Client ✅ COMPLETE

**Goal**: Minimal client that relays bytes

The client is ~290 lines including Windows stubs and SIGWINCH handling.

Deliverables:
- [x] Dual socket connection (data + control)
- [x] Handshake with version check, env vars
- [x] Raw mode setup/teardown
- [x] Bidirectional byte relay on data socket
- [x] Control socket handler (resize, ping/pong)
- [x] Signal handlers (SIGWINCH → control msg, SIGTERM → graceful exit)
- [ ] Windows: Console mode setup, Ctrl+C handling

### Phase 3: Server-Side Input & Rendering ✅ COMPLETE

**Goal**: Server handles all parsing and rendering

Deliverables:
- [x] `CaptureBackend` for ratatui
- [x] Server-side input byte parser (`InputParser`)
- [x] Terminal setup sequence generation (alt screen, mouse, etc.)
- [x] Resize handling (re-render on size change)

### Phase 4: Daemonization ✅ COMPLETE

**Goal**: Server runs in background, survives terminal close

Deliverables:
- [x] Unix daemonization (double-fork with setsid)
- [x] Windows detached process (DETACHED_PROCESS flag)
- [x] Socket cleanup on exit
- [x] Graceful shutdown

### Phase 5: PTY Migration 🔄 PARTIAL

**Goal**: Terminal panes survive detach

The server owns the Editor which owns TerminalManager, so PTYs are server-side.
Needs testing to verify terminal panes actually survive detach/reattach.

Deliverables:
- [x] Server-owned PTY manager (via Editor → TerminalManager)
- [x] Terminal output streaming (via normal render path)
- [ ] Reattach scrollback sync (needs testing)
- [x] Terminal resize propagation

### Phase 6: Session Management CLI ✅ COMPLETE

**Goal**: Full session control

Deliverables:
- [x] `fresh --list-sessions` - list sessions
- [x] `fresh --attach` - attach to session
- [x] `fresh --kill` - terminate session
- [ ] `fresh --kill --idle 7d` - kill stale sessions
- [x] Named sessions (`--session-name` flag)
- [ ] Session registry file
- [x] Server idle timeout (configurable, default 1 hour)
- [ ] Checkpoint-before-idle-shutdown

### Phase 7: Clipboard (OSC 52) ⏳ NOT STARTED

**Goal**: System clipboard works through daemon

Changes:
- Server generates OSC 52 sequences on yank
- Test with major terminals (Alacritty, iTerm2, WezTerm, Windows Terminal)
- Fallback notification when clipboard unavailable

Deliverables:
- [ ] OSC 52 generation for yank
- [ ] Bracketed paste handling (already exists, verify works)
- [ ] Status message for clipboard operations
- [ ] Documentation of terminal requirements

### Phase 8: Multi-Client Support (Post-MVP)

**Goal**: Multiple terminals viewing same session

Changes:
- Track multiple client connections
- Broadcast renders to all clients
- Handle different terminal sizes (use smallest, or per-client render)
- Client identification

---

## File Structure

```
crates/fresh-editor/src/
├── main.rs                      # Entry point, mode dispatch
├── server/
│   ├── mod.rs                   # Server entry, main loop
│   ├── listener.rs              # IPC listener (Unix socket / named pipe)
│   ├── input_parser.rs          # Parse raw bytes → KeyEvent/MouseEvent
│   ├── renderer.rs              # Capturing backend for ratatui
│   ├── client_manager.rs        # Track connected clients
│   └── daemon.rs                # Daemonization logic
├── client.rs                    # Ultra-light client (~100 lines)
│                                # - connect, raw mode, relay, restore
├── session/
│   ├── mod.rs                   # Session types (move from workspace.rs)
│   ├── registry.rs              # Session discovery registry
│   └── persistence.rs           # Checkpoint/restore logic
└── ...existing files...
```

Note: The client is intentionally a single file. If it grows beyond ~150 lines, we're adding too much complexity.

---

## Dependencies

### New Dependencies

```toml
[dependencies]
# IPC (Unix sockets + Windows named pipes)
interprocess = "2"

# Optional: daemonization helper (Unix only)
daemonize = "0.5"
```

Note: With the ultra-light client approach, we do **not** need:
- `bincode` - No structured serialization, just raw bytes
- Additional client-side dependencies

### Existing Dependencies (Already Present)

**Server uses (same as current Fresh):**
- `ratatui` - TUI framework (server captures output)
- `crossterm` - Input parsing (server-side now), terminal sequences
- `portable-pty` - PTY management
- `alacritty_terminal` - Terminal emulation
- `serde` - Session serialization
- `tokio` - Async runtime

**Client uses (minimal):**
- `crossterm` - Only for `enable_raw_mode()` / `disable_raw_mode()`
- `interprocess` - Socket connection
- Standard library for everything else

The client could even drop the `crossterm` dependency and use raw `tcsetattr`/`SetConsoleMode` calls (~20 lines of platform-specific code) for an even lighter footprint.

---

## Alternatives Considered

### 1. Keep Single Process, Only Improve Session Restore

**Pros**: Simpler, no IPC complexity
**Cons**: Can't survive terminal close, no shell continuity
**Decision**: Rejected - doesn't meet core requirements

### 2. Use Existing Multiplexer (tmux/zellij integration)

**Pros**: Battle-tested, already handles all the hard stuff
**Cons**: External dependency, can't deeply integrate, poor Windows support
**Decision**: Rejected - want native experience

### 3. WebSocket-based Protocol

**Pros**: Could enable remote access, web client
**Cons**: Overhead, complexity, security concerns
**Decision**: Rejected for MVP - can add later if needed

### 4. Shared Memory + Semaphores

**Pros**: Very fast IPC
**Cons**: Complex, error-prone, platform-specific
**Decision**: Rejected - sockets/pipes are fast enough, much simpler

### 5. Structured Client Protocol (serde + bincode)

Original design had client parsing input into `KeyEvent`/`MouseEvent` and server sending structured render operations.

**Pros**: Type-safe, explicit message boundaries, could support different terminal sizes per client
**Cons**: More client complexity, serialization overhead, more failure modes
**Decision**: Rejected in favor of ultra-light client - raw byte relay is simpler and sufficient

### 6. Client-Side Rendering with State Sync

Client receives editor state updates and renders locally.

**Pros**: Lower bandwidth (state changes vs pixels), client can render at different sizes
**Cons**: Duplicates rendering logic, state sync is hard, client becomes complex
**Decision**: Rejected - rendering once on server is simpler and correct

---

## Security Considerations

### Socket Permissions

Unix sockets inherit directory permissions:
```rust
// Create socket in user-only directory
let socket_dir = format!("/run/user/{}/fresh", users::get_current_uid());
std::fs::create_dir_all(&socket_dir)?;
std::fs::set_permissions(&socket_dir, Permissions::from_mode(0o700))?;
```

Windows named pipes use security descriptors (default is current user only).

### No Authentication in MVP

For MVP, assume:
- Socket is protected by filesystem permissions
- Only local connections
- Single user

Future: Add authentication token for multi-user/remote scenarios.

---

## Testing Strategy

### Unit Tests

- Protocol serialization round-trip
- Session registry operations
- Path encoding (reuse existing tests)

### Integration Tests

- Server spawn + client connect
- Input forwarding
- Render streaming
- Detach/reattach cycle
- Crash recovery

### Manual Testing Scenarios

1. Basic workflow: edit, detach, reattach, verify state
2. Terminal continuity: start `sleep 100`, detach, reattach, verify running
3. Crash recovery: kill -9 server, restart, verify restoration
4. Multi-client: two terminals attached, verify sync
5. Long-running: leave detached overnight, reattach

---

## Performance Considerations

### Render Efficiency

- Only send diffs, not full frames
- Batch small updates (coalesce rapid changes)
- Consider compression for large diffs (unlikely to be needed)

### Input Latency

- Direct socket write, no buffering
- Target: <5ms additional latency

### Memory

- Server holds all buffer state (same as current)
- Client is minimal (few KB)

### Benchmarks to Track

- Input-to-display latency
- Memory usage (server vs current single process)
- Startup time (client connecting to existing server)

---

## Open Questions

1. **Undo history persistence**: Should `EventLog` be serialized for full undo after crash? (Increases checkpoint size significantly)

2. **Remote access**: Should we design IPC to support future TCP tunneling for remote editing? (The dual-socket design could work over TCP with minor changes)

3. **Session naming**: Auto-generate vs require explicit names?

4. ~~**Orphan cleanup**: How long before killing serverless sessions?~~ **Resolved**: Default 24h idle timeout with configurable TTL.

5. **Multiple working directories**: One server per directory, or single server with multiple workspaces?

6. **Multi-client terminal sizes**: When two clients with different terminal sizes attach, which size wins? Options:
   - Use smallest common size
   - Primary client (first attached) wins
   - Per-client rendering (complex)

7. **OSC 52 failure detection**: How do we know if the terminal doesn't support clipboard? Silent failure is confusing. Consider:
   - Test on connect with a canary sequence
   - User setting to disable/enable
   - Status message on yank

8. **Heartbeat interval**: What's the right ping frequency for detecting dead connections without wasting bandwidth?

---

## References

- [tmux architecture](https://github.com/tmux/tmux/wiki/Getting-Started)
- [Zellij session management](https://zellij.dev/documentation/session-management)
- [WezTerm mux](https://wezfurlong.org/wezterm/multiplexing.html)
- [interprocess crate](https://docs.rs/interprocess/latest/interprocess/)
- [portable-pty crate](https://docs.rs/portable-pty/latest/portable_pty/)
- [OSC 52 specification](https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h3-Operating-System-Commands)
- Existing Fresh docs:
  - `docs/internal/terminal.md` - Terminal architecture
  - `docs/internal/SSH_REMOTE_EDITING_DESIGN.md` - Remote editing (similar IPC patterns)
  - `src/workspace.rs` - Workspace serialization

---

## Design Review Notes

Key architectural decisions made during design review:

### IPC Protocol
- **Decision**: Dual-socket (data + control) instead of byte-stuffed single socket
- **Rationale**: Keeps hot path pure, avoids scanning every byte for escape sequences, easier debugging

### Environment Negotiation
- **Decision**: Client sends `TERM`, `COLORTERM`, `LANG` in handshake
- **Rationale**: Server is detached, has no terminal context. Must render for client's actual terminal capabilities.

### Version Checking
- **Decision**: Mandatory protocol version in handshake, graceful mismatch handling
- **Rationale**: Prevents subtle bugs when user upgrades binary but old server still running

### Clipboard
- **Decision**: OSC 52 as primary clipboard mechanism
- **Rationale**: Server is detached from GUI session, can't use system clipboard APIs. OSC 52 works through the relay.

### Windows Console
- **Decision**: Explicit `ENABLE_VIRTUAL_TERMINAL_PROCESSING` and Ctrl+C signal handling
- **Rationale**: Windows Console behavior differs fundamentally from Unix terminals. Must handle explicitly.

### Server Lifecycle
- **Decision**: 24-hour default idle timeout with checkpoint-before-exit
- **Rationale**: Prevents zombie server accumulation while preserving user data
