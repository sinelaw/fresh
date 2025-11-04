# LSP Integration Architecture

## Overview

Language Server Protocol (LSP) support enables IDE-like features:
- Code completion (autocomplete)
- Diagnostics (errors, warnings)
- Go-to-definition
- Hover information
- Find references
- Code actions (quick fixes)
- Document formatting

## LSP in the Wild: Learning from Established Implementations

### Vim/Neovim LSP Clients

**coc.nvim** (Conquer of Completion)
- Architecture: Node.js-based extension host, similar to VSCode
- Configuration: JSON-based, mirrors VSCode extension configuration
- Features: Full LSP support + off-spec functionality for each server
- Philosophy: "Headless VSCode" - maximum compatibility with VSCode ecosystem

**Neovim Native LSP** (v0.5.0+)
- Architecture: Built-in Lua-based LSP client
- Configuration: Via nvim-lspconfig plugin
- Key Concept: `root_dir` determines workspace root for language server
- Philosophy: Minimal core + plugin ecosystem for features
- Debouncing: Built-in 150ms debounce for didChange notifications (`debounce_text_changes`)

**vim-lsp**
- Architecture: Async LSP protocol plugin
- Philosophy: Asynchronous operation to avoid blocking editor

### Emacs LSP Clients

**Eglot** (Built-in to Emacs 29+)
- Architecture: Lightweight, integrates with built-in Emacs tools
- Integration: Uses Flymake for diagnostics, xref for navigation, eldoc for hover
- Philosophy: "Stays out of your way" - minimal configuration
- Design: Translates between Emacs internal representations and LSP protocol
- Key Advantage: No external dependencies beyond Emacs

**lsp-mode**
- Architecture: Feature-rich, separate sub-packages per language server
- Integration: Optional integration with company, flycheck, projectile
- Features: Bespoke UI, dap-mode integration, multiple servers per file
- Philosophy: IDE-like experience with comprehensive LSP spec support
- Configuration: Per-server quirks handling

### VSCode LSP Architecture

**Core Components:**
- `vscode-languageclient`: npm module for VSCode extensions to communicate with LSP servers
- `vscode-languageserver`: npm module for implementing LSP servers in Node.js
- `vscode-languageserver-protocol`: TypeScript definition of LSP protocol
- `vscode-jsonrpc`: Underlying message protocol

**Architecture:**
- Language Client runs in Node.js Extension Host context
- Language Servers run in separate process (any language)
- Communication: IPC or sockets via vscode-languageclient
- Philosophy: Language-agnostic base layer, servers avoid performance cost via separate processes

## Critical Implementation Lessons

### 1. Position Encoding (UTF-8 vs UTF-16)

**The Problem:**
- LSP mandates line/column pairs where "column" is an index into **UTF-16-encoded** text
- Text contents are transmitted in **UTF-8**
- Most modern editors (Rust, Go) store strings in UTF-8 internally
- Example: In `a𐐀b`, character offset of `𐐀` is 1, but offset of `b` is 3 in UTF-16 (𐐀 uses 2 code units)

**LSP 3.17+ Solution:**
- Client announces supported encodings via `general.positionEncodings` capability
- Three encoding kinds:
  - `UTF-8`: Character offsets count UTF-8 code units (bytes) - preferred for Rust
  - `UTF-16`: Character offsets count UTF-16 code units - default, must be supported
  - `UTF-32`: Character offsets count UTF-32 code units (Unicode code points)

**Implementation Strategy:**
- Keep two positions per source location:
  - UTF-8 byte position (for indexing Rust `str` and `[u8]`)
  - UTF-16 code unit position (for LSP protocol)
- Use `lsp-positions` crate or similar for conversion utilities
- Always negotiate UTF-8 encoding in initialize if server supports it

### 2. Diagnostics Lifecycle

**Server Responsibilities:**
- When file is updated, server MUST re-compute and push diagnostics to client
- Even if diagnostics are unchanged, server must push them (to confirm they're current)
- Empty diagnostic array clears previous diagnostics
- **No merging on client side** - new diagnostics always replace old ones completely

**Client Handling:**
- Diagnostics arrive asynchronously via `textDocument/publishDiagnostics` notification
- Can arrive at any time, not just after didChange/didSave
- Client has **no control** over when diagnostics are sent
- Must handle out-of-order notifications gracefully

**Best Practice:**
- Store diagnostics by URI, replace entire diagnostic set per file
- Clear diagnostics when file is closed
- Display diagnostics via UI primitives (overlays for underlines, popups for details)

### 3. Async vs Sync Notification Handling

**Protocol Semantics:**
- **Requests**: Can be processed concurrently (async)
- **Notifications**: MUST be processed in order (sync)
- Notifications change state and affect semantics of later requests/notifications

**Common Anti-Pattern (tower-lsp):**
- Handles notifications asynchronously → out-of-order issues
- Example: `didChange` notification processed after `completion` request

**Correct Pattern (async-lsp):**
- Execute notification handlers synchronously
- Maintain main loop control for exit/error conditions
- Allow async processing of requests (completion, hover, etc.)

### 4. Performance: Debouncing & Throttling

**The Problem:**
- Typing generates rapid `didChange` notifications
- Each notification triggers expensive re-analysis
- Too many requests can overwhelm language server

**Solutions:**

**Debouncing** (wait before sending):
- Neovim: 150ms default debounce for didChange
- Emacs lsp-mode: Configurable debounce for full-sync servers
- Strategy: Wait for user to pause typing before notifying server

**Throttling** (limit rate):
- Ensure function executes at most once per time period
- Useful for completion requests during continuous typing

**Best Practice:**
- Debounce didChange notifications (100-150ms typical)
- Throttle completion requests (triggered by special characters)
- Always allow immediate notification on file save
- Make debounce interval configurable

### 5. Text Synchronization Strategies

**Full Document Sync:**
- Send entire file content on every change
- Simple to implement, works well for small files
- Current implementation: `TextDocumentContentChangeEvent { range: None, text: full_content }`

**Incremental Sync:**
- Send only changed ranges
- Better performance for large files
- More complex: requires accurate position tracking
- Must handle multi-cursor edits atomically

**Recommendation:**
- Start with full sync (simpler, sufficient for most cases)
- Add incremental sync later if profiling shows it's needed
- Measure before optimizing - full sync is often fast enough

### 6. Workspace Root Detection

**Critical for LSP:**
- `root_dir` (Neovim) or `rootUri` (LSP) determines workspace context
- Affects import resolution, symbol search, etc.
- Usually: nearest directory with `.git`, `Cargo.toml`, `package.json`, etc.

**Implementation:**
- Walk up directory tree from file path
- Look for language-specific markers
- Fall back to file's parent directory
- Cache per-workspace to avoid repeated lookups

## Our Architecture

We use a fully asynchronous, multi-threaded architecture with separate tasks for reading and writing:

```
┌──────────────────────────────────────────────────────────────────────┐
│                        Editor (main.rs)                              │
│  - Manages LSP lifecycle                                             │
│  - Routes events to LSP via LspManager                               │
│  - Displays LSP results (diagnostics)                                │
│  - Processes async messages from AsyncBridge                         │
└───────────────────────────┬──────────────────────────────────────────┘
                            │
                            ↓
┌──────────────────────────────────────────────────────────────────────┐
│                   LspManager (lsp_manager.rs)                        │
│  - Manages multiple language servers (one per language)             │
│  - Routes commands to appropriate LspHandle                          │
│  - Handles language detection                                        │
└───────────────────────────┬──────────────────────────────────────────┘
                            │
                            ↓
┌──────────────────────────────────────────────────────────────────────┐
│                      LspHandle (lsp_async.rs)                        │
│  - Public API for sending commands to LSP server                     │
│  - Non-blocking: sends commands via mpsc channel                     │
│  - Methods: initialize(), did_open(), did_change(), shutdown()       │
└───────────────────────────┬──────────────────────────────────────────┘
                            │ mpsc::channel (commands)
                            ↓
┌──────────────────────────────────────────────────────────────────────┐
│                       LspTask (lsp_async.rs)                         │
│  Runs in dedicated Tokio task with two independent subtasks:        │
│                                                                      │
│  ┌────────────────────────────────────────────────────────────┐    │
│  │  Command Processing Loop (Sequential)                      │    │
│  │  - Receives commands from mpsc channel                     │    │
│  │  - Queues commands until initialization completes          │    │
│  │  - Processes Initialize, DidOpen, DidChange sequentially   │    │
│  │  - Uses LspState for mutable state                         │    │
│  └────────────────────────────────────────────────────────────┘    │
│                                                                      │
│  ┌────────────────────────────────────────────────────────────┐    │
│  │  Stdout Reader Task (Continuous)                           │    │
│  │  - Continuously reads from language server stdout          │    │
│  │  - Dispatches responses to pending requests via HashMap    │    │
│  │  - Forwards notifications to main loop via std_mpsc        │    │
│  └────────────────────────────────────────────────────────────┘    │
│                                                                      │
│  Shared State:                                                       │
│  - Arc<Mutex<HashMap<i64, oneshot::Sender>>> for pending requests   │
└───────────────────────────┬──────────────────────────────────────────┘
                            │ stdin/stdout pipes
                            ↓
┌──────────────────────────────────────────────────────────────────────┐
│                  Language Servers (external)                         │
│  - rust-analyzer (Rust)                                              │
│  - typescript-language-server (TypeScript)                           │
│  - pyright (Python)                                                  │
└──────────────────────────────────────────────────────────────────────┘
```

### Key Architectural Decisions

**Two-Task Design**: Separate stdout reader from command processor to avoid deadlocks
- **Stdout Reader**: Runs continuously in background, reads responses and notifications
- **Command Processor**: Sequential processing, waits for Initialize before sending file notifications

**Command Queueing**: Commands sent before initialization are queued and replayed after success
- DidOpen and DidChange queue automatically if not initialized
- After Initialize succeeds, all pending commands are replayed in order

**Async Bridge**: std_mpsc channel bridges Tokio async world to main event loop
- Notifications (diagnostics) flow from LspTask → AsyncBridge → Editor
- Editor polls AsyncBridge during main event loop

**Request/Response Matching**: Shared HashMap tracks pending requests
- Command processor inserts oneshot::Sender before sending request
- Stdout reader looks up and sends response through oneshot channel
- Timeout handled with tokio::time::timeout

## Core Components

### 1. LspHandle (lsp_async.rs) - Public API

**Purpose**: Provides a non-blocking interface for the editor to communicate with LSP servers.

**Key Responsibilities**:
- Sends commands to LspTask via mpsc channel (non-blocking)
- Provides public methods: `initialize()`, `did_open()`, `did_change()`, `shutdown()`
- Tracks initialization status for status checks

**Design**: Commands are sent asynchronously and never block the editor UI.

### 2. LspTask (lsp_async.rs) - Async Worker

**Purpose**: Manages the actual LSP server process and communication.

**Architecture**: Runs in a dedicated Tokio task with two independent subtasks:

**Subtask 1 - Command Processing Loop (Sequential)**:
- Receives commands from mpsc channel
- Queues DidOpen/DidChange commands until initialization completes
- Processes Initialize first, then replays queued commands
- Uses LspState for all mutable state

**Subtask 2 - Stdout Reader Task (Continuous)**:
- Continuously reads from language server stdout in background
- Dispatches responses to pending requests via shared HashMap
- Forwards notifications (diagnostics) to main loop via AsyncBridge
- Never blocks command processing

**Shared State**: Arc<Mutex<HashMap>> tracks pending requests between the two tasks.

### 3. LspState (lsp_async.rs) - Mutable State

**Purpose**: Encapsulates all mutable state needed for LSP communication.

**Key Responsibilities**:
- Manages stdin writer for sending messages
- Tracks request IDs and document versions
- Stores server capabilities from initialization
- Handles all protocol-level message construction

**Design**: Extracted from LspTask to solve Rust ownership issues with split tasks.

### 4. LspManager (lsp_manager.rs) - Multi-Language Coordinator

**Purpose**: Manages multiple language servers (one per language).

**Key Responsibilities**:
- Routes commands to appropriate LspHandle based on language
- Handles language detection from file extensions
- Spawns new language servers on demand
- Coordinates shutdown of all servers

**Design**: Single manager, multiple handles, each managing one language server process.

### 5. Integration with Editor

**AsyncBridge**: Bridges the async Tokio world with the synchronous main event loop.
- LSP notifications flow through std_mpsc channel
- Editor polls during main event loop via `process_async_messages()`
- Diagnostics are received and stored per file URI

**File Event Flow**:
1. Editor opens file → LspManager detects language → spawns/gets LspHandle
2. Editor calls `lsp.did_open()` → queued if not initialized
3. LspTask processes Initialize → replays queued didOpen
4. Diagnostics arrive asynchronously → displayed in editor

**Text Change Flow** (future):
1. User types → Editor applies change to buffer
2. Editor calls `lsp.did_change()` with full document content
3. LspTask sends notification to language server
4. Diagnostics arrive asynchronously → updated in UI

## LSP Message Protocol

LSP uses JSON-RPC 2.0 over stdin/stdout:

### Request Format
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "textDocument/completion",
  "params": {
    "textDocument": {
      "uri": "file:///path/to/file.rs"
    },
    "position": {
      "line": 10,
      "character": 5
    }
  }
}
```

### Response Format
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "items": [
      {
        "label": "println!",
        "kind": 3,
        "detail": "macro",
        "documentation": "Prints to stdout"
      }
    ]
  }
}
```

### Notification Format (no response expected)
```json
{
  "jsonrpc": "2.0",
  "method": "textDocument/didChange",
  "params": {
    "textDocument": {
      "uri": "file:///path/to/file.rs",
      "version": 2
    },
    "contentChanges": [
      {
        "range": {
          "start": { "line": 10, "character": 5 },
          "end": { "line": 10, "character": 5 }
        },
        "text": "hello"
      }
    ]
  }
}
```

## Rendering LSP Features

### 1. Completion Popup

```
┌─────────────────────────────┐
│ > println!           macro  │
│   format!            macro  │
│   vec!               macro  │
│   String::new        fn     │
│   String::from       fn     │
└─────────────────────────────┘
```

Render below cursor, scrollable list with fuzzy filtering.

### 2. Diagnostics (Inline)

```rust
let x = "hello"  // Error: expected `;`
        ^^^^^^^
```

Show errors/warnings inline with squiggly underlines.

### 3. Diagnostics (Panel)

```
┌─ PROBLEMS ─────────────────────┐
│ ⚠ Warning: unused variable `x` │
│   src/main.rs:10:9             │
│                                │
│ ❌ Error: expected `;`         │
│   src/main.rs:12:16            │
└────────────────────────────────┘
```

Optional panel showing all diagnostics.

### 4. Hover Information

```
┌─────────────────────────────┐
│ fn println!(...)            │
│                             │
│ Prints to the standard      │
│ output with a newline.      │
└─────────────────────────────┘
```

Show on Ctrl+hover or keybinding.

## Configuration

In `config.json`:

```json
{
  "lsp": {
    "rust": {
      "command": "rust-analyzer",
      "args": [],
      "enabled": true
    },
    "typescript": {
      "command": "typescript-language-server",
      "args": ["--stdio"],
      "enabled": true
    },
    "python": {
      "command": "pyright-langserver",
      "args": ["--stdio"],
      "enabled": true
    }
  },
  "editor": {
    "completion_trigger_characters": [".", ":", ">"],
    "show_diagnostics": true,
    "diagnostic_delay_ms": 500
  }
}
```

## Implementation Strategy

### Phase 1: Basic LSP (2-3 days)
- [ ] Implement LspClient with JSON-RPC protocol
- [ ] Spawn and initialize language servers
- [ ] Send `textDocument/didOpen` on file open
- [ ] Send `textDocument/didChange` on edits (full sync)
- [ ] Request and display diagnostics
- [ ] Show diagnostics inline (basic)

**Milestone**: See rust-analyzer errors inline

### Phase 2: Completion (1 day)
- [ ] Request completion on trigger character
- [ ] Render completion popup
- [ ] Navigate completion list with arrow keys
- [ ] Insert selected completion
- [ ] Fuzzy filter completions as user types

**Milestone**: Working autocomplete

### Phase 3: Advanced Features (1-2 days)
- [ ] Implement go-to-definition (Ctrl+click or keybinding)
- [ ] Implement hover (show on keybinding)
- [ ] Incremental sync (send only changed ranges)
- [ ] Multiple language server support
- [ ] LSP Manager to coordinate servers

**Milestone**: Full IDE-like experience

### Phase 4: Polish (ongoing)
- [ ] Code actions (quick fixes)
- [ ] Find references
- [ ] Document formatting
- [ ] Signature help (function parameters)
- [ ] Diagnostics panel
- [ ] Performance optimization (debounce requests)

## Key Design Decisions

### Why spawn per-language, not per-file?
- **Efficiency**: One rust-analyzer handles all Rust files
- **Speed**: Server caches information across files
- **Correctness**: Server understands project-wide context

### Why incremental sync?
- **Performance**: Only send changed text, not entire file
- **Scale**: Works with large files (GB+)
- **Latency**: Minimal network overhead

### Why async LSP communication?
- **Responsiveness**: Don't block editor on LSP requests
- **Multiple requests**: Can have completion + diagnostics in flight
- **Timeout**: Can cancel slow requests

## Dependencies

Add to `Cargo.toml`:
```toml
[dependencies]
serde_json = "1.0"      # Already added
lsp-types = "0.95"      # LSP type definitions
jsonrpc-core = "18.0"   # JSON-RPC protocol
```

## Error Handling

### LSP Server Crashes
- Detect server exit
- Show notification to user
- Offer to restart server
- Gracefully degrade (no LSP features but editor still works)

### Request Timeouts
- Set timeout for requests (5 seconds default)
- Cancel timed-out requests
- Continue editing without blocking

### Invalid Responses
- Log error
- Continue without crashing
- Show warning to user

## Testing Strategy

### Unit Tests
- JSON-RPC protocol encoding/decoding
- Position conversion (byte offset ↔ line/col)
- Event → LSP change conversion

### Integration Tests
- Spawn mock LSP server
- Send initialize request
- Send did_open notification
- Request completion, verify response
- Send did_change, verify diagnostics update

### Manual Testing
- Test with rust-analyzer (Rust files)
- Test with typescript-language-server (TS files)
- Test with pyright (Python files)
- Test error recovery (kill server mid-session)

## Future Enhancements

- Inlay hints (type annotations)
- Semantic tokens (advanced highlighting)
- Code lens (inline actions)
- Workspace symbols (search across project)
- Rename refactoring
- Call hierarchy
