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

## Architecture

```
┌─────────────────────────────────────────┐
│           Editor (main.rs)              │
│  - Manages LSP lifecycle                │
│  - Routes events to LSP                 │
│  - Displays LSP results                 │
└──────────┬──────────────────────────────┘
           │
           ↓
┌─────────────────────────────────────────┐
│         LSP Client (lsp.rs)             │
│  - Spawn language servers               │
│  - Send LSP requests                    │
│  - Receive LSP notifications            │
│  - Manage server lifecycle              │
└──────────┬──────────────────────────────┘
           │
           ↓
┌─────────────────────────────────────────┐
│     Language Servers (external)         │
│  - rust-analyzer                        │
│  - typescript-language-server           │
│  - pyright                              │
│  - etc.                                 │
└─────────────────────────────────────────┘
```

## Core Components

### 1. LSP Client (lsp.rs)

```rust
pub struct LspClient {
    /// Process handle for the language server
    process: Child,

    /// Stdin writer for sending requests
    stdin: BufWriter<ChildStdin>,

    /// Stdout reader for receiving responses
    stdout: BufReader<ChildStdout>,

    /// Next request ID
    next_id: i64,

    /// Pending requests waiting for response
    pending: HashMap<i64, PendingRequest>,

    /// Server capabilities
    capabilities: ServerCapabilities,

    /// Current document versions (for incremental sync)
    document_versions: HashMap<PathBuf, i64>,
}

impl LspClient {
    /// Spawn a language server
    pub fn spawn(command: &str, args: &[String]) -> Result<Self>;

    /// Initialize the language server
    pub fn initialize(&mut self, root_uri: &str) -> Result<()>;

    /// Notify server of document open
    pub fn did_open(&mut self, uri: &str, text: &str, language_id: &str);

    /// Notify server of document change (incremental)
    pub fn did_change(&mut self, uri: &str, changes: Vec<TextDocumentContentChangeEvent>);

    /// Request completion at position
    pub fn completion(&mut self, uri: &str, line: u32, character: u32)
        -> Result<CompletionResponse>;

    /// Request hover information
    pub fn hover(&mut self, uri: &str, line: u32, character: u32)
        -> Result<Option<Hover>>;

    /// Request goto definition
    pub fn definition(&mut self, uri: &str, line: u32, character: u32)
        -> Result<Vec<Location>>;

    /// Request diagnostics (errors/warnings)
    pub fn diagnostics(&self, uri: &str) -> Vec<Diagnostic>;

    /// Shutdown and exit
    pub fn shutdown(&mut self) -> Result<()>;
}
```

### 2. LSP Manager (manages multiple language servers)

```rust
pub struct LspManager {
    /// Map from language ID to LSP client
    clients: HashMap<String, LspClient>,

    /// Config for server commands
    config: HashMap<String, LspServerConfig>,
}

struct LspServerConfig {
    /// Command to spawn the server
    command: String,

    /// Arguments
    args: Vec<String>,

    /// Languages this server handles
    languages: Vec<String>,
}

impl LspManager {
    /// Get or spawn LSP client for language
    pub fn get_or_spawn(&mut self, language: &str) -> Option<&mut LspClient>;

    /// Shutdown all servers
    pub fn shutdown_all(&mut self);
}
```

### 3. Integration with Editor

```rust
pub struct Editor {
    /// Current buffer states
    buffers: HashMap<usize, EditorState>,

    /// LSP manager
    lsp: LspManager,

    /// Completion popup state
    completion: Option<CompletionPopup>,

    /// Diagnostics per buffer
    diagnostics: HashMap<usize, Vec<Diagnostic>>,

    // ... other fields
}

impl Editor {
    /// Handle text edit event - notify LSP
    fn handle_edit(&mut self, buffer_id: usize, event: &Event) {
        // Apply event to buffer
        self.buffers.get_mut(&buffer_id).unwrap().apply(event);

        // Notify LSP of change
        if let Some(path) = self.buffers[&buffer_id].buffer.file_path() {
            if let Some(language) = self.language_for_path(path) {
                if let Some(lsp) = self.lsp.get_or_spawn(language) {
                    let changes = self.event_to_lsp_change(event);
                    lsp.did_change(&path.to_string_lossy(), changes);
                }
            }
        }
    }

    /// Request completion at cursor
    fn request_completion(&mut self, buffer_id: usize) {
        let state = &self.buffers[&buffer_id];
        let cursor_pos = state.primary_cursor().position;
        let (line, col) = self.position_to_line_col(&state.buffer, cursor_pos);

        if let Some(path) = state.buffer.file_path() {
            if let Some(language) = self.language_for_path(path) {
                if let Some(lsp) = self.lsp.get_or_spawn(language) {
                    if let Ok(completions) = lsp.completion(
                        &path.to_string_lossy(),
                        line as u32,
                        col as u32
                    ) {
                        self.show_completion_popup(completions);
                    }
                }
            }
        }
    }
}
```

### 4. Event → LSP Change Conversion

```rust
fn event_to_lsp_change(event: &Event) -> Vec<TextDocumentContentChangeEvent> {
    match event {
        Event::Insert { position, text, .. } => {
            vec![TextDocumentContentChangeEvent {
                range: Some(Range {
                    start: byte_to_position(position),
                    end: byte_to_position(position),
                }),
                text: text.clone(),
            }]
        }
        Event::Delete { range, .. } => {
            vec![TextDocumentContentChangeEvent {
                range: Some(Range {
                    start: byte_to_position(&range.start),
                    end: byte_to_position(&range.end),
                }),
                text: String::new(),
            }]
        }
        _ => vec![],
    }
}
```

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
