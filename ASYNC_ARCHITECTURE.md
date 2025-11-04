# Async Architecture Design

## Problem Statement

The current synchronous architecture has several issues:

1. **UI Freezing**: LSP requests (completion, hover, etc.) block the UI until response arrives
2. **Notification Loss**: LSP notifications (diagnostics) can arrive at any time but we only read during request/response
3. **File I/O Blocking**: Saving large files freezes the editor
4. **Input Lag**: 100ms poll timeout means up to 100ms delay before keypresses are processed

## Lessons from Established Editors

### Neovim (async-lsp approach)
- **Main loop**: Runs in main thread, handles UI updates
- **LSP I/O**: Background thread reads LSP stdout continuously
- **Notifications**: Queued to main thread via channel
- **Pattern**: Notifications processed synchronously (in order), requests processed async

### VSCode
- **Node.js event loop**: Non-blocking I/O by default
- **Language Client**: Runs in Extension Host (separate process)
- **IPC**: Uses JSON-RPC over sockets/pipes with event emitters
- **Pattern**: All I/O is async, UI updates on main thread

### Emacs (async processes)
- **Main thread**: Single-threaded Lisp interpreter
- **LSP I/O**: Async processes with process filters
- **Pattern**: Process output triggers callbacks, which update buffers

## Recommended Architecture: Actor-Like Model

### Core Principle: Separation of Concerns
- **Main Thread**: UI rendering and input handling (must be fast)
- **LSP Thread**: I/O with language server (can block)
- **File I/O Thread Pool**: Async file operations
- **Message Passing**: Channels for communication

### Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                       Main Thread                           │
│  ┌──────────────┐    ┌──────────────┐   ┌──────────────┐  │
│  │  Event Loop  │───▶│   Editor     │──▶│  Renderer    │  │
│  │  (crossterm) │    │   (State)    │   │  (ratatui)   │  │
│  └──────────────┘    └──────┬───────┘   └──────────────┘  │
│                             │                               │
│                             │ apply_event()                 │
│                             ▼                               │
│                      ┌──────────────┐                       │
│                      │  EventQueue  │◀─ messages from LSP   │
│                      │  (mpsc)      │◀─ messages from File  │
│                      └──────────────┘                       │
└─────────────────────────────────────────────────────────────┘
                             ▲                ▲
                             │                │
                             │ send events    │ send events
                             │                │
┌────────────────────────────┴───┐  ┌────────┴────────────────┐
│        LSP Thread              │  │   File I/O Thread       │
│  ┌──────────────────────────┐  │  │  ┌──────────────────┐  │
│  │  LspWorker               │  │  │  │  FileWorker      │  │
│  │  - Read stdout           │  │  │  │  - async save    │  │
│  │  - Handle notifications  │  │  │  │  - async load    │  │
│  │  - Send responses        │  │  │  └──────────────────┘  │
│  └──────────────────────────┘  │  └─────────────────────────┘
│                                 │
│  Language Server Process        │
│  (rust-analyzer, etc.)          │
└─────────────────────────────────┘
```

## Implementation Strategy

### Phase 1: Non-Blocking LSP I/O (Critical)

#### Problem
Currently, LSP client uses blocking I/O:
```rust
// Current: Blocks until response
pub fn read_message(&mut self) -> Result<JsonRpcMessage> {
    let mut headers = HashMap::new();
    // Blocks reading headers...
    let mut content = vec![0; content_length];
    self.stdout.read_exact(&mut content)?; // BLOCKS!
}
```

#### Solution: Background Reader Thread
```rust
pub struct LspClient {
    // Main thread handle
    process: Child,
    stdin: BufWriter<ChildStdin>,

    // Background thread (owns stdout)
    reader_thread: JoinHandle<()>,

    // Communication channels
    notification_rx: mpsc::Receiver<LspNotification>,
    response_rx: mpsc::Receiver<LspResponse>,

    // Request sending
    request_tx: mpsc::Sender<LspRequest>,
}

impl LspClient {
    pub fn spawn(command: &str, args: &[String]) -> Result<Self> {
        let mut child = Command::new(command)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()?;

        let stdout = child.stdout.take().unwrap();
        let (notification_tx, notification_rx) = mpsc::channel();
        let (response_tx, response_rx) = mpsc::channel();

        // Background thread reads stdout continuously
        let reader_thread = std::thread::spawn(move || {
            let mut reader = BufReader::new(stdout);
            loop {
                match read_message(&mut reader) {
                    Ok(JsonRpcMessage::Notification(n)) => {
                        notification_tx.send(n).ok();
                    }
                    Ok(JsonRpcMessage::Response(r)) => {
                        response_tx.send(r).ok();
                    }
                    Err(_) => break, // Server died
                }
            }
        });

        Ok(LspClient {
            process: child,
            stdin: BufWriter::new(child.stdin.take().unwrap()),
            reader_thread,
            notification_rx,
            response_rx,
            request_tx,
        })
    }

    // Non-blocking: check for notifications
    pub fn poll_notifications(&mut self) -> Vec<LspNotification> {
        self.notification_rx.try_iter().collect()
    }
}
```

### Phase 2: Async Request/Response Pattern

For operations that need responses (completion, hover, etc.):

```rust
// Request ID tracking
pub struct PendingRequest {
    request_id: i64,
    sender: oneshot::Sender<LspResponse>,
}

impl LspClient {
    // Async request (returns immediately, response comes via channel)
    pub fn request_async(
        &mut self,
        method: &str,
        params: Value,
    ) -> mpsc::Receiver<Result<Value>> {
        let (tx, rx) = mpsc::channel();
        let request_id = self.next_id;
        self.next_id += 1;

        // Send request
        self.send_request(request_id, method, params);

        // Store pending
        self.pending.insert(request_id, PendingRequest {
            request_id,
            sender: tx,
        });

        rx // Return receiver, caller waits or polls
    }
}
```

### Phase 3: Main Loop Integration

```rust
// main.rs
fn run_event_loop(
    editor: &mut Editor,
    terminal: &mut Terminal<_>,
) -> io::Result<()> {
    loop {
        // 1. Render
        terminal.draw(|frame| editor.render(frame))?;

        if editor.should_quit() {
            break;
        }

        // 2. Check for LSP notifications (non-blocking)
        if let Some(lsp) = editor.lsp_mut() {
            for notification in lsp.poll_notifications() {
                editor.handle_lsp_notification(notification);
            }
        }

        // 3. Poll for input (short timeout for responsiveness)
        if event_poll(Duration::from_millis(16))? { // 60fps
            match event_read()? {
                CrosstermEvent::Key(key_event) => {
                    handle_key_event(editor, key_event)?;
                }
                CrosstermEvent::Resize(width, height) => {
                    editor.resize(width, height);
                }
                _ => {}
            }
        }
    }

    Ok(())
}
```

### Phase 4: Editor Integration

```rust
// editor.rs
impl Editor {
    pub fn handle_lsp_notification(&mut self, notification: LspNotification) {
        match notification.method.as_str() {
            "textDocument/publishDiagnostics" => {
                let params: PublishDiagnosticsParams =
                    serde_json::from_value(notification.params).ok()?;

                // Update diagnostics display
                // This is now driven by LSP notifications, not manual calls
                self.update_diagnostics_for_uri(&params.uri, params.diagnostics);
            }
            _ => {}
        }
    }

    fn update_diagnostics_for_uri(&mut self, uri: &Url, diagnostics: Vec<Diagnostic>) {
        // Store diagnostics
        if let Some(lsp) = &mut self.lsp {
            lsp.update_diagnostics(uri, diagnostics);
        }

        // Update display if it's the active buffer
        if self.active_buffer_uri() == Some(uri) {
            self.update_diagnostics_display();
        }
    }
}
```

## Benefits of Async Architecture

### 1. Responsive UI
- Input processed within ~16ms (60fps) instead of 100ms
- UI never freezes waiting for LSP or file I/O

### 2. Continuous Diagnostics
- Diagnostics appear immediately when LSP sends them
- No manual polling or request needed

### 3. Better User Experience
- Type while completion is loading
- Continue editing while file saves in background
- No "frozen" feeling during operations

### 4. Scalability
- Can handle multiple LSP servers without blocking
- File operations can be parallelized
- Ready for future features (git operations, external tools)

## Implementation Priority

### High Priority (Do First)
1. **LSP Background Reader** - Critical for diagnostics to work properly
2. **Notification Handling** - publishDiagnostics must be received asynchronously
3. **Reduce Poll Timeout** - 16ms instead of 100ms for better responsiveness

### Medium Priority (Nice to Have)
4. **Async File Save** - Don't block on large file saves
5. **Async Completion** - Request completion, continue typing while waiting
6. **Request Cancellation** - Cancel in-flight requests when typing continues

### Low Priority (Future)
7. **Debouncing** - Implement smart debouncing for didChange (per LSP best practices)
8. **Progress Indicators** - Show loading state for long operations
9. **Multiple LSP Servers** - Handle multiple servers concurrently

## Alternative: Keep It Simple?

### Argument for Staying Synchronous
- **Simpler code**: No threads, no channels, no race conditions
- **Works for small files**: Current implementation is "good enough" for basic use
- **Easier to debug**: Single-threaded execution is easier to reason about

### Counterargument
- **Diagnostics won't work properly**: LSP server sends notifications anytime, we must read them
- **Poor UX**: 100ms input lag is noticeable and annoying
- **Not production-ready**: Any real editor needs async I/O
- **Complexity is manageable**: Background reader thread is ~50 lines of code

## Recommendation

**Implement Phase 1 (Background Reader) immediately** because:
1. Diagnostics display won't work without it (LSP notifications are asynchronous)
2. Relatively simple change (~100 lines)
3. No async runtime needed (just std::thread + mpsc)
4. Solves the most critical issue (notification loss)

**Defer Phases 2-4** until needed:
- Current sync requests work fine for initialization
- Async completion can wait until we implement completion feature
- File I/O is fast enough for now

## Code Changes Required

### Minimal Change (Phase 1 Only)
```rust
// src/lsp.rs - Add background reader
pub struct LspClient {
    // ... existing fields

    // New fields for async notifications
    reader_thread: Option<JoinHandle<()>>,
    notification_rx: mpsc::Receiver<JsonRpcNotification>,
}

// main.rs - Poll for notifications
fn run_event_loop(...) {
    loop {
        // Check for LSP notifications
        if let Some(lsp) = editor.lsp_mut() {
            for notif in lsp.poll_notifications() {
                editor.handle_lsp_notification(notif);
            }
        }

        // Reduce timeout for better responsiveness
        if event_poll(Duration::from_millis(16))? {
            // ... handle events
        }
    }
}
```

Estimated effort: **2-3 hours** for Phase 1 implementation + testing.

## Conclusion

**Yes, we need async I/O** - specifically for LSP notification handling. The current architecture cannot properly receive diagnostics from the LSP server because it only reads during request/response cycles.

The minimal change (background reader thread) gives us most of the benefits without introducing an async runtime or major complexity.
