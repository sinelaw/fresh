# LSP Request Queuing: Per-Server Capabilities and Init-Gated Dispatch

## Problem

Two related bugs in the LSP client:

1. **Multi-server capability mismatch**: When multiple LSP servers are configured
   for the same language, capabilities (semantic tokens, folding ranges, etc.)
   are stored per-language. The second server's capabilities overwrite the
   first's. `handle_for_feature_mut` then returns the wrong server — one that
   doesn't actually support the requested feature. Produces:
   `LSP response error: Method Not Found: textDocument/semanticTokens/range (code -32601)`

2. **Init-window race**: `ServerHandle` entries are created in `force_spawn()`
   before the `initialize` handshake completes. Feature requests sent during
   this window go to servers whose capabilities are unknown. The async handler
   returns empty/error responses, but the main loop has already committed state
   (e.g. `lsp_hover_request_sent = true`) that prevents retry.

## Current Architecture

### Command flow

```
Main loop                          Async handler (tokio task)
─────────                          ─────────────────────────
force_spawn()
  └─ LspHandle::spawn()
       └─ handle.initialize()  ──>  LspCommand::Initialize
                                      └─ send initialize request to server
                                      └─ await response
                                      └─ state.initialized = true
                                      └─ replay pending_commands (didOpen, etc.)
                                      └─ send LspInitialized to main loop
                                      └─ send LspStatusUpdate(Running)

handle.hover(...)  ──────────────>  LspCommand::Hover
                                      └─ if state.initialized: handle
                                      └─ else: send empty response back
```

### Existing queuing in the async handler

The async handler already queues some commands before initialization:

| Command type | Pre-init behavior |
|---|---|
| Notifications (didOpen, didChange, didClose, didSave) | **Queued** in `pending_commands`, replayed after init |
| Semantic tokens, folding ranges | Return **error** "LSP not initialized" |
| Hover, completion, definition, references, etc. | Return **empty response** |

The empty/error responses are a problem: the main loop interprets them as
"nothing found" rather than "server not ready, try again later".

### The gap

There are two separate init gates that are not synchronized:

1. **Async handler** (`state.initialized`): set when the initialize handshake
   completes, before `LspInitialized` is sent to the main loop.
2. **Main loop** (`ServerCapabilitySummary.initialized`): set when
   `LspInitialized` is processed, which happens later (async message delivery).

Feature requests from the main loop can be sent to the async handler between
these two points. The async handler will process them (since its
`state.initialized` is true), but the main loop doesn't yet know the server's
capabilities and may route to the wrong server in multi-server setups.

## Design: Queue All Requests Until Capabilities Are Known

### Principle

**The main loop never sends feature requests to a server whose capabilities
are unknown.** This is the VS Code LSP client model.

### Changes

#### 1. `has_capability` returns `false` before initialization

```rust
impl ServerHandle {
    pub fn has_capability(&self, feature: LspFeature) -> bool {
        if !self.capabilities.initialized {
            return false;  // not ready yet
        }
        // ... check actual capabilities
    }
}
```

`handle_for_feature_mut` already calls `has_capability`, so uninitialized
servers are automatically excluded from feature routing. No separate "pending
handles" map needed.

#### 2. Queue feature requests on the main loop when no server is ready

Instead of silently failing when `handle_for_feature_mut` returns `None` (or
`with_lsp_for_buffer` returns `None`), queue the request and replay it when
`LspInitialized` arrives.

Add to `App`:

```rust
/// Feature requests queued because no initialized server was available.
/// Keyed by language. Replayed when LspInitialized is received.
pending_feature_requests: HashMap<String, Vec<PendingFeatureRequest>>,
```

Where:

```rust
enum PendingFeatureRequest {
    Hover { buffer_id: BufferId, byte_pos: usize, screen_x: u16, screen_y: u16 },
    SemanticTokensFull { buffer_id: BufferId },
    SemanticTokensRange { buffer_id: BufferId, start_line: usize, end_line: usize },
    FoldingRanges { buffer_id: BufferId },
    // Note: completion and definition are user-initiated and will be
    // re-triggered naturally. No need to queue them.
}
```

Only queue requests that are **editor-initiated** (triggered by render, timers,
or initialization) — not user-initiated requests like completion or
go-to-definition, which the user will re-trigger.

#### 3. Replay pending requests on `LspInitialized`

In the `LspInitialized` handler (app/mod.rs), after `set_server_capabilities`:

```rust
AsyncMessage::LspInitialized { language, server_name, capabilities } => {
    // ... set capabilities on handle ...

    // Replay any feature requests that were queued while waiting for init
    if let Some(pending) = self.pending_feature_requests.remove(&language) {
        for request in pending {
            match request {
                PendingFeatureRequest::Hover { buffer_id, byte_pos, screen_x, screen_y } => {
                    self.mouse_hover_screen_position = Some((screen_x, screen_y));
                    let _ = self.request_hover_at_position(byte_pos);
                }
                PendingFeatureRequest::SemanticTokensFull { buffer_id } => {
                    self.schedule_semantic_tokens_full_refresh(buffer_id);
                }
                // ...
            }
        }
    }

    // These already handle the common case:
    self.resend_did_open_for_language(&language);
    self.request_semantic_tokens_for_language(&language);
    self.request_folding_ranges_for_language(&language);
}
```

Note: `request_semantic_tokens_for_language` and
`request_folding_ranges_for_language` already exist and handle the
semantic-tokens and folding-ranges cases. The main gap is **hover** — which
currently has no retry mechanism.

#### 4. Fix the hover "sent" flag

The hover state machine must not mark a request as "sent" unless it was
actually dispatched to a server:

```rust
pub fn force_check_mouse_hover(&mut self) -> bool {
    if let Some((byte_pos, _, screen_x, screen_y)) = self.mouse_state.lsp_hover_state {
        if !self.mouse_state.lsp_hover_request_sent {
            self.mouse_hover_screen_position = Some((screen_x, screen_y));
            match self.request_hover_at_position(byte_pos) {
                Ok(true) => {
                    self.mouse_state.lsp_hover_request_sent = true;
                    return true;
                }
                Ok(false) => return false, // not sent, retry later
                Err(e) => {
                    tracing::debug!("Failed to request hover: {}", e);
                    return false;
                }
            }
        }
    }
    false
}
```

Same fix for `check_mouse_hover_timer`.

`request_hover_at_position` returns `Ok(bool)` — `true` if the request was
dispatched, `false` if no server was available.

#### 5. Remove the async handler's empty-response fallback

Once the main loop properly gates requests, the async handler's pre-init
empty/error responses become dead code. They can be simplified to:

```rust
LspCommand::Hover { .. } => {
    if state.initialized {
        // ... handle normally
    }
    // else: main loop should never send this before init.
    // If it does, it's a bug — log a warning.
}
```

This is a cleanup step, not strictly required, but makes the invariant
explicit.

### What does NOT need queuing

| Request type | Why no queue needed |
|---|---|
| Completion | User-initiated (keystroke). User will type again. |
| Go-to-definition | User-initiated (shortcut/click). User will trigger again. |
| References | User-initiated. |
| Rename | User-initiated. |
| Signature help | Triggered on typing. Will re-trigger on next char. |
| Code actions | User-initiated (lightbulb/shortcut). |
| Document diagnostics | Pull-diagnostics are re-triggered on file changes. |
| Inlay hints | Re-requested on server quiescence and document changes. |

These all either retry naturally or are user-initiated.

### What DOES need queuing or special handling

| Request type | Current retry mechanism | Gap |
|---|---|---|
| Semantic tokens (full) | `request_semantic_tokens_for_language` on `LspInitialized` | None (already handled) |
| Semantic tokens (range) | Requested every render cycle | None (already handled) |
| Folding ranges | `request_folding_ranges_for_language` on `LspInitialized` | None (already handled) |
| Hover | `check_mouse_hover_timer` fires once, sets sent flag | **Gap: flag prevents retry. Fix: don't set flag until confirmed sent.** |

So the actual change is small:
1. `has_capability` returns `false` before init (already done)
2. Fix hover sent flag (the real bug)
3. No explicit queue needed — existing retry mechanisms cover everything

## Implementation order

1. **Per-server capabilities** (done): `ServerCapabilitySummary` on
   `ServerHandle`, `LspInitialized` carries `server_name` + capabilities,
   `handle_for_feature_mut` checks `has_capability`.

2. **`has_capability` returns `false` before init**: strict, correct behavior.

3. **Fix hover sent flag**: `request_hover_at_position` returns `Ok(bool)`,
   callers only set `lsp_hover_request_sent` on `Ok(true)`.

4. **Verify all other callers**: audit every call to `handle_for_feature_mut`
   / `with_lsp_for_buffer` to ensure they handle `None` gracefully (retry or
   ignore, not poison state).

5. **Optional cleanup**: remove async handler's pre-init empty/error responses,
   replace with debug warnings.

## Why not a full request queue?

After analysis, a full main-loop request queue (`PendingFeatureRequest` enum)
is unnecessary because:

- Notifications (didOpen, didChange, etc.) are already queued by the async
  handler's `pending_commands`.
- Semantic tokens and folding ranges are already re-requested from the
  `LspInitialized` handler.
- Hover just needs the sent-flag fix to enable natural retry via the timer.
- User-initiated requests (completion, definition, etc.) don't need queuing.

The "VS Code model" of queuing all requests is only needed when the client
wants to guarantee zero-loss delivery. In Fresh's architecture, the
`LspInitialized` handler already triggers the right follow-up requests,
so the simpler fix (gate + retry) achieves the same result.
