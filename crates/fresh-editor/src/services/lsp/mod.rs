//! LSP (Language Server Protocol) Client Architecture
//!
//! This module implements a full-featured LSP client for the Fresh editor.
//! It supports multiple concurrent language servers, async I/O, and robust
//! error handling with automatic server restarts.
//!
//! # Architecture Overview
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────────────┐
//! │                           Editor (App)                                  │
//! │                                                                         │
//! │  ┌─────────────────────────────────────────────────────────────────┐   │
//! │  │  BufferMetadata                                                  │   │
//! │  │  - lsp_opened_with: HashSet<u64>  (tracks didOpen per server)   │   │
//! │  └─────────────────────────────────────────────────────────────────┘   │
//! │                              │                                          │
//! │                              ▼                                          │
//! │  ┌─────────────────────────────────────────────────────────────────┐   │
//! │  │  with_lsp_for_buffer() helper                                    │   │
//! │  │  - Ensures didOpen is sent before any request                   │   │
//! │  │  - Lazy text fetching (only if didOpen needed)                  │   │
//! │  │  - Per-server-instance tracking via handle IDs                  │   │
//! │  └─────────────────────────────────────────────────────────────────┘   │
//! │                              │                                          │
//! └──────────────────────────────┼──────────────────────────────────────────┘
//!                                │
//!                                ▼
//! ┌─────────────────────────────────────────────────────────────────────────┐
//! │                         LspManager                                      │
//! │                                                                         │
//! │  - One manager per editor instance                                     │
//! │  - Manages multiple language servers (one per language)                │
//! │  - Handles server lifecycle (spawn, restart, shutdown)                 │
//! │  - Restart throttling with exponential backoff                         │
//! │  - Manual start/stop support via command palette                       │
//! │                                                                         │
//! │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐                  │
//! │  │ LspHandle    │  │ LspHandle    │  │ LspHandle    │  ...             │
//! │  │ (rust)       │  │ (typescript) │  │ (python)     │                  │
//! │  │ id: 1        │  │ id: 2        │  │ id: 3        │                  │
//! │  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘                  │
//! └─────────┼─────────────────┼─────────────────┼───────────────────────────┘
//!           │                 │                 │
//!           │  tokio channels │                 │
//!           ▼                 ▼                 ▼
//! ┌─────────────────────────────────────────────────────────────────────────┐
//! │                     Tokio Runtime (async tasks)                         │
//! │                                                                         │
//! │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐                  │
//! │  │ LspTask      │  │ LspTask      │  │ LspTask      │                  │
//! │  │ (rust-       │  │ (typescript- │  │ (python-     │                  │
//! │  │  analyzer)   │  │  language-   │  │  lsp)        │                  │
//! │  │              │  │  server)     │  │              │                  │
//! │  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘                  │
//! └─────────┼─────────────────┼─────────────────┼───────────────────────────┘
//!           │                 │                 │
//!           │ stdin/stdout    │                 │
//!           ▼                 ▼                 ▼
//! ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
//! │ rust-analyzer   │ │ typescript-     │ │ pylsp           │
//! │ (subprocess)    │ │ language-server │ │ (subprocess)    │
//! └─────────────────┘ └─────────────────┘ └─────────────────┘
//! ```
//!
//! # Module Structure
//!
//! - **`manager`**: [`LspManager`] - Manages multiple language servers, handles
//!   spawning/restarting, routes requests by language. Includes restart throttling
//!   with exponential backoff to prevent server crash loops.
//!
//! - **`async_handler`**: [`LspHandle`] and `LspTask` - The async LSP client
//!   implementation. `LspHandle` is a sync handle that sends commands via tokio
//!   channels. `LspTask` runs in a separate tokio task, managing the server
//!   subprocess and JSON-RPC I/O. Each handle has a unique `id` for tracking.
//!
//! - **`diagnostics`**: Converts LSP diagnostics to editor overlays (colored
//!   underlines for errors, warnings, etc.).
//!
//! # Message Flow
//!
//! ## Outgoing Requests (Editor → Server)
//!
//! 1. Editor calls a request method (e.g., `request_hover()`)
//! 2. `with_lsp_for_buffer()` helper ensures `didOpen` was sent to this server instance
//! 3. If needed, fetches buffer text and sends `didOpen` first
//! 4. Request is sent via `LspHandle` through tokio channel
//! 5. `LspTask` serializes to JSON-RPC and writes to server stdin
//! 6. Response is parsed and sent back through `AsyncBridge`
//!
//! ## Incoming Notifications (Server → Editor)
//!
//! 1. `LspTask` reads from server stdout
//! 2. Parses JSON-RPC message
//! 3. For notifications (diagnostics, progress, etc.), sends via `AsyncBridge`
//! 4. Editor's main loop receives and processes the notification
//!
//! # Document Synchronization
//!
//! The LSP protocol requires `textDocument/didOpen` before any other document
//! operations. We track this per-buffer, per-server-instance:
//!
//! - Each `LspHandle` has a unique `id` (monotonically increasing)
//! - `BufferMetadata.lsp_opened_with` is a `HashSet<u64>` of handle IDs
//! - Before any request, we check if the current handle's ID is in the set
//! - If not, we send `didOpen` first, then add the ID to the set
//! - This handles: multiple servers per buffer, server restarts (new ID)
//!
//! # Error Handling
//!
//! - **Server crashes**: Automatic restart with exponential backoff
//! - **Too many restarts**: Server enters cooldown, user notified
//! - **Request timeout**: Logged, doesn't block editor
//! - **Capability checks**: Some features check server capabilities before sending
//!   (e.g., pull diagnostics only if `diagnosticProvider` is advertised)

pub mod async_handler;
pub mod diagnostics;
pub mod manager;
pub mod semantic_tokens;

// Re-export for public API (used by tests)
pub use crate::types::LspServerConfig;
