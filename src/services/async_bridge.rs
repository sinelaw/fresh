//! Async Bridge: Communication between async Tokio runtime and sync main loop
//!
//! This module implements the hybrid architecture described in TOKIO_ANALYSIS.md:
//! - Tokio runtime handles I/O tasks (LSP, file watching, git, etc.)
//! - Main UI loop stays synchronous (rendering, input, buffer manipulation)
//! - std::sync::mpsc channels bridge the two worlds
//!
//! Philosophy:
//! - I/O should be async (LSP, filesystem, network)
//! - Computation should be sync (editing, rendering)
//! - Main loop remains responsive and simple

use crate::services::terminal::TerminalId;
use crate::view::file_tree::{FileTreeView, NodeId};
use lsp_types::{
    CodeActionOrCommand, CompletionItem, Diagnostic, InlayHint, Location, SemanticTokensLegend,
    SemanticTokensResult, SignatureHelp,
};
use serde_json::Value;
use std::sync::mpsc;

/// Messages sent from async tasks to the synchronous main loop
#[derive(Debug)]
pub enum AsyncMessage {
    /// LSP diagnostics received for a file
    LspDiagnostics {
        uri: String,
        diagnostics: Vec<Diagnostic>,
    },

    /// LSP server initialized successfully
    LspInitialized {
        language: String,
        /// Completion trigger characters from server capabilities
        completion_trigger_characters: Vec<String>,
        /// Legend describing semantic token types supported by the server
        semantic_tokens_legend: Option<SemanticTokensLegend>,
        /// Whether the server supports full document semantic tokens
        semantic_tokens_full: bool,
    },

    /// LSP server crashed or failed
    LspError {
        language: String,
        error: String,
        /// Path to the stderr log file for this LSP session
        stderr_log_path: Option<std::path::PathBuf>,
    },

    /// LSP completion response
    LspCompletion {
        request_id: u64,
        items: Vec<CompletionItem>,
    },

    /// LSP go-to-definition response
    LspGotoDefinition {
        request_id: u64,
        locations: Vec<Location>,
    },

    /// LSP rename response
    LspRename {
        request_id: u64,
        result: Result<lsp_types::WorkspaceEdit, String>,
    },

    /// LSP hover response
    LspHover {
        request_id: u64,
        /// Hover contents as a single string (joined if multiple parts)
        contents: String,
        /// Whether the content is markdown (true) or plaintext (false)
        is_markdown: bool,
        /// Optional range of the symbol that was hovered over (LSP line/character positions)
        /// Used to highlight the hovered symbol
        range: Option<((u32, u32), (u32, u32))>,
    },

    /// LSP find references response
    LspReferences {
        request_id: u64,
        locations: Vec<Location>,
    },

    /// LSP signature help response
    LspSignatureHelp {
        request_id: u64,
        signature_help: Option<SignatureHelp>,
    },

    /// LSP code actions response
    LspCodeActions {
        request_id: u64,
        actions: Vec<CodeActionOrCommand>,
    },

    /// LSP pulled diagnostics response (textDocument/diagnostic)
    LspPulledDiagnostics {
        request_id: u64,
        uri: String,
        /// New result_id for incremental updates (None if server doesn't support)
        result_id: Option<String>,
        /// Diagnostics (empty if unchanged)
        diagnostics: Vec<Diagnostic>,
        /// True if diagnostics haven't changed since previous_result_id
        unchanged: bool,
    },

    /// LSP inlay hints response (textDocument/inlayHint)
    LspInlayHints {
        request_id: u64,
        uri: String,
        /// Inlay hints for the requested range
        hints: Vec<InlayHint>,
    },

    /// LSP semantic tokens response (textDocument/semanticTokens/full)
    LspSemanticTokens {
        request_id: u64,
        uri: String,
        result: Result<Option<SemanticTokensResult>, String>,
    },

    /// LSP server status became quiescent (project fully loaded)
    /// This is a rust-analyzer specific notification (experimental/serverStatus)
    LspServerQuiescent { language: String },

    /// File changed externally (future: file watching)
    FileChanged { path: String },

    /// Git status updated (future: git integration)
    GitStatusChanged { status: String },

    /// File explorer initialized with tree view
    FileExplorerInitialized(FileTreeView),

    /// File explorer node toggle completed
    FileExplorerToggleNode(NodeId),

    /// File explorer node refresh completed
    FileExplorerRefreshNode(NodeId),

    /// File explorer expand to path completed
    /// Contains the updated FileTreeView with the path expanded and selected
    FileExplorerExpandedToPath(FileTreeView),

    /// Plugin process completed with output
    PluginProcessOutput {
        /// Unique ID for this process (to match with callback)
        process_id: u64,
        /// Standard output
        stdout: String,
        /// Standard error
        stderr: String,
        /// Exit code
        exit_code: i32,
    },

    /// LSP progress notification ($/progress)
    LspProgress {
        language: String,
        token: String,
        value: LspProgressValue,
    },

    /// LSP window message (window/showMessage)
    LspWindowMessage {
        language: String,
        message_type: LspMessageType,
        message: String,
    },

    /// LSP log message (window/logMessage)
    LspLogMessage {
        language: String,
        message_type: LspMessageType,
        message: String,
    },

    /// LSP server status update
    LspStatusUpdate {
        language: String,
        status: LspServerStatus,
    },
    /// Generic notification from an LSP server
    CustomNotification {
        language: String,
        method: String,
        params: Option<Value>,
    },

    /// LSP server request (server -> client)
    /// Used for custom/extension methods that plugins can handle
    LspServerRequest {
        language: String,
        server_command: String,
        method: String,
        params: Option<Value>,
    },
    /// Response for a plugin-initiated LSP request
    PluginLspResponse {
        language: String,
        request_id: u64,
        result: Result<Value, String>,
    },

    /// Generic plugin response (e.g., GetBufferText result)
    PluginResponse(crate::services::plugins::api::PluginResponse),

    /// File open dialog: directory listing completed
    FileOpenDirectoryLoaded(std::io::Result<Vec<crate::services::fs::FsEntry>>),

    /// Terminal output received (triggers redraw)
    TerminalOutput { terminal_id: TerminalId },

    /// Terminal process exited
    TerminalExited { terminal_id: TerminalId },
}

/// LSP progress value types
#[derive(Debug, Clone)]
pub enum LspProgressValue {
    Begin {
        title: String,
        message: Option<String>,
        percentage: Option<u32>,
    },
    Report {
        message: Option<String>,
        percentage: Option<u32>,
    },
    End {
        message: Option<String>,
    },
}

/// LSP message type (corresponds to MessageType in LSP spec)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LspMessageType {
    Error = 1,
    Warning = 2,
    Info = 3,
    Log = 4,
}

/// LSP server status
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LspServerStatus {
    Starting,
    Initializing,
    Running,
    Error,
    Shutdown,
}

/// Bridge between async Tokio runtime and sync main loop
///
/// Design:
/// - Lightweight, cloneable sender that can be passed to async tasks
/// - Non-blocking receiver checked each frame in main loop
/// - No locks needed in main loop (channel handles synchronization)
#[derive(Clone)]
pub struct AsyncBridge {
    sender: mpsc::Sender<AsyncMessage>,
    // Receiver wrapped in Arc<Mutex<>> to allow cloning
    receiver: std::sync::Arc<std::sync::Mutex<mpsc::Receiver<AsyncMessage>>>,
}

impl AsyncBridge {
    /// Create a new async bridge with an unbounded channel
    ///
    /// Unbounded is appropriate here because:
    /// 1. Main loop processes messages every 16ms (60fps)
    /// 2. LSP messages are infrequent (< 100/sec typically)
    /// 3. Memory usage is bounded by message rate × frame time
    pub fn new() -> Self {
        let (sender, receiver) = mpsc::channel();
        Self {
            sender,
            receiver: std::sync::Arc::new(std::sync::Mutex::new(receiver)),
        }
    }

    /// Get a cloneable sender for async tasks
    ///
    /// This sender can be:
    /// - Cloned freely (cheap Arc internally)
    /// - Sent to async tasks
    /// - Stored in LspClient instances
    pub fn sender(&self) -> mpsc::Sender<AsyncMessage> {
        self.sender.clone()
    }

    /// Try to receive pending messages (non-blocking)
    ///
    /// Called each frame in the main loop to process async messages.
    /// Returns all pending messages without blocking.
    pub fn try_recv_all(&self) -> Vec<AsyncMessage> {
        let mut messages = Vec::new();

        // Lock the receiver and drain all pending messages
        if let Ok(receiver) = self.receiver.lock() {
            while let Ok(msg) = receiver.try_recv() {
                messages.push(msg);
            }
        }

        messages
    }

    /// Check if there are pending messages (non-blocking)
    pub fn has_messages(&self) -> bool {
        // Note: This is racy but safe - only used for optimization
        if let Ok(receiver) = self.receiver.lock() {
            receiver.try_recv().is_ok()
        } else {
            false
        }
    }
}

impl Default for AsyncBridge {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_async_bridge_send_receive() {
        let bridge = AsyncBridge::new();
        let sender = bridge.sender();

        // Send a message
        sender
            .send(AsyncMessage::LspInitialized {
                language: "rust".to_string(),
                completion_trigger_characters: vec![".".to_string()],
                semantic_tokens_legend: None,
                semantic_tokens_full: false,
            })
            .unwrap();

        // Receive it
        let messages = bridge.try_recv_all();
        assert_eq!(messages.len(), 1);

        match &messages[0] {
            AsyncMessage::LspInitialized {
                language,
                completion_trigger_characters,
                ..
            } => {
                assert_eq!(language, "rust");
                assert_eq!(completion_trigger_characters, &vec![".".to_string()]);
            }
            _ => panic!("Wrong message type"),
        }
    }

    #[test]
    fn test_async_bridge_multiple_messages() {
        let bridge = AsyncBridge::new();
        let sender = bridge.sender();

        // Send multiple messages
        sender
            .send(AsyncMessage::LspInitialized {
                language: "rust".to_string(),
                completion_trigger_characters: vec![],
                semantic_tokens_legend: None,
                semantic_tokens_full: false,
            })
            .unwrap();
        sender
            .send(AsyncMessage::LspInitialized {
                language: "typescript".to_string(),
                completion_trigger_characters: vec![],
                semantic_tokens_legend: None,
                semantic_tokens_full: false,
            })
            .unwrap();

        // Receive all at once
        let messages = bridge.try_recv_all();
        assert_eq!(messages.len(), 2);
    }

    #[test]
    fn test_async_bridge_no_messages() {
        let bridge = AsyncBridge::new();

        // Try to receive with no messages
        let messages = bridge.try_recv_all();
        assert_eq!(messages.len(), 0);
    }

    #[test]
    fn test_async_bridge_clone_sender() {
        let bridge = AsyncBridge::new();
        let sender1 = bridge.sender();
        let sender2 = sender1.clone();

        // Both senders work
        sender1
            .send(AsyncMessage::LspInitialized {
                language: "rust".to_string(),
                completion_trigger_characters: vec![],
                semantic_tokens_legend: None,
                semantic_tokens_full: false,
            })
            .unwrap();
        sender2
            .send(AsyncMessage::LspInitialized {
                language: "typescript".to_string(),
                completion_trigger_characters: vec![],
                semantic_tokens_legend: None,
                semantic_tokens_full: false,
            })
            .unwrap();

        let messages = bridge.try_recv_all();
        assert_eq!(messages.len(), 2);
    }

    #[test]
    fn test_async_bridge_diagnostics() {
        let bridge = AsyncBridge::new();
        let sender = bridge.sender();

        // Send diagnostic message
        let diagnostics = vec![lsp_types::Diagnostic {
            range: lsp_types::Range {
                start: lsp_types::Position {
                    line: 0,
                    character: 0,
                },
                end: lsp_types::Position {
                    line: 0,
                    character: 5,
                },
            },
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("rust-analyzer".to_string()),
            message: "test error".to_string(),
            related_information: None,
            tags: None,
            data: None,
        }];

        sender
            .send(AsyncMessage::LspDiagnostics {
                uri: "file:///test.rs".to_string(),
                diagnostics: diagnostics.clone(),
            })
            .unwrap();

        let messages = bridge.try_recv_all();
        assert_eq!(messages.len(), 1);

        match &messages[0] {
            AsyncMessage::LspDiagnostics {
                uri,
                diagnostics: diags,
            } => {
                assert_eq!(uri, "file:///test.rs");
                assert_eq!(diags.len(), 1);
                assert_eq!(diags[0].message, "test error");
            }
            _ => panic!("Expected LspDiagnostics message"),
        }
    }

    #[test]
    fn test_async_bridge_error_message() {
        let bridge = AsyncBridge::new();
        let sender = bridge.sender();

        sender
            .send(AsyncMessage::LspError {
                language: "rust".to_string(),
                error: "Failed to initialize".to_string(),
                stderr_log_path: None,
            })
            .unwrap();

        let messages = bridge.try_recv_all();
        assert_eq!(messages.len(), 1);

        match &messages[0] {
            AsyncMessage::LspError {
                language,
                error,
                stderr_log_path,
            } => {
                assert_eq!(language, "rust");
                assert_eq!(error, "Failed to initialize");
                assert!(stderr_log_path.is_none());
            }
            _ => panic!("Expected LspError message"),
        }
    }

    #[test]
    fn test_async_bridge_clone_bridge() {
        let bridge = AsyncBridge::new();
        let bridge_clone = bridge.clone();
        let sender = bridge.sender();

        // Send via original bridge's sender
        sender
            .send(AsyncMessage::LspInitialized {
                language: "rust".to_string(),
                completion_trigger_characters: vec![],
                semantic_tokens_legend: None,
                semantic_tokens_full: false,
            })
            .unwrap();

        // Receive via cloned bridge
        let messages = bridge_clone.try_recv_all();
        assert_eq!(messages.len(), 1);
    }

    #[test]
    fn test_async_bridge_multiple_calls_to_try_recv_all() {
        let bridge = AsyncBridge::new();
        let sender = bridge.sender();

        sender
            .send(AsyncMessage::LspInitialized {
                language: "rust".to_string(),
                completion_trigger_characters: vec![],
                semantic_tokens_legend: None,
                semantic_tokens_full: false,
            })
            .unwrap();

        // First call gets the message
        let messages1 = bridge.try_recv_all();
        assert_eq!(messages1.len(), 1);

        // Second call gets nothing
        let messages2 = bridge.try_recv_all();
        assert_eq!(messages2.len(), 0);
    }

    #[test]
    fn test_async_bridge_ordering() {
        let bridge = AsyncBridge::new();
        let sender = bridge.sender();

        // Send messages in order
        sender
            .send(AsyncMessage::LspInitialized {
                language: "rust".to_string(),
                completion_trigger_characters: vec![],
                semantic_tokens_legend: None,
                semantic_tokens_full: false,
            })
            .unwrap();
        sender
            .send(AsyncMessage::LspInitialized {
                language: "typescript".to_string(),
                completion_trigger_characters: vec![],
                semantic_tokens_legend: None,
                semantic_tokens_full: false,
            })
            .unwrap();
        sender
            .send(AsyncMessage::LspInitialized {
                language: "python".to_string(),
                completion_trigger_characters: vec![],
                semantic_tokens_legend: None,
                semantic_tokens_full: false,
            })
            .unwrap();

        // Messages should be received in same order
        let messages = bridge.try_recv_all();
        assert_eq!(messages.len(), 3);

        match (&messages[0], &messages[1], &messages[2]) {
            (
                AsyncMessage::LspInitialized { language: l1, .. },
                AsyncMessage::LspInitialized { language: l2, .. },
                AsyncMessage::LspInitialized { language: l3, .. },
            ) => {
                assert_eq!(l1, "rust");
                assert_eq!(l2, "typescript");
                assert_eq!(l3, "python");
            }
            _ => panic!("Expected ordered LspInitialized messages"),
        }
    }
}
