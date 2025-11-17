//! Async LSP Client using Tokio
//!
//! This module implements an asynchronous LSP client that:
//! - Runs in a separate Tokio task
//! - Uses tokio::process for async process I/O
//! - Sends notifications to main loop via AsyncBridge
//! - Handles LSP notifications asynchronously (diagnostics, etc.)
//!
//! Architecture:
//! - LspTask: Async task that manages LSP process and I/O
//! - LspHandle: Sync handle that can send commands to the task
//! - Uses tokio channels for command/response communication

use crate::async_bridge::{
    AsyncBridge, AsyncMessage, LspMessageType, LspProgressValue, LspServerStatus,
};
use crate::process_limits::ProcessLimits;
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidOpenTextDocument, DidSaveTextDocument, Initialized, Notification,
        PublishDiagnostics,
    },
    request::{Initialize, Request, Shutdown},
    ClientCapabilities, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, InitializeParams, InitializeResult, InitializedParams,
    PublishDiagnosticsParams, ServerCapabilities, TextDocumentContentChangeEvent,
    TextDocumentIdentifier, TextDocumentItem, Uri, VersionedTextDocumentIdentifier,
    WindowClientCapabilities, WorkspaceFolder,
};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{mpsc as std_mpsc, Arc, Mutex};
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::process::{Child, ChildStdin, ChildStdout, Command};
use tokio::sync::{mpsc, oneshot};

/// A JSON-RPC message
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum JsonRpcMessage {
    Request(JsonRpcRequest),
    Response(JsonRpcResponse),
    Notification(JsonRpcNotification),
}

/// A JSON-RPC request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcRequest {
    pub jsonrpc: String,
    pub id: i64,
    pub method: String,
    pub params: Option<Value>,
}

/// A JSON-RPC response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcResponse {
    pub jsonrpc: String,
    pub id: i64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<JsonRpcError>,
}

/// A JSON-RPC notification (no response expected)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcNotification {
    pub jsonrpc: String,
    pub method: String,
    pub params: Option<Value>,
}

/// A JSON-RPC error
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcError {
    pub code: i64,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

/// LSP client state machine
///
/// Tracks the lifecycle of the LSP client connection with proper state transitions.
/// This prevents invalid operations (e.g., can't initialize twice, can't send requests when stopped).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LspClientState {
    /// Initial state before spawning
    Initial,
    /// Process spawning in progress
    Starting,
    /// Initialize request sent, waiting for response
    Initializing,
    /// Initialized and ready for requests
    Running,
    /// Shutdown in progress
    Stopping,
    /// Cleanly stopped
    Stopped,
    /// Failed or crashed
    Error,
}

impl LspClientState {
    /// Check if this state can transition to another state
    pub fn can_transition_to(&self, next: LspClientState) -> bool {
        use LspClientState::*;
        match (self, next) {
            // From Initial, can only start
            (Initial, Starting) => true,
            // From Starting, can initialize or error
            (Starting, Initializing) | (Starting, Error) => true,
            // From Initializing, can become running or error
            (Initializing, Running) | (Initializing, Error) => true,
            // From Running, can stop or error
            (Running, Stopping) | (Running, Error) => true,
            // From Stopping, can become stopped or error
            (Stopping, Stopped) | (Stopping, Error) => true,
            // From Stopped or Error, can restart
            (Stopped, Starting) | (Error, Starting) => true,
            // Any state can become error
            (_, Error) => true,
            // Same state is always valid (no-op)
            (a, b) if *a == b => true,
            // All other transitions are invalid
            _ => false,
        }
    }

    /// Transition to a new state, returning error if invalid
    pub fn transition_to(&mut self, next: LspClientState) -> Result<(), String> {
        if self.can_transition_to(next) {
            *self = next;
            Ok(())
        } else {
            Err(format!(
                "Invalid state transition from {:?} to {:?}",
                self, next
            ))
        }
    }

    /// Check if the client is ready to send requests
    pub fn can_send_requests(&self) -> bool {
        matches!(self, LspClientState::Running)
    }

    /// Check if the client can accept initialization
    pub fn can_initialize(&self) -> bool {
        matches!(
            self,
            LspClientState::Initial | LspClientState::Starting | LspClientState::Stopped
        )
    }

    /// Convert to LspServerStatus for UI reporting
    pub fn to_server_status(&self) -> LspServerStatus {
        match self {
            LspClientState::Initial => LspServerStatus::Starting,
            LspClientState::Starting => LspServerStatus::Starting,
            LspClientState::Initializing => LspServerStatus::Initializing,
            LspClientState::Running => LspServerStatus::Running,
            LspClientState::Stopping => LspServerStatus::Shutdown,
            LspClientState::Stopped => LspServerStatus::Shutdown,
            LspClientState::Error => LspServerStatus::Error,
        }
    }
}

/// Create common LSP client capabilities with workDoneProgress support
fn create_client_capabilities() -> ClientCapabilities {
    use lsp_types::{
        GeneralClientCapabilities, RenameClientCapabilities,
        TextDocumentClientCapabilities, WorkspaceClientCapabilities, WorkspaceEditClientCapabilities,
    };

    ClientCapabilities {
        window: Some(WindowClientCapabilities {
            work_done_progress: Some(true),
            ..Default::default()
        }),
        workspace: Some(WorkspaceClientCapabilities {
            apply_edit: Some(true),
            workspace_edit: Some(WorkspaceEditClientCapabilities {
                document_changes: Some(true),
                ..Default::default()
            }),
            ..Default::default()
        }),
        text_document: Some(TextDocumentClientCapabilities {
            rename: Some(RenameClientCapabilities {
                dynamic_registration: Some(true),
                prepare_support: Some(true),
                honors_change_annotations: Some(true),
                ..Default::default()
            }),
            ..Default::default()
        }),
        general: Some(GeneralClientCapabilities {
            ..Default::default()
        }),
        ..Default::default()
    }
}

/// Commands sent from the main loop to the LSP task
#[derive(Debug)]
enum LspCommand {
    /// Initialize the server
    Initialize {
        root_uri: Option<Uri>,
        response: oneshot::Sender<Result<InitializeResult, String>>,
    },

    /// Notify document opened
    DidOpen {
        uri: Uri,
        text: String,
        language_id: String,
    },

    /// Notify document changed
    DidChange {
        uri: Uri,
        content_changes: Vec<TextDocumentContentChangeEvent>,
    },

    /// Notify document saved
    DidSave { uri: Uri, text: Option<String> },

    /// Request completion at position
    Completion {
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
    },

    /// Request go-to-definition
    GotoDefinition {
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
    },

    /// Request rename
    Rename {
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
        new_name: String,
    },

    /// Request hover documentation
    Hover {
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
    },

    /// Cancel a pending request
    CancelRequest {
        /// Editor's request ID to cancel
        request_id: u64,
    },

    /// Shutdown the server
    Shutdown,
}

/// Mutable state for LSP command processing
struct LspState {
    /// Stdin for sending messages
    stdin: ChildStdin,

    /// Next request ID
    next_id: i64,

    /// Server capabilities
    capabilities: Option<ServerCapabilities>,

    /// Document versions
    document_versions: HashMap<PathBuf, i64>,

    /// Whether initialized
    initialized: bool,

    /// Sender for async messages to main loop
    async_tx: std_mpsc::Sender<AsyncMessage>,

    /// Language ID (for error reporting)
    language: String,

    /// Mapping from editor request_id to LSP JSON-RPC id for cancellation
    /// Key: editor request_id, Value: LSP JSON-RPC id
    active_requests: HashMap<u64, i64>,
}

impl LspState {
    /// Write a message to stdin
    async fn write_message<T: Serialize>(&mut self, message: &T) -> Result<(), String> {
        let json =
            serde_json::to_string(message).map_err(|e| format!("Serialization error: {}", e))?;

        let content = format!("Content-Length: {}\r\n\r\n{}", json.len(), json);

        tracing::debug!("Writing LSP message to stdin ({} bytes)", content.len());

        self.stdin
            .write_all(content.as_bytes())
            .await
            .map_err(|e| format!("Failed to write to stdin: {}", e))?;

        self.stdin
            .flush()
            .await
            .map_err(|e| format!("Failed to flush stdin: {}", e))?;

        tracing::debug!("Successfully sent LSP message");

        Ok(())
    }

    /// Send a notification using lsp-types Notification trait (type-safe)
    async fn send_notification<N>(&mut self, params: N::Params) -> Result<(), String>
    where
        N: Notification,
    {
        let notification = JsonRpcNotification {
            jsonrpc: "2.0".to_string(),
            method: N::METHOD.to_string(),
            params: Some(serde_json::to_value(params).expect("Failed to serialize params")),
        };

        self.write_message(&notification).await
    }

    /// Send request using shared pending map
    async fn send_request_sequential<P: Serialize, R: for<'de> Deserialize<'de>>(
        &mut self,
        method: &str,
        params: Option<P>,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<R, String> {
        self.send_request_sequential_tracked(method, params, pending, None)
            .await
    }

    /// Send request using shared pending map with optional editor request tracking
    async fn send_request_sequential_tracked<P: Serialize, R: for<'de> Deserialize<'de>>(
        &mut self,
        method: &str,
        params: Option<P>,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
        editor_request_id: Option<u64>,
    ) -> Result<R, String> {
        let id = self.next_id;
        self.next_id += 1;

        // Track the mapping if editor_request_id is provided
        if let Some(editor_id) = editor_request_id {
            self.active_requests.insert(editor_id, id);
            tracing::debug!(
                "Tracking request: editor_id={}, lsp_id={}",
                editor_id,
                id
            );
        }

        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id,
            method: method.to_string(),
            params: params.map(|p| serde_json::to_value(p).expect("Failed to serialize params")),
        };

        let (tx, rx) = oneshot::channel();
        pending.lock().unwrap().insert(id, tx);

        self.write_message(&request).await?;

        tracing::debug!("Sent LSP request id={}, waiting for response...", id);

        // Await response (this is OK now because the reader task will send it)
        let result = rx
            .await
            .map_err(|_| "Response channel closed".to_string())??;

        tracing::debug!("Received LSP response for request id={}", id);

        // Remove tracking after response received
        if let Some(editor_id) = editor_request_id {
            self.active_requests.remove(&editor_id);
            tracing::debug!(
                "Completed request: editor_id={}, lsp_id={}",
                editor_id,
                id
            );
        }

        serde_json::from_value(result).map_err(|e| format!("Failed to deserialize response: {}", e))
    }

    /// Handle initialize command
    async fn handle_initialize_sequential(
        &mut self,
        root_uri: Option<Uri>,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<InitializeResult, String> {
        tracing::info!(
            "Initializing async LSP server with root_uri: {:?}",
            root_uri
        );

        let workspace_folders = root_uri.as_ref().map(|uri| {
            vec![WorkspaceFolder {
                uri: uri.clone(),
                name: uri
                    .path()
                    .as_str()
                    .split('/')
                    .last()
                    .unwrap_or("workspace")
                    .to_string(),
            }]
        });

        let params = InitializeParams {
            process_id: Some(std::process::id()),
            capabilities: create_client_capabilities(),
            workspace_folders,
            ..Default::default()
        };

        let result: InitializeResult = self
            .send_request_sequential(Initialize::METHOD, Some(params), pending)
            .await?;

        self.capabilities = Some(result.capabilities.clone());

        // Send initialized notification
        self.send_notification::<Initialized>(InitializedParams {})
            .await?;

        self.initialized = true;

        // Notify main loop
        let _ = self.async_tx.send(AsyncMessage::LspInitialized {
            language: self.language.clone(),
        });

        // Send running status
        let _ = self.async_tx.send(AsyncMessage::LspStatusUpdate {
            language: self.language.clone(),
            status: LspServerStatus::Running,
        });

        tracing::info!("Async LSP server initialized successfully");

        Ok(result)
    }

    /// Handle did_open command
    async fn handle_did_open_sequential(
        &mut self,
        uri: Uri,
        text: String,
        language_id: String,
        _pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        tracing::debug!("LSP: did_open for {}", uri.as_str());

        let params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id,
                version: 0,
                text,
            },
        };

        self.document_versions
            .insert(PathBuf::from(uri.path().as_str()), 0);

        self.send_notification::<DidOpenTextDocument>(params).await
    }

    /// Handle did_change command
    async fn handle_did_change_sequential(
        &mut self,
        uri: Uri,
        content_changes: Vec<TextDocumentContentChangeEvent>,
        _pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        tracing::debug!("LSP: did_change for {}", uri.as_str());

        let path = PathBuf::from(uri.path().as_str());
        let version = self.document_versions.entry(path).or_insert(0);
        *version += 1;

        let params = DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: *version as i32,
            },
            content_changes,
        };

        self.send_notification::<DidChangeTextDocument>(params)
            .await
    }

    /// Handle did_save command
    async fn handle_did_save(&mut self, uri: Uri, text: Option<String>) -> Result<(), String> {
        tracing::debug!("LSP: did_save for {}", uri.as_str());

        let params = DidSaveTextDocumentParams {
            text_document: TextDocumentIdentifier { uri },
            text,
        };

        self.send_notification::<DidSaveTextDocument>(params).await
    }

    /// Handle completion request
    async fn handle_completion(
        &mut self,
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        use lsp_types::{
            CompletionParams, PartialResultParams, Position, TextDocumentIdentifier,
            TextDocumentPositionParams, WorkDoneProgressParams,
        };

        tracing::debug!(
            "LSP: completion request at {}:{}:{}",
            uri.as_str(),
            line,
            character
        );

        let params = CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position { line, character },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            context: None,
        };

        // Send request and get response (tracked for cancellation)
        match self
            .send_request_sequential_tracked::<_, Value>(
                "textDocument/completion",
                Some(params),
                pending,
                Some(request_id),
            )
            .await
        {
            Ok(result) => {
                // Parse the completion response
                let items = if let Ok(list) =
                    serde_json::from_value::<lsp_types::CompletionList>(result.clone())
                {
                    list.items
                } else if let Ok(items) =
                    serde_json::from_value::<Vec<lsp_types::CompletionItem>>(result)
                {
                    items
                } else {
                    vec![]
                };

                // Send to main loop
                let _ = self
                    .async_tx
                    .send(AsyncMessage::LspCompletion { request_id, items });
                Ok(())
            }
            Err(e) => {
                tracing::error!("Completion request failed: {}", e);
                // Send empty completion on error
                let _ = self.async_tx.send(AsyncMessage::LspCompletion {
                    request_id,
                    items: vec![],
                });
                Err(e)
            }
        }
    }

    /// Handle go-to-definition request
    async fn handle_goto_definition(
        &mut self,
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        use lsp_types::{
            GotoDefinitionParams, PartialResultParams, Position, TextDocumentIdentifier,
            TextDocumentPositionParams, WorkDoneProgressParams,
        };

        tracing::debug!(
            "LSP: go-to-definition request at {}:{}:{}",
            uri.as_str(),
            line,
            character
        );

        let params = GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position { line, character },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };

        // Send request and get response
        match self
            .send_request_sequential::<_, Value>("textDocument/definition", Some(params), pending)
            .await
        {
            Ok(result) => {
                // Parse the definition response (can be Location, Vec<Location>, or LocationLink)
                let locations = if let Ok(loc) =
                    serde_json::from_value::<lsp_types::Location>(result.clone())
                {
                    vec![loc]
                } else if let Ok(locs) =
                    serde_json::from_value::<Vec<lsp_types::Location>>(result.clone())
                {
                    locs
                } else if let Ok(links) =
                    serde_json::from_value::<Vec<lsp_types::LocationLink>>(result)
                {
                    // Convert LocationLink to Location
                    links
                        .into_iter()
                        .map(|link| lsp_types::Location {
                            uri: link.target_uri,
                            range: link.target_selection_range,
                        })
                        .collect()
                } else {
                    vec![]
                };

                // Send to main loop
                let _ = self.async_tx.send(AsyncMessage::LspGotoDefinition {
                    request_id,
                    locations,
                });
                Ok(())
            }
            Err(e) => {
                tracing::error!("Go-to-definition request failed: {}", e);
                // Send empty locations on error
                let _ = self.async_tx.send(AsyncMessage::LspGotoDefinition {
                    request_id,
                    locations: vec![],
                });
                Err(e)
            }
        }
    }

    /// Handle rename request
    async fn handle_rename(
        &mut self,
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
        new_name: String,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        use lsp_types::{
            Position, RenameParams, TextDocumentIdentifier, TextDocumentPositionParams,
            WorkDoneProgressParams,
        };

        tracing::debug!(
            "LSP: rename request at {}:{}:{} to '{}'",
            uri.as_str(),
            line,
            character,
            new_name
        );

        let params = RenameParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position { line, character },
            },
            new_name,
            work_done_progress_params: WorkDoneProgressParams::default(),
        };

        // Send request and get response
        match self
            .send_request_sequential::<_, Value>("textDocument/rename", Some(params), pending)
            .await
        {
            Ok(result) => {
                // Parse the workspace edit response
                match serde_json::from_value::<lsp_types::WorkspaceEdit>(result) {
                    Ok(workspace_edit) => {
                        // Send to main loop
                        let _ = self.async_tx.send(AsyncMessage::LspRename {
                            request_id,
                            result: Ok(workspace_edit),
                        });
                        Ok(())
                    }
                    Err(e) => {
                        tracing::error!("Failed to parse rename response: {}", e);
                        let _ = self.async_tx.send(AsyncMessage::LspRename {
                            request_id,
                            result: Err(format!("Failed to parse rename response: {}", e)),
                        });
                        Err(format!("Failed to parse rename response: {}", e))
                    }
                }
            }
            Err(e) => {
                tracing::error!("Rename request failed: {}", e);
                // Send error to main loop
                let _ = self.async_tx.send(AsyncMessage::LspRename {
                    request_id,
                    result: Err(e.clone()),
                });
                Err(e)
            }
        }
    }

    /// Handle hover documentation request
    async fn handle_hover(
        &mut self,
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        use lsp_types::{
            HoverParams, Position, TextDocumentIdentifier, TextDocumentPositionParams,
            WorkDoneProgressParams,
        };

        tracing::debug!(
            "LSP: hover request at {}:{}:{}",
            uri.as_str(),
            line,
            character
        );

        let params = HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position { line, character },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
        };

        // Send request and get response
        match self
            .send_request_sequential::<_, Value>("textDocument/hover", Some(params), pending)
            .await
        {
            Ok(result) => {
                // Parse the hover response
                let (contents, is_markdown, range) = if result.is_null() {
                    // No hover information available
                    (String::new(), false, None)
                } else {
                    match serde_json::from_value::<lsp_types::Hover>(result) {
                        Ok(hover) => {
                            // Extract text from hover contents
                            let (contents, is_markdown) = Self::extract_hover_contents(&hover.contents);
                            // Extract the range if provided (tells us which symbol was hovered)
                            let range = hover.range.map(|r| {
                                (
                                    (r.start.line, r.start.character),
                                    (r.end.line, r.end.character),
                                )
                            });
                            (contents, is_markdown, range)
                        }
                        Err(e) => {
                            tracing::error!("Failed to parse hover response: {}", e);
                            (String::new(), false, None)
                        }
                    }
                };

                // Send to main loop
                let _ = self.async_tx.send(AsyncMessage::LspHover {
                    request_id,
                    contents,
                    is_markdown,
                    range,
                });
                Ok(())
            }
            Err(e) => {
                tracing::error!("Hover request failed: {}", e);
                // Send empty result on error (no hover available)
                let _ = self.async_tx.send(AsyncMessage::LspHover {
                    request_id,
                    contents: String::new(),
                    is_markdown: false,
                    range: None,
                });
                Err(e)
            }
        }
    }

    /// Extract text from hover contents (handles both MarkedString and MarkupContent)
    /// Returns (content_string, is_markdown)
    fn extract_hover_contents(contents: &lsp_types::HoverContents) -> (String, bool) {
        use lsp_types::{HoverContents, MarkedString, MarkupContent, MarkupKind};

        match contents {
            HoverContents::Scalar(marked) => match marked {
                MarkedString::String(s) => (s.clone(), false),
                MarkedString::LanguageString(ls) => {
                    // Language strings are formatted as markdown code blocks
                    (format!("```{}\n{}\n```", ls.language, ls.value), true)
                }
            },
            HoverContents::Array(arr) => {
                // Array of marked strings - format as markdown
                let content = arr
                    .iter()
                    .map(|marked| match marked {
                        MarkedString::String(s) => s.clone(),
                        MarkedString::LanguageString(ls) => {
                            format!("```{}\n{}\n```", ls.language, ls.value)
                        }
                    })
                    .collect::<Vec<_>>()
                    .join("\n\n");
                (content, true)
            },
            HoverContents::Markup(MarkupContent { kind, value }) => {
                // Check if it's markdown or plaintext
                let is_markdown = matches!(kind, MarkupKind::Markdown);
                (value.clone(), is_markdown)
            }
        }
    }

    /// Handle shutdown command
    async fn handle_shutdown(&mut self) -> Result<(), String> {
        tracing::info!("Shutting down async LSP server");

        let notification = JsonRpcNotification {
            jsonrpc: "2.0".to_string(),
            method: "shutdown".to_string(),
            params: None,
        };

        self.write_message(&notification).await?;

        let exit = JsonRpcNotification {
            jsonrpc: "2.0".to_string(),
            method: "exit".to_string(),
            params: None,
        };

        self.write_message(&exit).await
    }

    /// Send a cancel request notification to the server
    async fn send_cancel_request(&mut self, lsp_id: i64) -> Result<(), String> {
        tracing::debug!("Sending $/cancelRequest for LSP id {}", lsp_id);

        let notification = JsonRpcNotification {
            jsonrpc: "2.0".to_string(),
            method: "$/cancelRequest".to_string(),
            params: Some(serde_json::json!({ "id": lsp_id })),
        };

        self.write_message(&notification).await
    }

    /// Cancel a request by editor request_id
    async fn handle_cancel_request(&mut self, request_id: u64) -> Result<(), String> {
        if let Some(lsp_id) = self.active_requests.remove(&request_id) {
            tracing::info!(
                "Cancelling request: editor_id={}, lsp_id={}",
                request_id,
                lsp_id
            );
            self.send_cancel_request(lsp_id).await
        } else {
            tracing::debug!(
                "Cancel request ignored: no active LSP request for editor_id={}",
                request_id
            );
            Ok(())
        }
    }
}


/// Async LSP task that handles all I/O
struct LspTask {
    /// Process handle
    process: Child,

    /// Stdin for sending messages
    stdin: ChildStdin,

    /// Stdout for receiving messages
    stdout: BufReader<ChildStdout>,

    /// Next request ID
    next_id: i64,

    /// Pending requests waiting for response
    pending: HashMap<i64, oneshot::Sender<Result<Value, String>>>,

    /// Server capabilities
    capabilities: Option<ServerCapabilities>,

    /// Document versions
    document_versions: HashMap<PathBuf, i64>,

    /// Whether initialized
    initialized: bool,

    /// Sender for async messages to main loop
    async_tx: std_mpsc::Sender<AsyncMessage>,

    /// Language ID (for error reporting)
    language: String,
}

#[allow(dead_code)]
impl LspTask {
    /// Create a new LSP task
    async fn spawn(
        command: &str,
        args: &[String],
        language: String,
        async_tx: std_mpsc::Sender<AsyncMessage>,
        process_limits: &ProcessLimits,
    ) -> Result<Self, String> {
        tracing::info!("Spawning async LSP server: {} {:?}", command, args);
        tracing::info!("Process limits: {:?}", process_limits);

        let mut cmd = Command::new(command);
        cmd.args(args)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .kill_on_drop(true);

        // Apply resource limits to the process
        process_limits
            .apply_to_command(&mut cmd)
            .map_err(|e| format!("Failed to apply process limits: {}", e))?;

        let mut process = cmd
            .spawn()
            .map_err(|e| format!("Failed to spawn LSP process: {}", e))?;

        let stdin = process
            .stdin
            .take()
            .ok_or_else(|| "Failed to get stdin".to_string())?;

        let stdout = BufReader::new(
            process
                .stdout
                .take()
                .ok_or_else(|| "Failed to get stdout".to_string())?,
        );

        // Spawn a task to read and log stderr from the LSP process
        if let Some(stderr) = process.stderr.take() {
            let language_clone = language.clone();
            tokio::spawn(async move {
                use tokio::io::{AsyncBufReadExt, BufReader as TokioBufReader};
                let mut stderr_reader = TokioBufReader::new(stderr);
                let mut line = String::new();
                loop {
                    line.clear();
                    match stderr_reader.read_line(&mut line).await {
                        Ok(0) => {
                            tracing::info!(
                                "LSP ({}) stderr closed (process may have exited)",
                                language_clone
                            );
                            break;
                        }
                        Ok(_) => {
                            let trimmed = line.trim();
                            if !trimmed.is_empty() {
                                tracing::warn!("LSP ({}) stderr: {}", language_clone, trimmed);
                            }
                        }
                        Err(e) => {
                            tracing::error!(
                                "LSP ({}) stderr read error: {}",
                                language_clone,
                                e
                            );
                            break;
                        }
                    }
                }
            });
        }

        Ok(Self {
            process,
            stdin,
            stdout,
            next_id: 0,
            pending: HashMap::new(),
            capabilities: None,
            document_versions: HashMap::new(),
            initialized: false,
            async_tx,
            language,
        })
    }

    /// Run the task (processes commands and reads from stdout)
    async fn run(self, mut command_rx: mpsc::Receiver<LspCommand>) {
        tracing::info!("LspTask::run() started for language: {}", self.language);

        // Create state struct for command processing
        let mut state = LspState {
            stdin: self.stdin,
            next_id: self.next_id,
            capabilities: self.capabilities,
            document_versions: self.document_versions,
            initialized: self.initialized,
            async_tx: self.async_tx.clone(),
            language: self.language.clone(),
            active_requests: HashMap::new(),
        };

        // Move stdout out, share pending
        let mut stdout = self.stdout;
        let pending = Arc::new(Mutex::new(self.pending));

        let async_tx = state.async_tx.clone();
        let language_clone = state.language.clone();

        // Create channel for server-to-client request responses
        // The reader task will send responses through this channel
        let (server_response_tx, mut server_response_rx) = mpsc::channel::<JsonRpcResponse>(100);

        // Spawn stdout reader task - continuously reads and dispatches messages
        let pending_clone = pending.clone();
        let async_tx_reader = async_tx.clone();
        let language_clone_reader = language_clone.clone();
        tokio::spawn(async move {
            tracing::info!(
                "LSP stdout reader task started for {}",
                language_clone_reader
            );
            loop {
                match read_message_from_stdout(&mut stdout).await {
                    Ok(message) => {
                        tracing::debug!("Read message from LSP server: {:?}", message);
                        if let Err(e) = handle_message_dispatch(
                            message,
                            &pending_clone,
                            &async_tx_reader,
                            &language_clone_reader,
                            &server_response_tx,
                        )
                        .await
                        {
                            tracing::error!("Error handling LSP message: {}", e);
                        }
                    }
                    Err(e) => {
                        tracing::error!("Error reading from LSP server: {}", e);
                        let _ = async_tx_reader.send(AsyncMessage::LspStatusUpdate {
                            language: language_clone_reader.clone(),
                            status: LspServerStatus::Error,
                        });
                        let _ = async_tx_reader.send(AsyncMessage::LspError {
                            language: language_clone_reader.clone(),
                            error: format!("Read error: {}", e),
                        });
                        break;
                    }
                }
            }
            tracing::info!(
                "LSP stdout reader task exiting for {}",
                language_clone_reader
            );
        });

        // Sequential command processing loop with server response handling
        let mut pending_commands = Vec::new();
        loop {
            tokio::select! {
                // Handle server-to-client responses (high priority)
                Some(response) = server_response_rx.recv() => {
                    tracing::debug!("Sending response to server request id={}", response.id);
                    if let Err(e) = state.write_message(&response).await {
                        tracing::error!("Failed to send response to server: {}", e);
                    }
                }
                // Handle commands from the editor
                Some(cmd) = command_rx.recv() => {
                    tracing::debug!("LspTask received command: {:?}", cmd);
                    match cmd {
                        LspCommand::Initialize { root_uri, response } => {
                            // Send initializing status
                            let _ = async_tx.send(AsyncMessage::LspStatusUpdate {
                                language: language_clone.clone(),
                                status: LspServerStatus::Initializing,
                            });
                            tracing::info!("Processing Initialize command");
                            let result =
                                state.handle_initialize_sequential(root_uri, &pending).await;
                            let success = result.is_ok();
                            let _ = response.send(result);

                            // After successful initialization, replay pending commands
                            if success {
                                let queued = std::mem::take(&mut pending_commands);
                                if !queued.is_empty() {
                                    tracing::info!(
                                        "Replaying {} pending commands after initialization",
                                        queued.len()
                                    );
                                    for queued_cmd in queued {
                                        match queued_cmd {
                                            LspCommand::DidOpen {
                                                uri,
                                                text,
                                                language_id,
                                            } => {
                                                tracing::info!(
                                                    "Replaying DidOpen for {}",
                                                    uri.as_str()
                                                );
                                                let _ = state
                                                    .handle_did_open_sequential(
                                                        uri,
                                                        text,
                                                        language_id,
                                                        &pending,
                                                    )
                                                    .await;
                                            }
                                            LspCommand::DidChange {
                                                uri,
                                                content_changes,
                                            } => {
                                                tracing::info!(
                                                    "Replaying DidChange for {}",
                                                    uri.as_str()
                                                );
                                                let _ = state
                                                    .handle_did_change_sequential(
                                                        uri,
                                                        content_changes,
                                                        &pending,
                                                    )
                                                    .await;
                                            }
                                            LspCommand::DidSave { uri, text } => {
                                                tracing::info!(
                                                    "Replaying DidSave for {}",
                                                    uri.as_str()
                                                );
                                                let _ = state.handle_did_save(uri, text).await;
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                            }
                        }
                        LspCommand::DidOpen {
                            uri,
                            text,
                            language_id,
                        } => {
                            if state.initialized {
                                tracing::info!("Processing DidOpen for {}", uri.as_str());
                                let _ = state
                                    .handle_did_open_sequential(uri, text, language_id, &pending)
                                    .await;
                            } else {
                                tracing::debug!(
                                    "Queueing DidOpen for {} until initialization completes",
                                    uri.as_str()
                                );
                                pending_commands.push(LspCommand::DidOpen {
                                    uri,
                                    text,
                                    language_id,
                                });
                            }
                        }
                        LspCommand::DidChange {
                            uri,
                            content_changes,
                        } => {
                            if state.initialized {
                                tracing::debug!("Processing DidChange for {}", uri.as_str());
                                let _ = state
                                    .handle_did_change_sequential(uri, content_changes, &pending)
                                    .await;
                            } else {
                                tracing::debug!(
                                    "Queueing DidChange for {} until initialization completes",
                                    uri.as_str()
                                );
                                pending_commands.push(LspCommand::DidChange {
                                    uri,
                                    content_changes,
                                });
                            }
                        }
                        LspCommand::DidSave { uri, text } => {
                            if state.initialized {
                                tracing::info!("Processing DidSave for {}", uri.as_str());
                                let _ = state.handle_did_save(uri, text).await;
                            } else {
                                tracing::debug!(
                                    "Queueing DidSave for {} until initialization completes",
                                    uri.as_str()
                                );
                                pending_commands.push(LspCommand::DidSave { uri, text });
                            }
                        }
                        LspCommand::Completion {
                            request_id,
                            uri,
                            line,
                            character,
                        } => {
                            if state.initialized {
                                tracing::info!(
                                    "Processing Completion request for {}",
                                    uri.as_str()
                                );
                                let _ = state
                                    .handle_completion(request_id, uri, line, character, &pending)
                                    .await;
                            } else {
                                tracing::debug!("LSP not initialized, sending empty completion");
                                let _ = state.async_tx.send(AsyncMessage::LspCompletion {
                                    request_id,
                                    items: vec![],
                                });
                            }
                        }
                        LspCommand::GotoDefinition {
                            request_id,
                            uri,
                            line,
                            character,
                        } => {
                            if state.initialized {
                                tracing::info!(
                                    "Processing GotoDefinition request for {}",
                                    uri.as_str()
                                );
                                let _ = state
                                    .handle_goto_definition(
                                        request_id, uri, line, character, &pending,
                                    )
                                    .await;
                            } else {
                                tracing::debug!("LSP not initialized, sending empty locations");
                                let _ = state.async_tx.send(AsyncMessage::LspGotoDefinition {
                                    request_id,
                                    locations: vec![],
                                });
                            }
                        }
                        LspCommand::Rename {
                            request_id,
                            uri,
                            line,
                            character,
                            new_name,
                        } => {
                            if state.initialized {
                                tracing::info!("Processing Rename request for {}", uri.as_str());
                                let _ = state
                                    .handle_rename(
                                        request_id, uri, line, character, new_name, &pending,
                                    )
                                    .await;
                            } else {
                                tracing::debug!("LSP not initialized, cannot rename");
                                let _ = state.async_tx.send(AsyncMessage::LspRename {
                                    request_id,
                                    result: Err("LSP not initialized".to_string()),
                                });
                            }
                        }
                        LspCommand::Hover {
                            request_id,
                            uri,
                            line,
                            character,
                        } => {
                            if state.initialized {
                                tracing::info!("Processing Hover request for {}", uri.as_str());
                                let _ = state
                                    .handle_hover(request_id, uri, line, character, &pending)
                                    .await;
                            } else {
                                tracing::debug!("LSP not initialized, cannot get hover");
                                let _ = state.async_tx.send(AsyncMessage::LspHover {
                                    request_id,
                                    contents: String::new(),
                                    is_markdown: false,
                                    range: None,
                                });
                            }
                        }
                        LspCommand::CancelRequest { request_id } => {
                            tracing::info!(
                                "Processing CancelRequest for editor_id={}",
                                request_id
                            );
                            let _ = state.handle_cancel_request(request_id).await;
                        }
                        LspCommand::Shutdown => {
                            tracing::info!("Processing Shutdown command");
                            let _ = state.handle_shutdown().await;
                            break;
                        }
                    }
                }
                // Handle channel closure
                else => {
                    tracing::info!("Command channel closed");
                    break;
                }
            }
        }

        tracing::info!("LSP task exiting for language: {}", self.language);
    }

    /// Sequential version of handle_initialize that uses shared pending map
    async fn handle_initialize_sequential(
        &mut self,
        root_uri: Option<Uri>,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<InitializeResult, String> {
        tracing::info!(
            "Initializing async LSP server with root_uri: {:?}",
            root_uri
        );

        let workspace_folders = root_uri.as_ref().map(|uri| {
            vec![WorkspaceFolder {
                uri: uri.clone(),
                name: uri
                    .path()
                    .as_str()
                    .split('/')
                    .last()
                    .unwrap_or("workspace")
                    .to_string(),
            }]
        });

        let params = InitializeParams {
            process_id: Some(std::process::id()),
            capabilities: create_client_capabilities(),
            workspace_folders,
            ..Default::default()
        };

        let result: InitializeResult = self
            .send_request_sequential(Initialize::METHOD, Some(params), pending)
            .await?;

        self.capabilities = Some(result.capabilities.clone());

        // Send initialized notification
        self.send_notification::<Initialized>(InitializedParams {})
            .await?;

        self.initialized = true;

        // Notify main loop
        let _ = self.async_tx.send(AsyncMessage::LspInitialized {
            language: self.language.clone(),
        });

        // Send running status
        let _ = self.async_tx.send(AsyncMessage::LspStatusUpdate {
            language: self.language.clone(),
            status: LspServerStatus::Running,
        });

        tracing::info!("Async LSP server initialized successfully");

        Ok(result)
    }

    /// Sequential version of handle_did_open
    async fn handle_did_open_sequential(
        &mut self,
        uri: Uri,
        text: String,
        language_id: String,
        _pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        tracing::debug!("LSP: did_open for {}", uri.as_str());

        let params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id,
                version: 0,
                text,
            },
        };

        self.document_versions
            .insert(PathBuf::from(uri.path().as_str()), 0);

        self.send_notification::<DidOpenTextDocument>(params).await
    }

    /// Sequential version of handle_did_change
    async fn handle_did_change_sequential(
        &mut self,
        uri: Uri,
        content_changes: Vec<TextDocumentContentChangeEvent>,
        _pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        tracing::debug!("LSP: did_change for {}", uri.as_str());

        let path = PathBuf::from(uri.path().as_str());
        let version = self.document_versions.entry(path).or_insert(0);
        *version += 1;

        let params = DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: *version as i32,
            },
            content_changes,
        };

        self.send_notification::<DidChangeTextDocument>(params)
            .await
    }

    /// Send request using shared pending map (for sequential command processing)
    async fn send_request_sequential<P: Serialize, R: for<'de> Deserialize<'de>>(
        &mut self,
        method: &str,
        params: Option<P>,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<R, String> {
        let id = self.next_id;
        self.next_id += 1;

        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id,
            method: method.to_string(),
            params: params.map(|p| serde_json::to_value(p).expect("Failed to serialize params")),
        };

        let (tx, rx) = oneshot::channel();
        pending.lock().unwrap().insert(id, tx);

        self.write_message(&request).await?;

        tracing::debug!("Sent LSP request id={}, waiting for response...", id);

        // Await response (this is OK now because the reader task will send it)
        let result = rx
            .await
            .map_err(|_| "Response channel closed".to_string())??;

        tracing::debug!("Received LSP response for request id={}", id);

        serde_json::from_value(result).map_err(|e| format!("Failed to deserialize response: {}", e))
    }

    /// Handle shutdown command
    async fn handle_shutdown(&mut self) -> Result<(), String> {
        if !self.initialized {
            return Ok(());
        }

        tracing::info!("Shutting down async LSP server");

        // Send shutdown request
        let _: Value = self
            .send_request(Shutdown::METHOD, Option::<()>::None)
            .await?;

        // Send exit notification (manually, as Exit doesn't use the Notification trait)
        let notification = JsonRpcNotification {
            jsonrpc: "2.0".to_string(),
            method: "exit".to_string(),
            params: None,
        };
        self.write_message(&notification).await?;

        // Kill process
        let _ = self.process.kill().await;

        Ok(())
    }

    /// Send a request and await response
    async fn send_request<P: Serialize, R: for<'de> Deserialize<'de>>(
        &mut self,
        method: &str,
        params: Option<P>,
    ) -> Result<R, String> {
        let id = self.next_id;
        self.next_id += 1;

        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id,
            method: method.to_string(),
            params: params.map(|p| serde_json::to_value(p).expect("Failed to serialize params")),
        };

        let (tx, rx) = oneshot::channel();
        self.pending.insert(id, tx);

        self.write_message(&request).await?;

        // Await response
        let result = rx
            .await
            .map_err(|_| "Response channel closed".to_string())??;

        serde_json::from_value(result).map_err(|e| format!("Failed to deserialize response: {}", e))
    }

    /// Send a notification using lsp-types Notification trait (type-safe)
    async fn send_notification<N>(&mut self, params: N::Params) -> Result<(), String>
    where
        N: Notification,
    {
        let notification = JsonRpcNotification {
            jsonrpc: "2.0".to_string(),
            method: N::METHOD.to_string(),
            params: Some(serde_json::to_value(params).expect("Failed to serialize params")),
        };

        self.write_message(&notification).await
    }

    /// Write a message to stdin
    async fn write_message<T: Serialize>(&mut self, message: &T) -> Result<(), String> {
        let json =
            serde_json::to_string(message).map_err(|e| format!("Serialization error: {}", e))?;

        let content = format!("Content-Length: {}\r\n\r\n{}", json.len(), json);

        self.stdin
            .write_all(content.as_bytes())
            .await
            .map_err(|e| format!("Failed to write to stdin: {}", e))?;

        self.stdin
            .flush()
            .await
            .map_err(|e| format!("Failed to flush stdin: {}", e))?;

        tracing::trace!("Sent LSP message: {}", json);

        Ok(())
    }

    /// Read a message from stdout
    async fn read_message(&mut self) -> Result<JsonRpcMessage, String> {
        // Read headers
        let mut content_length: Option<usize> = None;

        loop {
            let mut line = String::new();
            self.stdout
                .read_line(&mut line)
                .await
                .map_err(|e| format!("Failed to read from stdout: {}", e))?;

            if line == "\r\n" {
                break;
            }

            if line.starts_with("Content-Length: ") {
                content_length = Some(
                    line[16..]
                        .trim()
                        .parse()
                        .map_err(|e| format!("Invalid Content-Length: {}", e))?,
                );
            }
        }

        let content_length =
            content_length.ok_or_else(|| "Missing Content-Length header".to_string())?;

        // Read content
        let mut content = vec![0u8; content_length];
        self.stdout
            .read_exact(&mut content)
            .await
            .map_err(|e| format!("Failed to read content: {}", e))?;

        let json = String::from_utf8(content).map_err(|e| format!("Invalid UTF-8: {}", e))?;

        tracing::trace!("Received LSP message: {}", json);

        serde_json::from_str(&json).map_err(|e| format!("Failed to deserialize message: {}", e))
    }

    /// Handle an incoming message
    async fn handle_message(&mut self, message: JsonRpcMessage) -> Result<(), String> {
        match message {
            JsonRpcMessage::Response(response) => {
                if let Some(tx) = self.pending.remove(&response.id) {
                    let result = if let Some(error) = response.error {
                        Err(format!(
                            "LSP error: {} (code {})",
                            error.message, error.code
                        ))
                    } else {
                        response
                            .result
                            .ok_or_else(|| "No result in response".to_string())
                    };
                    let _ = tx.send(result);
                }
            }
            JsonRpcMessage::Notification(notification) => {
                self.handle_notification(notification).await?;
            }
            JsonRpcMessage::Request(_) => {
                tracing::warn!("Received request from server, ignoring");
            }
        }
        Ok(())
    }

    /// Handle a notification from the server
    async fn handle_notification(
        &mut self,
        notification: JsonRpcNotification,
    ) -> Result<(), String> {
        match notification.method.as_str() {
            PublishDiagnostics::METHOD => {
                if let Some(params) = notification.params {
                    let params: PublishDiagnosticsParams = serde_json::from_value(params)
                        .map_err(|e| format!("Failed to deserialize diagnostics: {}", e))?;

                    tracing::debug!(
                        "Received {} diagnostics for {}",
                        params.diagnostics.len(),
                        params.uri.as_str()
                    );

                    // Send to main loop
                    let _ = self.async_tx.send(AsyncMessage::LspDiagnostics {
                        uri: params.uri.to_string(),
                        diagnostics: params.diagnostics,
                    });
                }
            }
            "window/showMessage" => {
                if let Some(params) = notification.params {
                    if let Ok(msg) =
                        serde_json::from_value::<serde_json::Map<String, Value>>(params)
                    {
                        let message_type_num =
                            msg.get("type").and_then(|v| v.as_i64()).unwrap_or(3);
                        let message = msg
                            .get("message")
                            .and_then(|v| v.as_str())
                            .unwrap_or("(no message)")
                            .to_string();

                        let message_type = match message_type_num {
                            1 => LspMessageType::Error,
                            2 => LspMessageType::Warning,
                            3 => LspMessageType::Info,
                            _ => LspMessageType::Log,
                        };

                        // Log it
                        match message_type {
                            LspMessageType::Error => tracing::error!("LSP: {}", message),
                            LspMessageType::Warning => tracing::warn!("LSP: {}", message),
                            LspMessageType::Info => tracing::info!("LSP: {}", message),
                            LspMessageType::Log => tracing::debug!("LSP: {}", message),
                        }

                        // Send to UI
                        let _ = self.async_tx.send(AsyncMessage::LspWindowMessage {
                            language: self.language.clone(),
                            message_type,
                            message,
                        });
                    }
                }
            }
            "window/logMessage" => {
                if let Some(params) = notification.params {
                    if let Ok(msg) =
                        serde_json::from_value::<serde_json::Map<String, Value>>(params)
                    {
                        let message_type_num =
                            msg.get("type").and_then(|v| v.as_i64()).unwrap_or(4);
                        let message = msg
                            .get("message")
                            .and_then(|v| v.as_str())
                            .unwrap_or("(no message)")
                            .to_string();

                        let message_type = match message_type_num {
                            1 => LspMessageType::Error,
                            2 => LspMessageType::Warning,
                            3 => LspMessageType::Info,
                            _ => LspMessageType::Log,
                        };

                        // Log it
                        match message_type {
                            LspMessageType::Error => tracing::error!("LSP: {}", message),
                            LspMessageType::Warning => tracing::warn!("LSP: {}", message),
                            LspMessageType::Info => tracing::info!("LSP: {}", message),
                            LspMessageType::Log => tracing::debug!("LSP: {}", message),
                        }

                        // Send to UI
                        let _ = self.async_tx.send(AsyncMessage::LspLogMessage {
                            language: self.language.clone(),
                            message_type,
                            message,
                        });
                    }
                }
            }
            "$/progress" => {
                if let Some(params) = notification.params {
                    if let Ok(progress) =
                        serde_json::from_value::<serde_json::Map<String, Value>>(params)
                    {
                        let token = progress
                            .get("token")
                            .and_then(|v| {
                                if let Some(s) = v.as_str() {
                                    Some(s.to_string())
                                } else if let Some(n) = v.as_i64() {
                                    Some(n.to_string())
                                } else {
                                    None
                                }
                            })
                            .unwrap_or_else(|| "unknown".to_string());

                        if let Some(value_obj) = progress.get("value").and_then(|v| v.as_object()) {
                            let kind = value_obj.get("kind").and_then(|v| v.as_str());

                            let value = match kind {
                                Some("begin") => {
                                    let title = value_obj
                                        .get("title")
                                        .and_then(|v| v.as_str())
                                        .unwrap_or("Working...")
                                        .to_string();
                                    let message = value_obj
                                        .get("message")
                                        .and_then(|v| v.as_str())
                                        .map(|s| s.to_string());
                                    let percentage = value_obj
                                        .get("percentage")
                                        .and_then(|v| v.as_u64())
                                        .map(|p| p as u32);

                                    tracing::info!(
                                        "LSP ({}) progress begin: {} {:?} {:?}",
                                        self.language,
                                        title,
                                        message,
                                        percentage
                                    );

                                    Some(LspProgressValue::Begin {
                                        title,
                                        message,
                                        percentage,
                                    })
                                }
                                Some("report") => {
                                    let message = value_obj
                                        .get("message")
                                        .and_then(|v| v.as_str())
                                        .map(|s| s.to_string());
                                    let percentage = value_obj
                                        .get("percentage")
                                        .and_then(|v| v.as_u64())
                                        .map(|p| p as u32);

                                    tracing::debug!(
                                        "LSP ({}) progress report: {:?} {:?}",
                                        self.language,
                                        message,
                                        percentage
                                    );

                                    Some(LspProgressValue::Report {
                                        message,
                                        percentage,
                                    })
                                }
                                Some("end") => {
                                    let message = value_obj
                                        .get("message")
                                        .and_then(|v| v.as_str())
                                        .map(|s| s.to_string());

                                    tracing::info!(
                                        "LSP ({}) progress end: {:?}",
                                        self.language,
                                        message
                                    );

                                    Some(LspProgressValue::End { message })
                                }
                                _ => None,
                            };

                            if let Some(value) = value {
                                let _ = self.async_tx.send(AsyncMessage::LspProgress {
                                    language: self.language.clone(),
                                    token,
                                    value,
                                });
                            }
                        }
                    }
                }
            }
            _ => {
                tracing::debug!("Unhandled notification: {}", notification.method);
            }
        }

        Ok(())
    }
}

/// Standalone function to read a message from stdout (for reader task)
async fn read_message_from_stdout(
    stdout: &mut BufReader<ChildStdout>,
) -> Result<JsonRpcMessage, String> {
    // Read headers
    let mut content_length: Option<usize> = None;

    loop {
        let mut line = String::new();
        let bytes_read = stdout
            .read_line(&mut line)
            .await
            .map_err(|e| format!("Failed to read from stdout: {}", e))?;

        // EOF detected - LSP server closed stdout
        if bytes_read == 0 {
            return Err("LSP server closed stdout (EOF)".to_string());
        }

        if line == "\r\n" {
            break;
        }

        if line.starts_with("Content-Length: ") {
            content_length = Some(
                line[16..]
                    .trim()
                    .parse()
                    .map_err(|e| format!("Invalid Content-Length: {}", e))?,
            );
        }
    }

    let content_length =
        content_length.ok_or_else(|| "Missing Content-Length header".to_string())?;

    // Read content
    let mut content = vec![0u8; content_length];
    stdout
        .read_exact(&mut content)
        .await
        .map_err(|e| format!("Failed to read content: {}", e))?;

    let json = String::from_utf8(content).map_err(|e| format!("Invalid UTF-8: {}", e))?;

    tracing::trace!("Received LSP message: {}", json);

    serde_json::from_str(&json).map_err(|e| format!("Failed to deserialize message: {}", e))
}

/// Standalone function to handle and dispatch messages (for reader task)
async fn handle_message_dispatch(
    message: JsonRpcMessage,
    pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    async_tx: &std_mpsc::Sender<AsyncMessage>,
    language: &str,
    server_response_tx: &mpsc::Sender<JsonRpcResponse>,
) -> Result<(), String> {
    match message {
        JsonRpcMessage::Response(response) => {
            tracing::debug!("Received LSP response for request id={}", response.id);
            if let Some(tx) = pending.lock().unwrap().remove(&response.id) {
                let result = if let Some(error) = response.error {
                    tracing::warn!(
                        "LSP response error: {} (code {})",
                        error.message,
                        error.code
                    );
                    Err(format!(
                        "LSP error: {} (code {})",
                        error.message, error.code
                    ))
                } else {
                    tracing::debug!("LSP response success for request id={}", response.id);
                    response
                        .result
                        .ok_or_else(|| "No result in response".to_string())
                };
                let _ = tx.send(result);
            } else {
                tracing::warn!(
                    "Received LSP response for unknown request id={}",
                    response.id
                );
            }
        }
        JsonRpcMessage::Notification(notification) => {
            tracing::debug!("Received LSP notification: {}", notification.method);
            handle_notification_dispatch(notification, async_tx, language).await?;
        }
        JsonRpcMessage::Request(request) => {
            // Handle server-to-client requests - MUST respond to avoid timeouts
            tracing::debug!("Received request from server: {}", request.method);
            let response = match request.method.as_str() {
                "window/workDoneProgress/create" => {
                    // Server wants to create a progress token - acknowledge it
                    tracing::debug!("Acknowledging workDoneProgress/create (id={})", request.id);
                    JsonRpcResponse {
                        jsonrpc: "2.0".to_string(),
                        id: request.id,
                        result: Some(Value::Null),
                        error: None,
                    }
                }
                "workspace/configuration" => {
                    // Return empty configuration - rust-analyzer will use defaults
                    tracing::debug!("Responding to workspace/configuration with empty config");
                    JsonRpcResponse {
                        jsonrpc: "2.0".to_string(),
                        id: request.id,
                        result: Some(Value::Array(vec![])),
                        error: None,
                    }
                }
                "client/registerCapability" => {
                    // Server wants to register a capability dynamically - acknowledge
                    tracing::debug!("Acknowledging client/registerCapability (id={})", request.id);
                    JsonRpcResponse {
                        jsonrpc: "2.0".to_string(),
                        id: request.id,
                        result: Some(Value::Null),
                        error: None,
                    }
                }
                _ => {
                    // For unknown methods, return null to acknowledge receipt
                    tracing::warn!("Unhandled server request: {}", request.method);
                    JsonRpcResponse {
                        jsonrpc: "2.0".to_string(),
                        id: request.id,
                        result: Some(Value::Null),
                        error: None,
                    }
                }
            };

            // Send response through channel to be written to stdin
            if let Err(e) = server_response_tx.send(response).await {
                tracing::error!("Failed to queue server response: {}", e);
            }
        }
    }
    Ok(())
}

/// Standalone function to handle notifications (for reader task)
async fn handle_notification_dispatch(
    notification: JsonRpcNotification,
    async_tx: &std_mpsc::Sender<AsyncMessage>,
    language: &str,
) -> Result<(), String> {
    match notification.method.as_str() {
        PublishDiagnostics::METHOD => {
            if let Some(params) = notification.params {
                let params: PublishDiagnosticsParams = serde_json::from_value(params)
                    .map_err(|e| format!("Failed to deserialize diagnostics: {}", e))?;

                tracing::debug!(
                    "Received {} diagnostics for {}",
                    params.diagnostics.len(),
                    params.uri.as_str()
                );

                // Send to main loop
                let _ = async_tx.send(AsyncMessage::LspDiagnostics {
                    uri: params.uri.to_string(),
                    diagnostics: params.diagnostics,
                });
            }
        }
        "window/showMessage" => {
            if let Some(params) = notification.params {
                if let Ok(msg) = serde_json::from_value::<serde_json::Map<String, Value>>(params) {
                    let message_type_num = msg.get("type").and_then(|v| v.as_i64()).unwrap_or(3);
                    let message = msg
                        .get("message")
                        .and_then(|v| v.as_str())
                        .unwrap_or("(no message)")
                        .to_string();

                    let message_type = match message_type_num {
                        1 => LspMessageType::Error,
                        2 => LspMessageType::Warning,
                        3 => LspMessageType::Info,
                        _ => LspMessageType::Log,
                    };

                    // Log it as well
                    match message_type {
                        LspMessageType::Error => tracing::error!("LSP ({}): {}", language, message),
                        LspMessageType::Warning => {
                            tracing::warn!("LSP ({}): {}", language, message)
                        }
                        LspMessageType::Info => tracing::info!("LSP ({}): {}", language, message),
                        LspMessageType::Log => tracing::debug!("LSP ({}): {}", language, message),
                    }

                    // Send to UI
                    let _ = async_tx.send(AsyncMessage::LspWindowMessage {
                        language: language.to_string(),
                        message_type,
                        message,
                    });
                }
            }
        }
        "window/logMessage" => {
            if let Some(params) = notification.params {
                if let Ok(msg) = serde_json::from_value::<serde_json::Map<String, Value>>(params) {
                    let message_type_num = msg.get("type").and_then(|v| v.as_i64()).unwrap_or(4);
                    let message = msg
                        .get("message")
                        .and_then(|v| v.as_str())
                        .unwrap_or("(no message)")
                        .to_string();

                    let message_type = match message_type_num {
                        1 => LspMessageType::Error,
                        2 => LspMessageType::Warning,
                        3 => LspMessageType::Info,
                        _ => LspMessageType::Log,
                    };

                    // Log it as well
                    match message_type {
                        LspMessageType::Error => tracing::error!("LSP ({}): {}", language, message),
                        LspMessageType::Warning => {
                            tracing::warn!("LSP ({}): {}", language, message)
                        }
                        LspMessageType::Info => tracing::info!("LSP ({}): {}", language, message),
                        LspMessageType::Log => tracing::debug!("LSP ({}): {}", language, message),
                    }

                    // Send to UI
                    let _ = async_tx.send(AsyncMessage::LspLogMessage {
                        language: language.to_string(),
                        message_type,
                        message,
                    });
                }
            }
        }
        "$/progress" => {
            if let Some(params) = notification.params {
                if let Ok(progress) =
                    serde_json::from_value::<serde_json::Map<String, Value>>(params)
                {
                    let token = progress
                        .get("token")
                        .and_then(|v| {
                            if let Some(s) = v.as_str() {
                                Some(s.to_string())
                            } else if let Some(n) = v.as_i64() {
                                Some(n.to_string())
                            } else {
                                None
                            }
                        })
                        .unwrap_or_else(|| "unknown".to_string());

                    if let Some(value_obj) = progress.get("value").and_then(|v| v.as_object()) {
                        let kind = value_obj.get("kind").and_then(|v| v.as_str());

                        let value = match kind {
                            Some("begin") => {
                                let title = value_obj
                                    .get("title")
                                    .and_then(|v| v.as_str())
                                    .unwrap_or("Working...")
                                    .to_string();
                                let message = value_obj
                                    .get("message")
                                    .and_then(|v| v.as_str())
                                    .map(|s| s.to_string());
                                let percentage = value_obj
                                    .get("percentage")
                                    .and_then(|v| v.as_u64())
                                    .map(|p| p as u32);

                                tracing::info!(
                                    "LSP ({}) progress begin: {} {:?} {:?}",
                                    language,
                                    title,
                                    message,
                                    percentage
                                );

                                Some(LspProgressValue::Begin {
                                    title,
                                    message,
                                    percentage,
                                })
                            }
                            Some("report") => {
                                let message = value_obj
                                    .get("message")
                                    .and_then(|v| v.as_str())
                                    .map(|s| s.to_string());
                                let percentage = value_obj
                                    .get("percentage")
                                    .and_then(|v| v.as_u64())
                                    .map(|p| p as u32);

                                tracing::debug!(
                                    "LSP ({}) progress report: {:?} {:?}",
                                    language,
                                    message,
                                    percentage
                                );

                                Some(LspProgressValue::Report {
                                    message,
                                    percentage,
                                })
                            }
                            Some("end") => {
                                let message = value_obj
                                    .get("message")
                                    .and_then(|v| v.as_str())
                                    .map(|s| s.to_string());

                                tracing::info!("LSP ({}) progress end: {:?}", language, message);

                                Some(LspProgressValue::End { message })
                            }
                            _ => None,
                        };

                        if let Some(value) = value {
                            let _ = async_tx.send(AsyncMessage::LspProgress {
                                language: language.to_string(),
                                token,
                                value,
                            });
                        }
                    }
                }
            }
        }
        _ => {
            tracing::debug!("Unhandled notification: {}", notification.method);
        }
    }

    Ok(())
}

/// Synchronous handle to an async LSP task
pub struct LspHandle {
    /// Channel for sending commands to the task
    command_tx: mpsc::Sender<LspCommand>,

    /// Client state
    state: Arc<Mutex<LspClientState>>,

    /// Runtime handle for blocking operations
    runtime: tokio::runtime::Handle,
}

impl LspHandle {
    /// Spawn a new LSP server in an async task
    pub fn spawn(
        runtime: &tokio::runtime::Handle,
        command: &str,
        args: &[String],
        language: String,
        async_bridge: &AsyncBridge,
        process_limits: ProcessLimits,
    ) -> Result<Self, String> {
        let (command_tx, command_rx) = mpsc::channel(100); // Buffer up to 100 commands
        let async_tx = async_bridge.sender();
        let language_clone = language.clone();
        let command = command.to_string();
        let args = args.to_vec();
        let state = Arc::new(Mutex::new(LspClientState::Starting));

        // Send starting status
        let _ = async_tx.send(AsyncMessage::LspStatusUpdate {
            language: language.clone(),
            status: LspServerStatus::Starting,
        });

        let state_clone = state.clone();
        runtime.spawn(async move {
            match LspTask::spawn(
                &command,
                &args,
                language_clone.clone(),
                async_tx.clone(),
                &process_limits,
            )
            .await
            {
                Ok(task) => {
                    task.run(command_rx).await;
                }
                Err(e) => {
                    tracing::error!("Failed to spawn LSP task: {}", e);

                    // Transition to error state
                    if let Ok(mut s) = state_clone.lock() {
                        let _ = s.transition_to(LspClientState::Error);
                    }

                    let _ = async_tx.send(AsyncMessage::LspStatusUpdate {
                        language: language_clone.clone(),
                        status: LspServerStatus::Error,
                    });
                    let _ = async_tx.send(AsyncMessage::LspError {
                        language: language_clone,
                        error: e,
                    });
                }
            }
        });

        Ok(Self {
            command_tx,
            state,
            runtime: runtime.clone(),
        })
    }

    /// Initialize the server (non-blocking)
    ///
    /// This sends the initialize request asynchronously. The server will be ready
    /// when `is_initialized()` returns true. Other methods that require initialization
    /// will fail gracefully until then.
    pub fn initialize(&self, root_uri: Option<Uri>) -> Result<(), String> {
        // Validate state transition
        {
            let mut state = self.state.lock().unwrap();
            if !state.can_initialize() {
                return Err(format!(
                    "Cannot initialize: client is in state {:?}",
                    *state
                ));
            }
            // Transition to Initializing
            state.transition_to(LspClientState::Initializing)?;
        }

        let state = self.state.clone();

        // Create a channel for the response, but don't wait for it
        let (tx, rx) = oneshot::channel();

        self.command_tx
            .try_send(LspCommand::Initialize {
                root_uri,
                response: tx,
            })
            .map_err(|_| "Failed to send initialize command".to_string())?;

        // Spawn a task to wait for the response and update the state
        let runtime = self.runtime.clone();
        runtime.spawn(async move {
            match tokio::time::timeout(std::time::Duration::from_secs(10), rx).await {
                Ok(Ok(Ok(_))) => {
                    // Successfully initialized
                    if let Ok(mut s) = state.lock() {
                        let _ = s.transition_to(LspClientState::Running);
                    }
                    tracing::info!("LSP initialization completed successfully");
                }
                Ok(Ok(Err(e))) => {
                    tracing::error!("LSP initialization failed: {}", e);
                    if let Ok(mut s) = state.lock() {
                        let _ = s.transition_to(LspClientState::Error);
                    }
                }
                Ok(Err(_)) => {
                    tracing::error!("LSP initialization response channel closed");
                    if let Ok(mut s) = state.lock() {
                        let _ = s.transition_to(LspClientState::Error);
                    }
                }
                Err(_) => {
                    tracing::error!("LSP initialization timed out after 10 seconds");
                    if let Ok(mut s) = state.lock() {
                        let _ = s.transition_to(LspClientState::Error);
                    }
                }
            }
        });

        Ok(())
    }

    /// Check if the server is initialized
    pub fn is_initialized(&self) -> bool {
        self.state.lock().unwrap().can_send_requests()
    }

    /// Get the current client state
    pub fn state(&self) -> LspClientState {
        *self.state.lock().unwrap()
    }

    /// Notify document opened
    pub fn did_open(&self, uri: Uri, text: String, language_id: String) -> Result<(), String> {
        // Send command to LspTask which will queue it if not initialized yet
        self.command_tx
            .try_send(LspCommand::DidOpen {
                uri,
                text,
                language_id,
            })
            .map_err(|_| "Failed to send did_open command".to_string())
    }

    /// Notify document changed
    pub fn did_change(
        &self,
        uri: Uri,
        content_changes: Vec<TextDocumentContentChangeEvent>,
    ) -> Result<(), String> {
        // Send command to LspTask which will queue it if not initialized yet
        self.command_tx
            .try_send(LspCommand::DidChange {
                uri,
                content_changes,
            })
            .map_err(|_| "Failed to send did_change command".to_string())
    }

    /// Send didSave notification
    pub fn did_save(&self, uri: Uri, text: Option<String>) -> Result<(), String> {
        self.command_tx
            .try_send(LspCommand::DidSave { uri, text })
            .map_err(|_| "Failed to send did_save command".to_string())
    }

    /// Request completion at position
    pub fn completion(
        &self,
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
    ) -> Result<(), String> {
        self.command_tx
            .try_send(LspCommand::Completion {
                request_id,
                uri,
                line,
                character,
            })
            .map_err(|_| "Failed to send completion command".to_string())
    }

    /// Request go-to-definition
    pub fn goto_definition(
        &self,
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
    ) -> Result<(), String> {
        self.command_tx
            .try_send(LspCommand::GotoDefinition {
                request_id,
                uri,
                line,
                character,
            })
            .map_err(|_| "Failed to send goto_definition command".to_string())
    }

    /// Request rename
    pub fn rename(
        &self,
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
        new_name: String,
    ) -> Result<(), String> {
        self.command_tx
            .try_send(LspCommand::Rename {
                request_id,
                uri,
                line,
                character,
                new_name,
            })
            .map_err(|_| "Failed to send rename command".to_string())
    }

    /// Request hover documentation
    pub fn hover(
        &self,
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
    ) -> Result<(), String> {
        self.command_tx
            .try_send(LspCommand::Hover {
                request_id,
                uri,
                line,
                character,
            })
            .map_err(|_| "Failed to send hover command".to_string())
    }

    /// Cancel a pending request by its editor request_id
    ///
    /// This sends a $/cancelRequest notification to the LSP server.
    /// If the request has already completed or doesn't exist, this is a no-op.
    pub fn cancel_request(&self, request_id: u64) -> Result<(), String> {
        self.command_tx
            .try_send(LspCommand::CancelRequest { request_id })
            .map_err(|_| "Failed to send cancel_request command".to_string())
    }

    /// Shutdown the server
    pub fn shutdown(&self) -> Result<(), String> {
        // Transition to Stopping state
        {
            let mut state = self.state.lock().unwrap();
            if let Err(e) = state.transition_to(LspClientState::Stopping) {
                tracing::warn!("State transition warning during shutdown: {}", e);
                // Don't fail shutdown due to state transition errors
            }
        }

        self.command_tx
            .try_send(LspCommand::Shutdown)
            .map_err(|_| "Failed to send shutdown command".to_string())?;

        // Transition to Stopped state
        // Note: This happens optimistically. The actual shutdown might take time.
        {
            let mut state = self.state.lock().unwrap();
            let _ = state.transition_to(LspClientState::Stopped);
        }

        Ok(())
    }
}

impl Drop for LspHandle {
    fn drop(&mut self) {
        // Best-effort shutdown on drop
        // Use try_send instead of blocking_send to avoid panicking if:
        // 1. The tokio runtime is shut down
        // 2. The channel is full or closed
        // 3. We're dropping during a panic
        let _ = self.command_tx.try_send(LspCommand::Shutdown);

        // Update state to Stopped
        if let Ok(mut state) = self.state.lock() {
            let _ = state.transition_to(LspClientState::Stopped);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_json_rpc_request_serialization() {
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: 1,
            method: "initialize".to_string(),
            params: Some(serde_json::json!({"rootUri": "file:///test"})),
        };

        let json = serde_json::to_string(&request).unwrap();
        assert!(json.contains("\"jsonrpc\":\"2.0\""));
        assert!(json.contains("\"id\":1"));
        assert!(json.contains("\"method\":\"initialize\""));
        assert!(json.contains("\"rootUri\":\"file:///test\""));
    }

    #[test]
    fn test_json_rpc_response_serialization() {
        let response = JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: 1,
            result: Some(serde_json::json!({"success": true})),
            error: None,
        };

        let json = serde_json::to_string(&response).unwrap();
        assert!(json.contains("\"jsonrpc\":\"2.0\""));
        assert!(json.contains("\"id\":1"));
        assert!(json.contains("\"success\":true"));
        assert!(!json.contains("\"error\""));
    }

    #[test]
    fn test_json_rpc_error_response() {
        let response = JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: 1,
            result: None,
            error: Some(JsonRpcError {
                code: -32600,
                message: "Invalid request".to_string(),
                data: None,
            }),
        };

        let json = serde_json::to_string(&response).unwrap();
        assert!(json.contains("\"error\""));
        assert!(json.contains("\"code\":-32600"));
        assert!(json.contains("\"message\":\"Invalid request\""));
    }

    #[test]
    fn test_json_rpc_notification_serialization() {
        let notification = JsonRpcNotification {
            jsonrpc: "2.0".to_string(),
            method: "textDocument/didOpen".to_string(),
            params: Some(serde_json::json!({"uri": "file:///test.rs"})),
        };

        let json = serde_json::to_string(&notification).unwrap();
        assert!(json.contains("\"jsonrpc\":\"2.0\""));
        assert!(json.contains("\"method\":\"textDocument/didOpen\""));
        assert!(json.contains("\"uri\":\"file:///test.rs\""));
        assert!(!json.contains("\"id\"")); // Notifications have no ID
    }

    #[test]
    fn test_json_rpc_message_deserialization_request() {
        let json =
            r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"rootUri":"file:///test"}}"#;
        let message: JsonRpcMessage = serde_json::from_str(json).unwrap();

        match message {
            JsonRpcMessage::Request(request) => {
                assert_eq!(request.jsonrpc, "2.0");
                assert_eq!(request.id, 1);
                assert_eq!(request.method, "initialize");
                assert!(request.params.is_some());
            }
            _ => panic!("Expected Request"),
        }
    }

    #[test]
    fn test_json_rpc_message_deserialization_response() {
        let json = r#"{"jsonrpc":"2.0","id":1,"result":{"success":true}}"#;
        let message: JsonRpcMessage = serde_json::from_str(json).unwrap();

        match message {
            JsonRpcMessage::Response(response) => {
                assert_eq!(response.jsonrpc, "2.0");
                assert_eq!(response.id, 1);
                assert!(response.result.is_some());
                assert!(response.error.is_none());
            }
            _ => panic!("Expected Response"),
        }
    }

    #[test]
    fn test_json_rpc_message_deserialization_notification() {
        let json = r#"{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"uri":"file:///test.rs"}}"#;
        let message: JsonRpcMessage = serde_json::from_str(json).unwrap();

        match message {
            JsonRpcMessage::Notification(notification) => {
                assert_eq!(notification.jsonrpc, "2.0");
                assert_eq!(notification.method, "textDocument/didOpen");
                assert!(notification.params.is_some());
            }
            _ => panic!("Expected Notification"),
        }
    }

    #[test]
    fn test_json_rpc_error_deserialization() {
        let json =
            r#"{"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"Invalid request"}}"#;
        let message: JsonRpcMessage = serde_json::from_str(json).unwrap();

        match message {
            JsonRpcMessage::Response(response) => {
                assert_eq!(response.jsonrpc, "2.0");
                assert_eq!(response.id, 1);
                assert!(response.result.is_none());
                assert!(response.error.is_some());
                let error = response.error.unwrap();
                assert_eq!(error.code, -32600);
                assert_eq!(error.message, "Invalid request");
            }
            _ => panic!("Expected Response with error"),
        }
    }

    #[tokio::test]
    async fn test_lsp_handle_spawn_and_drop() {
        // This test spawns a mock LSP server (cat command that echoes input)
        // and tests the spawn/drop lifecycle
        let runtime = tokio::runtime::Handle::current();
        let async_bridge = AsyncBridge::new();

        // Use 'cat' as a mock LSP server (it will just echo stdin to stdout)
        // This will fail to initialize but allows us to test the spawn mechanism
        let result = LspHandle::spawn(
            &runtime,
            "cat",
            &[],
            "test".to_string(),
            &async_bridge,
            ProcessLimits::unlimited(),
        );

        // Should succeed in spawning
        assert!(result.is_ok());

        let handle = result.unwrap();

        // Let handle drop (which calls shutdown via Drop impl)
        drop(handle);

        // Give task time to receive shutdown and exit
        tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
    }

    #[tokio::test]
    async fn test_lsp_handle_did_open_queues_before_initialization() {
        let runtime = tokio::runtime::Handle::current();
        let async_bridge = AsyncBridge::new();

        let handle = LspHandle::spawn(
            &runtime,
            "cat",
            &[],
            "test".to_string(),
            &async_bridge,
            ProcessLimits::unlimited(),
        )
        .unwrap();

        // did_open now succeeds and queues the command for when server is initialized
        let result = handle.did_open(
            "file:///test.rs".parse().unwrap(),
            "fn main() {}".to_string(),
            "rust".to_string(),
        );

        // Should succeed (command is queued)
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_lsp_handle_did_change_queues_before_initialization() {
        let runtime = tokio::runtime::Handle::current();
        let async_bridge = AsyncBridge::new();

        let handle = LspHandle::spawn(
            &runtime,
            "cat",
            &[],
            "test".to_string(),
            &async_bridge,
            ProcessLimits::unlimited(),
        )
        .unwrap();

        // Test incremental sync: insert "fn main() {}" at position (0, 0)
        let result = handle.did_change(
            "file:///test.rs".parse().unwrap(),
            vec![TextDocumentContentChangeEvent {
                range: Some(lsp_types::Range::new(
                    lsp_types::Position::new(0, 0),
                    lsp_types::Position::new(0, 0),
                )),
                range_length: None,
                text: "fn main() {}".to_string(),
            }],
        );

        // Should succeed (command is queued)
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_lsp_handle_incremental_change_with_range() {
        let runtime = tokio::runtime::Handle::current();
        let async_bridge = AsyncBridge::new();

        let handle = LspHandle::spawn(
            &runtime,
            "cat",
            &[],
            "test".to_string(),
            &async_bridge,
            ProcessLimits::unlimited(),
        )
        .unwrap();

        // Test incremental delete: remove text from (0, 3) to (0, 7)
        let result = handle.did_change(
            "file:///test.rs".parse().unwrap(),
            vec![TextDocumentContentChangeEvent {
                range: Some(lsp_types::Range::new(
                    lsp_types::Position::new(0, 3),
                    lsp_types::Position::new(0, 7),
                )),
                range_length: None,
                text: String::new(), // Empty string for deletion
            }],
        );

        // Should succeed (command is queued)
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_lsp_handle_spawn_invalid_command() {
        let runtime = tokio::runtime::Handle::current();
        let async_bridge = AsyncBridge::new();

        // Try to spawn with an invalid command
        let result = LspHandle::spawn(
            &runtime,
            "this-command-does-not-exist-12345",
            &[],
            "test".to_string(),
            &async_bridge,
            ProcessLimits::unlimited(),
        );

        // Should succeed in creating handle (error happens asynchronously)
        // The error will be sent to async_bridge
        assert!(result.is_ok());

        // Give the task time to fail
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

        // Check that we received an error message
        let messages = async_bridge.try_recv_all();
        assert!(!messages.is_empty());

        let has_error = messages
            .iter()
            .any(|msg| matches!(msg, AsyncMessage::LspError { .. }));
        assert!(has_error, "Expected LspError message");
    }

    #[test]
    fn test_lsp_handle_shutdown_from_sync_context() {
        // Test shutdown from a synchronous context (requires spawning a separate thread)
        // This simulates how shutdown is called from the main editor loop
        std::thread::spawn(|| {
            // Create a tokio runtime for this thread
            let rt = tokio::runtime::Runtime::new().unwrap();
            let async_bridge = AsyncBridge::new();

            let handle = rt.block_on(async {
                let runtime = tokio::runtime::Handle::current();
                LspHandle::spawn(
                    &runtime,
                    "cat",
                    &[],
                    "test".to_string(),
                    &async_bridge,
                    ProcessLimits::unlimited(),
                )
                .unwrap()
            });

            // This should succeed from a non-async context
            assert!(handle.shutdown().is_ok());

            // Give task time to exit
            std::thread::sleep(std::time::Duration::from_millis(50));
        })
        .join()
        .unwrap();
    }

    #[test]
    fn test_lsp_command_debug_format() {
        // Test that LspCommand has Debug implementation
        let cmd = LspCommand::Shutdown;
        let debug_str = format!("{:?}", cmd);
        assert!(debug_str.contains("Shutdown"));
    }

    #[test]
    fn test_lsp_client_state_can_initialize_from_starting() {
        // This test verifies that the state machine allows initialization from the Starting state.
        // This is critical because LspHandle::spawn() sets state to Starting, and then
        // get_or_spawn() immediately calls handle.initialize(). Without this fix,
        // initialization would fail with "Cannot initialize: client is in state Starting".

        let state = LspClientState::Starting;

        // The fix: Starting state should allow initialization
        assert!(
            state.can_initialize(),
            "Starting state must allow initialization to avoid race condition"
        );

        // Verify the full initialization flow works
        let mut state = LspClientState::Starting;

        // Should be able to transition to Initializing
        assert!(state.can_transition_to(LspClientState::Initializing));
        assert!(state.transition_to(LspClientState::Initializing).is_ok());

        // And then to Running
        assert!(state.can_transition_to(LspClientState::Running));
        assert!(state.transition_to(LspClientState::Running).is_ok());
    }

    #[tokio::test]
    async fn test_lsp_handle_initialize_from_starting_state() {
        // This test reproduces the bug where initialize() would fail because
        // the handle's state is Starting (set by spawn()) but can_initialize()
        // only allowed Initial or Stopped states.
        //
        // The bug manifested as:
        // ERROR: Failed to send initialize command for rust: Cannot initialize: client is in state Starting

        let runtime = tokio::runtime::Handle::current();
        let async_bridge = AsyncBridge::new();

        // Spawn creates the handle with state = Starting
        let handle = LspHandle::spawn(
            &runtime,
            "cat", // Simple command that will exit immediately
            &[],
            "test".to_string(),
            &async_bridge,
            ProcessLimits::unlimited(),
        )
        .unwrap();

        // Immediately call initialize - this is what get_or_spawn() does
        // Before the fix, this would fail with "Cannot initialize: client is in state Starting"
        let result = handle.initialize(None);

        assert!(
            result.is_ok(),
            "initialize() must succeed from Starting state. Got error: {:?}",
            result.err()
        );
    }

    #[tokio::test]
    async fn test_lsp_state_machine_race_condition_fix() {
        // Integration test that simulates the exact flow that was broken:
        // 1. LspManager::get_or_spawn() calls LspHandle::spawn()
        // 2. spawn() sets state to Starting and spawns async task
        // 3. get_or_spawn() immediately calls handle.initialize()
        // 4. initialize() should succeed even though state is Starting

        let runtime = tokio::runtime::Handle::current();
        let async_bridge = AsyncBridge::new();

        // Create a simple fake LSP server script that responds to initialize
        let fake_lsp_script = r#"
            read -r line  # Read Content-Length header
            read -r empty # Read empty line
            read -r json  # Read JSON body

            # Send a valid initialize response
            response='{"jsonrpc":"2.0","id":1,"result":{"capabilities":{}}}'
            echo "Content-Length: ${#response}"
            echo ""
            echo -n "$response"

            # Keep running to avoid EOF
            sleep 10
        "#;

        // Spawn with bash to execute the fake LSP
        let handle = LspHandle::spawn(
            &runtime,
            "bash",
            &["-c".to_string(), fake_lsp_script.to_string()],
            "fake".to_string(),
            &async_bridge,
            ProcessLimits::unlimited(),
        )
        .unwrap();

        // This is the critical test: initialize must succeed from Starting state
        let init_result = handle.initialize(None);
        assert!(
            init_result.is_ok(),
            "initialize() failed from Starting state: {:?}",
            init_result.err()
        );

        // Give the async task time to process
        tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;

        // Check that we received status update messages
        let messages = async_bridge.try_recv_all();
        let has_status_update = messages
            .iter()
            .any(|msg| matches!(msg, AsyncMessage::LspStatusUpdate { .. }));

        assert!(
            has_status_update,
            "Expected status update messages from LSP initialization"
        );

        // Cleanup
        let _ = handle.shutdown();
    }
}

