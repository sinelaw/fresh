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

use crate::services::async_bridge::{
    AsyncBridge, AsyncMessage, LspMessageType, LspProgressValue, LspSemanticTokensResponse,
    LspServerStatus,
};
use crate::services::process_limits::ProcessLimits;
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidOpenTextDocument, DidSaveTextDocument, Initialized, Notification,
        PublishDiagnostics,
    },
    request::{Initialize, Request},
    ClientCapabilities, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, InitializeParams, InitializeResult, InitializedParams,
    PublishDiagnosticsParams, SemanticTokenModifier, SemanticTokenType,
    SemanticTokensClientCapabilities, SemanticTokensClientCapabilitiesRequests,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensParams, SemanticTokensResult,
    SemanticTokensServerCapabilities, ServerCapabilities, TextDocumentContentChangeEvent,
    TextDocumentIdentifier, TextDocumentItem, TokenFormat, Uri, VersionedTextDocumentIdentifier,
    WindowClientCapabilities, WorkspaceFolder,
};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{mpsc as std_mpsc, Arc, Mutex};
use std::time::Instant;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::process::{Child, ChildStdin, ChildStdout, Command};
use tokio::sync::{mpsc, oneshot};

/// Grace period after didOpen before sending didChange (in milliseconds)
/// This gives the LSP server time to process didOpen before receiving changes
const DID_OPEN_GRACE_PERIOD_MS: u64 = 200;

/// Check if a document is already open and should skip didOpen.
/// Returns true if the document is already open (should skip), false if it should proceed.
fn should_skip_did_open(
    document_versions: &HashMap<PathBuf, i64>,
    path: &PathBuf,
    language: &str,
    uri: &Uri,
) -> bool {
    if document_versions.contains_key(path) {
        tracing::debug!(
            "LSP ({}): skipping didOpen - document already open: {}",
            language,
            uri.as_str()
        );
        true
    } else {
        false
    }
}

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
        matches!(self, Self::Running)
    }

    /// Check if the client can accept initialization
    pub fn can_initialize(&self) -> bool {
        matches!(self, Self::Initial | Self::Starting | Self::Stopped)
    }

    /// Convert to LspServerStatus for UI reporting
    pub fn to_server_status(&self) -> LspServerStatus {
        match self {
            Self::Initial => LspServerStatus::Starting,
            Self::Starting => LspServerStatus::Starting,
            Self::Initializing => LspServerStatus::Initializing,
            Self::Running => LspServerStatus::Running,
            Self::Stopping => LspServerStatus::Shutdown,
            Self::Stopped => LspServerStatus::Shutdown,
            Self::Error => LspServerStatus::Error,
        }
    }
}

/// Create common LSP client capabilities with workDoneProgress support
fn create_client_capabilities() -> ClientCapabilities {
    use lsp_types::{
        GeneralClientCapabilities, RenameClientCapabilities, TextDocumentClientCapabilities,
        WorkspaceClientCapabilities, WorkspaceEditClientCapabilities,
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
            semantic_tokens: Some(SemanticTokensClientCapabilities {
                dynamic_registration: Some(true),
                requests: SemanticTokensClientCapabilitiesRequests {
                    range: Some(true),
                    full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),
                },
                token_types: vec![
                    SemanticTokenType::NAMESPACE,
                    SemanticTokenType::TYPE,
                    SemanticTokenType::CLASS,
                    SemanticTokenType::ENUM,
                    SemanticTokenType::INTERFACE,
                    SemanticTokenType::STRUCT,
                    SemanticTokenType::TYPE_PARAMETER,
                    SemanticTokenType::PARAMETER,
                    SemanticTokenType::VARIABLE,
                    SemanticTokenType::PROPERTY,
                    SemanticTokenType::ENUM_MEMBER,
                    SemanticTokenType::EVENT,
                    SemanticTokenType::FUNCTION,
                    SemanticTokenType::METHOD,
                    SemanticTokenType::MACRO,
                    SemanticTokenType::KEYWORD,
                    SemanticTokenType::MODIFIER,
                    SemanticTokenType::COMMENT,
                    SemanticTokenType::STRING,
                    SemanticTokenType::NUMBER,
                    SemanticTokenType::REGEXP,
                    SemanticTokenType::OPERATOR,
                    SemanticTokenType::DECORATOR,
                ],
                token_modifiers: vec![
                    SemanticTokenModifier::DECLARATION,
                    SemanticTokenModifier::DEFINITION,
                    SemanticTokenModifier::READONLY,
                    SemanticTokenModifier::STATIC,
                    SemanticTokenModifier::DEPRECATED,
                    SemanticTokenModifier::ABSTRACT,
                    SemanticTokenModifier::ASYNC,
                    SemanticTokenModifier::MODIFICATION,
                    SemanticTokenModifier::DOCUMENTATION,
                    SemanticTokenModifier::DEFAULT_LIBRARY,
                ],
                formats: vec![TokenFormat::RELATIVE],
                overlapping_token_support: Some(true),
                multiline_token_support: Some(true),
                server_cancel_support: Some(true),
                augments_syntax_tokens: Some(true),
            }),
            ..Default::default()
        }),
        general: Some(GeneralClientCapabilities {
            ..Default::default()
        }),
        // Enable rust-analyzer experimental features
        experimental: Some(serde_json::json!({
            "serverStatusNotification": true
        })),
        ..Default::default()
    }
}

fn extract_semantic_token_capability(
    capabilities: &ServerCapabilities,
) -> (Option<SemanticTokensLegend>, bool, bool, bool) {
    capabilities
        .semantic_tokens_provider
        .as_ref()
        .map(|provider| match provider {
            SemanticTokensServerCapabilities::SemanticTokensOptions(options) => (
                Some(options.legend.clone()),
                semantic_tokens_full_supported(&options.full),
                semantic_tokens_full_delta_supported(&options.full),
                options.range.unwrap_or(false),
            ),
            SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(options) => {
                let legend = options.semantic_tokens_options.legend.clone();
                let full = semantic_tokens_full_supported(&options.semantic_tokens_options.full);
                let delta =
                    semantic_tokens_full_delta_supported(&options.semantic_tokens_options.full);
                let range = options.semantic_tokens_options.range.unwrap_or(false);
                (Some(legend), full, delta, range)
            }
        })
        .unwrap_or((None, false, false, false))
}

fn semantic_tokens_full_supported(full: &Option<SemanticTokensFullOptions>) -> bool {
    match full {
        Some(SemanticTokensFullOptions::Bool(v)) => *v,
        Some(SemanticTokensFullOptions::Delta { .. }) => true,
        None => false,
    }
}

fn semantic_tokens_full_delta_supported(full: &Option<SemanticTokensFullOptions>) -> bool {
    match full {
        Some(SemanticTokensFullOptions::Delta { delta }) => delta.unwrap_or(false),
        _ => false,
    }
}

/// Commands sent from the main loop to the LSP task
#[derive(Debug)]
enum LspCommand {
    /// Initialize the server
    Initialize {
        root_uri: Option<Uri>,
        initialization_options: Option<Value>,
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

    /// Request find references
    References {
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
    },

    /// Request signature help
    SignatureHelp {
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
    },

    /// Request code actions
    CodeActions {
        request_id: u64,
        uri: Uri,
        start_line: u32,
        start_char: u32,
        end_line: u32,
        end_char: u32,
        diagnostics: Vec<lsp_types::Diagnostic>,
    },

    /// Request document diagnostics (pull model)
    DocumentDiagnostic {
        request_id: u64,
        uri: Uri,
        /// Previous result_id for incremental updates (None for full refresh)
        previous_result_id: Option<String>,
    },

    /// Request inlay hints for a range (LSP 3.17+)
    InlayHints {
        request_id: u64,
        uri: Uri,
        /// Range to get hints for (typically viewport)
        start_line: u32,
        start_char: u32,
        end_line: u32,
        end_char: u32,
    },

    /// Request semantic tokens for the entire document
    SemanticTokensFull { request_id: u64, uri: Uri },

    /// Request semantic tokens delta for the entire document
    SemanticTokensFullDelta {
        request_id: u64,
        uri: Uri,
        previous_result_id: String,
    },

    /// Request semantic tokens for a range
    SemanticTokensRange {
        request_id: u64,
        uri: Uri,
        range: lsp_types::Range,
    },

    /// Cancel a pending request
    CancelRequest {
        /// Editor's request ID to cancel
        request_id: u64,
    },

    /// Custom request initiated by a plugin
    PluginRequest {
        request_id: u64,
        method: String,
        params: Option<Value>,
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

    /// Track when didOpen was sent for each document to avoid race with didChange
    /// The LSP server needs time to process didOpen before it can handle didChange
    pending_opens: HashMap<PathBuf, Instant>,

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
    /// Replay pending commands that were queued before initialization
    async fn replay_pending_commands(
        &mut self,
        commands: Vec<LspCommand>,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) {
        if commands.is_empty() {
            return;
        }
        tracing::info!(
            "Replaying {} pending commands after initialization",
            commands.len()
        );
        for cmd in commands {
            match cmd {
                LspCommand::DidOpen {
                    uri,
                    text,
                    language_id,
                } => {
                    tracing::info!("Replaying DidOpen for {}", uri.as_str());
                    let _ = self
                        .handle_did_open_sequential(uri, text, language_id, pending)
                        .await;
                }
                LspCommand::DidChange {
                    uri,
                    content_changes,
                } => {
                    tracing::info!("Replaying DidChange for {}", uri.as_str());
                    let _ = self
                        .handle_did_change_sequential(uri, content_changes, pending)
                        .await;
                }
                LspCommand::DidSave { uri, text } => {
                    tracing::info!("Replaying DidSave for {}", uri.as_str());
                    let _ = self.handle_did_save(uri, text).await;
                }
                LspCommand::SemanticTokensFull { request_id, uri } => {
                    tracing::info!("Replaying semantic tokens request for {}", uri.as_str());
                    let _ = self
                        .handle_semantic_tokens_full(request_id, uri, pending)
                        .await;
                }
                LspCommand::SemanticTokensFullDelta {
                    request_id,
                    uri,
                    previous_result_id,
                } => {
                    tracing::info!(
                        "Replaying semantic tokens delta request for {}",
                        uri.as_str()
                    );
                    let _ = self
                        .handle_semantic_tokens_full_delta(
                            request_id,
                            uri,
                            previous_result_id,
                            pending,
                        )
                        .await;
                }
                LspCommand::SemanticTokensRange {
                    request_id,
                    uri,
                    range,
                } => {
                    tracing::info!(
                        "Replaying semantic tokens range request for {}",
                        uri.as_str()
                    );
                    let _ = self
                        .handle_semantic_tokens_range(request_id, uri, range, pending)
                        .await;
                }
                _ => {}
            }
        }
    }

    /// Write a message to stdin
    async fn write_message<T: Serialize>(&mut self, message: &T) -> Result<(), String> {
        let json =
            serde_json::to_string(message).map_err(|e| format!("Serialization error: {}", e))?;

        let content = format!("Content-Length: {}\r\n\r\n{}", json.len(), json);

        tracing::trace!("Writing LSP message to stdin ({} bytes)", content.len());

        self.stdin
            .write_all(content.as_bytes())
            .await
            .map_err(|e| format!("Failed to write to stdin: {}", e))?;

        self.stdin
            .flush()
            .await
            .map_err(|e| format!("Failed to flush stdin: {}", e))?;

        tracing::trace!("Successfully sent LSP message");

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
            params: Some(
                serde_json::to_value(params)
                    .map_err(|e| format!("Failed to serialize params: {}", e))?,
            ),
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
            tracing::trace!("Tracking request: editor_id={}, lsp_id={}", editor_id, id);
        }

        let params_value = params
            .map(|p| serde_json::to_value(p))
            .transpose()
            .map_err(|e| format!("Failed to serialize params: {}", e))?;
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id,
            method: method.to_string(),
            params: params_value,
        };

        let (tx, rx) = oneshot::channel();
        pending.lock().unwrap().insert(id, tx);

        self.write_message(&request).await?;

        tracing::trace!("Sent LSP request id={}, waiting for response...", id);

        // Await response (this is OK now because the reader task will send it)
        let result = rx
            .await
            .map_err(|_| "Response channel closed".to_string())??;

        tracing::trace!("Received LSP response for request id={}", id);

        // Remove tracking after response received
        if let Some(editor_id) = editor_request_id {
            self.active_requests.remove(&editor_id);
            tracing::trace!("Completed request: editor_id={}, lsp_id={}", editor_id, id);
        }

        serde_json::from_value(result).map_err(|e| format!("Failed to deserialize response: {}", e))
    }

    /// Handle initialize command
    async fn handle_initialize_sequential(
        &mut self,
        root_uri: Option<Uri>,
        initialization_options: Option<Value>,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<InitializeResult, String> {
        tracing::info!(
            "Initializing async LSP server with root_uri: {:?}, initialization_options: {:?}",
            root_uri,
            initialization_options
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
            initialization_options,
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

        // Extract completion trigger characters from server capabilities
        let completion_trigger_characters = result
            .capabilities
            .completion_provider
            .as_ref()
            .and_then(|cp| cp.trigger_characters.clone())
            .unwrap_or_default();

        let (
            semantic_tokens_legend,
            semantic_tokens_full,
            semantic_tokens_full_delta,
            semantic_tokens_range,
        ) = extract_semantic_token_capability(&result.capabilities);

        // Notify main loop
        let _ = self.async_tx.send(AsyncMessage::LspInitialized {
            language: self.language.clone(),
            completion_trigger_characters,
            semantic_tokens_legend,
            semantic_tokens_full,
            semantic_tokens_full_delta,
            semantic_tokens_range,
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
        let path = PathBuf::from(uri.path().as_str());

        if should_skip_did_open(&self.document_versions, &path, &self.language, &uri) {
            return Ok(());
        }

        tracing::trace!("LSP: did_open for {}", uri.as_str());

        let params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id,
                version: 0,
                text,
            },
        };

        self.document_versions.insert(path.clone(), 0);

        // Record when we sent didOpen so didChange can wait if needed
        self.pending_opens.insert(path, Instant::now());

        self.send_notification::<DidOpenTextDocument>(params).await
    }

    /// Handle did_change command
    async fn handle_did_change_sequential(
        &mut self,
        uri: Uri,
        content_changes: Vec<TextDocumentContentChangeEvent>,
        _pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        tracing::trace!("LSP: did_change for {}", uri.as_str());

        let path = PathBuf::from(uri.path().as_str());

        // If the document hasn't been opened yet (not in document_versions),
        // skip this change - the upcoming didOpen will have the current content
        if !self.document_versions.contains_key(&path) {
            tracing::debug!(
                "LSP ({}): skipping didChange - document not yet opened",
                self.language
            );
            return Ok(());
        }

        // Check if this document was recently opened and wait if needed
        // This prevents race conditions where the server receives didChange
        // before it has finished processing didOpen
        if let Some(opened_at) = self.pending_opens.get(&path) {
            let elapsed = opened_at.elapsed();
            let grace_period = std::time::Duration::from_millis(DID_OPEN_GRACE_PERIOD_MS);
            if elapsed < grace_period {
                let wait_time = grace_period - elapsed;
                tracing::debug!(
                    "LSP ({}): waiting {:?} for didOpen grace period before didChange",
                    self.language,
                    wait_time
                );
                tokio::time::sleep(wait_time).await;
            }
            // Remove from pending_opens after grace period has passed
            self.pending_opens.remove(&path);
        }

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
        tracing::trace!("LSP: did_save for {}", uri.as_str());

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

        tracing::trace!(
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

        tracing::trace!(
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

        tracing::trace!(
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

        tracing::trace!(
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
                tracing::debug!("Raw LSP hover response: {:?}", result);
                // Parse the hover response
                let (contents, is_markdown, range) = if result.is_null() {
                    // No hover information available
                    (String::new(), false, None)
                } else {
                    match serde_json::from_value::<lsp_types::Hover>(result) {
                        Ok(hover) => {
                            // Extract text from hover contents
                            let (contents, is_markdown) =
                                Self::extract_hover_contents(&hover.contents);
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
            }
            HoverContents::Markup(MarkupContent { kind, value }) => {
                // Check if it's markdown or plaintext
                let is_markdown = matches!(kind, MarkupKind::Markdown);
                (value.clone(), is_markdown)
            }
        }
    }

    /// Handle find references request
    async fn handle_references(
        &mut self,
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        use lsp_types::{
            PartialResultParams, Position, ReferenceContext, ReferenceParams,
            TextDocumentIdentifier, WorkDoneProgressParams,
        };

        tracing::trace!(
            "LSP: find references request at {}:{}:{}",
            uri.as_str(),
            line,
            character
        );

        let params = ReferenceParams {
            text_document_position: lsp_types::TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position { line, character },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            context: ReferenceContext {
                include_declaration: true,
            },
        };

        // Send request and get response
        match self
            .send_request_sequential::<_, Value>("textDocument/references", Some(params), pending)
            .await
        {
            Ok(result) => {
                // Parse the references response (Vec<Location> or null)
                let locations = if result.is_null() {
                    Vec::new()
                } else {
                    serde_json::from_value::<Vec<lsp_types::Location>>(result).unwrap_or_default()
                };

                tracing::trace!("LSP: found {} references", locations.len());

                // Send to main loop
                let _ = self.async_tx.send(AsyncMessage::LspReferences {
                    request_id,
                    locations,
                });
                Ok(())
            }
            Err(e) => {
                tracing::error!("Find references request failed: {}", e);
                // Send empty result on error
                let _ = self.async_tx.send(AsyncMessage::LspReferences {
                    request_id,
                    locations: Vec::new(),
                });
                Err(e)
            }
        }
    }

    /// Handle signature help request
    async fn handle_signature_help(
        &mut self,
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        use lsp_types::{
            Position, SignatureHelpParams, TextDocumentIdentifier, TextDocumentPositionParams,
            WorkDoneProgressParams,
        };

        tracing::trace!(
            "LSP: signature help request at {}:{}:{}",
            uri.as_str(),
            line,
            character
        );

        let params = SignatureHelpParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position { line, character },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            context: None, // We can add context later for re-triggers
        };

        // Send request and get response
        match self
            .send_request_sequential::<_, Value>(
                "textDocument/signatureHelp",
                Some(params),
                pending,
            )
            .await
        {
            Ok(result) => {
                // Parse the signature help response (SignatureHelp or null)
                let signature_help = if result.is_null() {
                    None
                } else {
                    serde_json::from_value::<lsp_types::SignatureHelp>(result).ok()
                };

                tracing::trace!(
                    "LSP: signature help received: {} signatures",
                    signature_help
                        .as_ref()
                        .map(|h| h.signatures.len())
                        .unwrap_or(0)
                );

                // Send to main loop
                let _ = self.async_tx.send(AsyncMessage::LspSignatureHelp {
                    request_id,
                    signature_help,
                });
                Ok(())
            }
            Err(e) => {
                tracing::error!("Signature help request failed: {}", e);
                // Send empty result on error
                let _ = self.async_tx.send(AsyncMessage::LspSignatureHelp {
                    request_id,
                    signature_help: None,
                });
                Err(e)
            }
        }
    }

    /// Handle code actions request
    async fn handle_code_actions(
        &mut self,
        request_id: u64,
        uri: Uri,
        start_line: u32,
        start_char: u32,
        end_line: u32,
        end_char: u32,
        diagnostics: Vec<lsp_types::Diagnostic>,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        use lsp_types::{
            CodeActionContext, CodeActionParams, PartialResultParams, Position, Range,
            TextDocumentIdentifier, WorkDoneProgressParams,
        };

        tracing::trace!(
            "LSP: code actions request at {}:{}:{}-{}:{}",
            uri.as_str(),
            start_line,
            start_char,
            end_line,
            end_char
        );

        let params = CodeActionParams {
            text_document: TextDocumentIdentifier { uri },
            range: Range {
                start: Position {
                    line: start_line,
                    character: start_char,
                },
                end: Position {
                    line: end_line,
                    character: end_char,
                },
            },
            context: CodeActionContext {
                diagnostics,
                only: None,
                trigger_kind: None,
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };

        // Send request and get response
        match self
            .send_request_sequential::<_, Value>("textDocument/codeAction", Some(params), pending)
            .await
        {
            Ok(result) => {
                // Parse the code actions response (Vec<CodeActionOrCommand> or null)
                let actions = if result.is_null() {
                    Vec::new()
                } else {
                    serde_json::from_value::<Vec<lsp_types::CodeActionOrCommand>>(result)
                        .unwrap_or_default()
                };

                tracing::trace!("LSP: received {} code actions", actions.len());

                // Send to main loop
                let _ = self.async_tx.send(AsyncMessage::LspCodeActions {
                    request_id,
                    actions,
                });
                Ok(())
            }
            Err(e) => {
                tracing::error!("Code actions request failed: {}", e);
                // Send empty result on error
                let _ = self.async_tx.send(AsyncMessage::LspCodeActions {
                    request_id,
                    actions: Vec::new(),
                });
                Err(e)
            }
        }
    }

    /// Handle document diagnostic request (pull diagnostics)
    async fn handle_document_diagnostic(
        &mut self,
        request_id: u64,
        uri: Uri,
        previous_result_id: Option<String>,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        use lsp_types::{
            DocumentDiagnosticParams, PartialResultParams, TextDocumentIdentifier,
            WorkDoneProgressParams,
        };

        // Check if server supports pull diagnostics (diagnosticProvider capability)
        if self
            .capabilities
            .as_ref()
            .and_then(|c| c.diagnostic_provider.as_ref())
            .is_none()
        {
            tracing::trace!(
                "LSP: server does not support pull diagnostics, skipping request for {}",
                uri.as_str()
            );
            return Ok(());
        }

        tracing::trace!(
            "LSP: document diagnostic request for {} (previous_result_id: {:?})",
            uri.as_str(),
            previous_result_id
        );

        let params = DocumentDiagnosticParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            identifier: None,
            previous_result_id,
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        };

        // Send request and get response
        match self
            .send_request_sequential::<_, Value>("textDocument/diagnostic", Some(params), pending)
            .await
        {
            Ok(result) => {
                // Parse the diagnostic report result
                // Can be RelatedFullDocumentDiagnosticReport or RelatedUnchangedDocumentDiagnosticReport
                let uri_string = uri.as_str().to_string();

                // Try to parse as full report first
                if let Ok(full_report) = serde_json::from_value::<
                    lsp_types::RelatedFullDocumentDiagnosticReport,
                >(result.clone())
                {
                    let diagnostics = full_report.full_document_diagnostic_report.items;
                    let result_id = full_report.full_document_diagnostic_report.result_id;

                    tracing::trace!(
                        "LSP: received {} diagnostics for {} (result_id: {:?})",
                        diagnostics.len(),
                        uri_string,
                        result_id
                    );

                    let _ = self.async_tx.send(AsyncMessage::LspPulledDiagnostics {
                        request_id,
                        uri: uri_string,
                        result_id,
                        diagnostics,
                        unchanged: false,
                    });
                } else if let Ok(unchanged_report) = serde_json::from_value::<
                    lsp_types::RelatedUnchangedDocumentDiagnosticReport,
                >(result.clone())
                {
                    let result_id = unchanged_report
                        .unchanged_document_diagnostic_report
                        .result_id;

                    tracing::trace!(
                        "LSP: diagnostics unchanged for {} (result_id: {:?})",
                        uri_string,
                        result_id
                    );

                    let _ = self.async_tx.send(AsyncMessage::LspPulledDiagnostics {
                        request_id,
                        uri: uri_string,
                        result_id: Some(result_id),
                        diagnostics: Vec::new(),
                        unchanged: true,
                    });
                } else {
                    // Fallback: try to parse as DocumentDiagnosticReportResult
                    tracing::warn!(
                        "LSP: could not parse diagnostic report, sending empty: {}",
                        result
                    );
                    let _ = self.async_tx.send(AsyncMessage::LspPulledDiagnostics {
                        request_id,
                        uri: uri_string,
                        result_id: None,
                        diagnostics: Vec::new(),
                        unchanged: false,
                    });
                }

                Ok(())
            }
            Err(e) => {
                tracing::error!("Document diagnostic request failed: {}", e);
                // Send empty result on error
                let _ = self.async_tx.send(AsyncMessage::LspPulledDiagnostics {
                    request_id,
                    uri: uri.as_str().to_string(),
                    result_id: None,
                    diagnostics: Vec::new(),
                    unchanged: false,
                });
                Err(e)
            }
        }
    }

    /// Handle inlay hints request (LSP 3.17+)
    async fn handle_inlay_hints(
        &mut self,
        request_id: u64,
        uri: Uri,
        start_line: u32,
        start_char: u32,
        end_line: u32,
        end_char: u32,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        use lsp_types::{
            InlayHintParams, Position, Range, TextDocumentIdentifier, WorkDoneProgressParams,
        };

        tracing::trace!(
            "LSP: inlay hints request for {} ({}:{} - {}:{})",
            uri.as_str(),
            start_line,
            start_char,
            end_line,
            end_char
        );

        let params = InlayHintParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            range: Range {
                start: Position {
                    line: start_line,
                    character: start_char,
                },
                end: Position {
                    line: end_line,
                    character: end_char,
                },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
        };

        match self
            .send_request_sequential::<_, Option<Vec<lsp_types::InlayHint>>>(
                "textDocument/inlayHint",
                Some(params),
                pending,
            )
            .await
        {
            Ok(hints) => {
                let hints = hints.unwrap_or_default();
                let uri_string = uri.as_str().to_string();

                tracing::trace!(
                    "LSP: received {} inlay hints for {}",
                    hints.len(),
                    uri_string
                );

                let _ = self.async_tx.send(AsyncMessage::LspInlayHints {
                    request_id,
                    uri: uri_string,
                    hints,
                });

                Ok(())
            }
            Err(e) => {
                tracing::error!("Inlay hints request failed: {}", e);
                // Send empty result on error
                let _ = self.async_tx.send(AsyncMessage::LspInlayHints {
                    request_id,
                    uri: uri.as_str().to_string(),
                    hints: Vec::new(),
                });
                Err(e)
            }
        }
    }

    async fn handle_semantic_tokens_full(
        &mut self,
        request_id: u64,
        uri: Uri,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        use lsp_types::{
            request::SemanticTokensFullRequest, PartialResultParams, TextDocumentIdentifier,
            WorkDoneProgressParams,
        };

        tracing::trace!("LSP: semanticTokens/full request for {}", uri.as_str());

        let params = SemanticTokensParams {
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            text_document: TextDocumentIdentifier { uri: uri.clone() },
        };

        match self
            .send_request_sequential_tracked::<_, Option<SemanticTokensResult>>(
                SemanticTokensFullRequest::METHOD,
                Some(params),
                pending,
                Some(request_id),
            )
            .await
        {
            Ok(result) => {
                let _ = self.async_tx.send(AsyncMessage::LspSemanticTokens {
                    request_id,
                    uri: uri.as_str().to_string(),
                    response: LspSemanticTokensResponse::Full(Ok(result)),
                });
                Ok(())
            }
            Err(e) => {
                tracing::error!("Semantic tokens request failed: {}", e);
                let _ = self.async_tx.send(AsyncMessage::LspSemanticTokens {
                    request_id,
                    uri: uri.as_str().to_string(),
                    response: LspSemanticTokensResponse::Full(Err(e.clone())),
                });
                Err(e)
            }
        }
    }

    async fn handle_semantic_tokens_full_delta(
        &mut self,
        request_id: u64,
        uri: Uri,
        previous_result_id: String,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        use lsp_types::{
            request::SemanticTokensFullDeltaRequest, PartialResultParams,
            SemanticTokensDeltaParams, SemanticTokensFullDeltaResult, TextDocumentIdentifier,
            WorkDoneProgressParams,
        };

        tracing::trace!(
            "LSP: semanticTokens/full/delta request for {}",
            uri.as_str()
        );

        let params = SemanticTokensDeltaParams {
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            previous_result_id,
        };

        match self
            .send_request_sequential_tracked::<_, Option<SemanticTokensFullDeltaResult>>(
                SemanticTokensFullDeltaRequest::METHOD,
                Some(params),
                pending,
                Some(request_id),
            )
            .await
        {
            Ok(result) => {
                let _ = self.async_tx.send(AsyncMessage::LspSemanticTokens {
                    request_id,
                    uri: uri.as_str().to_string(),
                    response: LspSemanticTokensResponse::FullDelta(Ok(result)),
                });
                Ok(())
            }
            Err(e) => {
                tracing::error!("Semantic tokens delta request failed: {}", e);
                let _ = self.async_tx.send(AsyncMessage::LspSemanticTokens {
                    request_id,
                    uri: uri.as_str().to_string(),
                    response: LspSemanticTokensResponse::FullDelta(Err(e.clone())),
                });
                Err(e)
            }
        }
    }

    async fn handle_semantic_tokens_range(
        &mut self,
        request_id: u64,
        uri: Uri,
        range: lsp_types::Range,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        use lsp_types::{
            request::SemanticTokensRangeRequest, PartialResultParams, SemanticTokensRangeParams,
            TextDocumentIdentifier, WorkDoneProgressParams,
        };

        tracing::trace!("LSP: semanticTokens/range request for {}", uri.as_str());

        let params = SemanticTokensRangeParams {
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            range,
        };

        match self
            .send_request_sequential_tracked::<_, Option<lsp_types::SemanticTokensRangeResult>>(
                SemanticTokensRangeRequest::METHOD,
                Some(params),
                pending,
                Some(request_id),
            )
            .await
        {
            Ok(result) => {
                let _ = self.async_tx.send(AsyncMessage::LspSemanticTokens {
                    request_id,
                    uri: uri.as_str().to_string(),
                    response: LspSemanticTokensResponse::Range(Ok(result)),
                });
                Ok(())
            }
            Err(e) => {
                tracing::error!("Semantic tokens range request failed: {}", e);
                let _ = self.async_tx.send(AsyncMessage::LspSemanticTokens {
                    request_id,
                    uri: uri.as_str().to_string(),
                    response: LspSemanticTokensResponse::Range(Err(e.clone())),
                });
                Err(e)
            }
        }
    }

    /// Handle a plugin-initiated request by forwarding it to the server
    async fn handle_plugin_request(
        &mut self,
        request_id: u64,
        method: String,
        params: Option<Value>,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) {
        tracing::trace!(
            "Plugin request {} => method={} params={:?}",
            request_id,
            method,
            params
        );
        let result = self
            .send_request_sequential_tracked::<Value, Value>(
                &method,
                params,
                pending,
                Some(request_id),
            )
            .await;

        tracing::trace!(
            "Plugin request {} completed with result {:?}",
            request_id,
            &result
        );
        let _ = self.async_tx.send(AsyncMessage::PluginLspResponse {
            language: self.language.clone(),
            request_id,
            result,
        });
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
        tracing::trace!("Sending $/cancelRequest for LSP id {}", lsp_id);

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
            tracing::trace!(
                "Cancel request ignored: no active LSP request for editor_id={}",
                request_id
            );
            Ok(())
        }
    }
}

/// Async LSP task that handles all I/O
struct LspTask {
    /// Process handle - kept alive for lifetime management (kill_on_drop)
    _process: Child,

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

    /// Track when didOpen was sent for each document to avoid race with didChange
    /// The LSP server needs time to process didOpen before it can handle didChange
    pending_opens: HashMap<PathBuf, Instant>,

    /// Whether initialized
    initialized: bool,

    /// Sender for async messages to main loop
    async_tx: std_mpsc::Sender<AsyncMessage>,

    /// Language ID (for error reporting)
    language: String,

    /// Server command (for plugin identification)
    server_command: String,

    /// Path to stderr log file
    stderr_log_path: std::path::PathBuf,
}

impl LspTask {
    /// Create a new LSP task
    async fn spawn(
        command: &str,
        args: &[String],
        language: String,
        async_tx: std_mpsc::Sender<AsyncMessage>,
        process_limits: &ProcessLimits,
        stderr_log_path: std::path::PathBuf,
    ) -> Result<Self, String> {
        tracing::info!("Spawning async LSP server: {} {:?}", command, args);
        tracing::info!("Process limits: {:?}", process_limits);
        tracing::info!("LSP stderr will be logged to: {:?}", stderr_log_path);

        // Check if the command exists before trying to spawn
        // This provides a clearer error message than the generic "No such file or directory"
        if !Self::command_exists(command) {
            return Err(format!(
                "LSP server executable '{}' not found. Please install it or check your PATH.",
                command
            ));
        }

        // Create stderr log file and redirect process stderr directly to it
        let stderr_file = std::fs::File::create(&stderr_log_path).map_err(|e| {
            format!(
                "Failed to create LSP stderr log file {:?}: {}",
                stderr_log_path, e
            )
        })?;

        let mut cmd = Command::new(command);
        cmd.args(args)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::from(stderr_file))
            .kill_on_drop(true);

        // Apply resource limits to the process
        process_limits
            .apply_to_command(&mut cmd)
            .map_err(|e| format!("Failed to apply process limits: {}", e))?;

        let mut process = cmd.spawn().map_err(|e| {
            format!(
                "Failed to spawn LSP server '{}': {}",
                command,
                match e.kind() {
                    std::io::ErrorKind::NotFound => "executable not found in PATH".to_string(),
                    std::io::ErrorKind::PermissionDenied =>
                        "permission denied (check file permissions)".to_string(),
                    _ => e.to_string(),
                }
            )
        })?;

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

        Ok(Self {
            _process: process,
            stdin,
            stdout,
            next_id: 0,
            pending: HashMap::new(),
            capabilities: None,
            document_versions: HashMap::new(),
            pending_opens: HashMap::new(),
            initialized: false,
            async_tx,
            language,
            server_command: command.to_string(),
            stderr_log_path,
        })
    }

    /// Check if a command exists in PATH or as an absolute path
    fn command_exists(command: &str) -> bool {
        use std::path::Path;

        // If it's an absolute path, check if the file exists and is executable
        if command.contains('/') || command.contains('\\') {
            let path = Path::new(command);
            return path.exists() && path.is_file();
        }

        // Otherwise, search in PATH
        if let Ok(path_var) = std::env::var("PATH") {
            #[cfg(unix)]
            let separator = ':';
            #[cfg(windows)]
            let separator = ';';

            for dir in path_var.split(separator) {
                let full_path = Path::new(dir).join(command);
                if full_path.exists() && full_path.is_file() {
                    return true;
                }
                // On Windows, also check with .exe extension
                #[cfg(windows)]
                {
                    let with_exe = Path::new(dir).join(format!("{}.exe", command));
                    if with_exe.exists() && with_exe.is_file() {
                        return true;
                    }
                }
            }
        }

        false
    }

    /// Spawn the stdout reader task that continuously reads and dispatches LSP messages
    fn spawn_stdout_reader(
        mut stdout: BufReader<ChildStdout>,
        pending: Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
        async_tx: std_mpsc::Sender<AsyncMessage>,
        language: String,
        server_command: String,
        server_response_tx: mpsc::Sender<JsonRpcResponse>,
        stderr_log_path: std::path::PathBuf,
        shutting_down: Arc<AtomicBool>,
    ) {
        tokio::spawn(async move {
            tracing::info!("LSP stdout reader task started for {}", language);
            loop {
                match read_message_from_stdout(&mut stdout).await {
                    Ok(message) => {
                        tracing::trace!("Read message from LSP server: {:?}", message);
                        if let Err(e) = handle_message_dispatch(
                            message,
                            &pending,
                            &async_tx,
                            &language,
                            &server_command,
                            &server_response_tx,
                        )
                        .await
                        {
                            tracing::error!("Error handling LSP message: {}", e);
                        }
                    }
                    Err(e) => {
                        // Only report error if this wasn't an intentional shutdown
                        if shutting_down.load(Ordering::SeqCst) {
                            tracing::info!(
                                "LSP stdout reader exiting due to graceful shutdown for {}",
                                language
                            );
                        } else {
                            tracing::error!("Error reading from LSP server: {}", e);
                            let _ = async_tx.send(AsyncMessage::LspStatusUpdate {
                                language: language.clone(),
                                status: LspServerStatus::Error,
                            });
                            let _ = async_tx.send(AsyncMessage::LspError {
                                language: language.clone(),
                                error: format!("Read error: {}", e),
                                stderr_log_path: Some(stderr_log_path.clone()),
                            });
                        }
                        break;
                    }
                }
            }
            tracing::info!("LSP stdout reader task exiting for {}", language);
        });
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
            pending_opens: self.pending_opens,
            initialized: self.initialized,
            async_tx: self.async_tx.clone(),
            language: self.language.clone(),
            active_requests: HashMap::new(),
        };

        let pending = Arc::new(Mutex::new(self.pending));
        let async_tx = state.async_tx.clone();
        let language_clone = state.language.clone();

        // Create channel for server-to-client request responses
        let (server_response_tx, mut server_response_rx) = mpsc::channel::<JsonRpcResponse>(100);

        // Flag to indicate intentional shutdown (prevents spurious error messages)
        let shutting_down = Arc::new(AtomicBool::new(false));

        // Spawn stdout reader task
        Self::spawn_stdout_reader(
            self.stdout,
            pending.clone(),
            async_tx.clone(),
            language_clone.clone(),
            self.server_command.clone(),
            server_response_tx,
            self.stderr_log_path,
            shutting_down.clone(),
        );

        // Sequential command processing loop with server response handling
        let mut pending_commands = Vec::new();
        loop {
            tokio::select! {
                // Handle server-to-client responses (high priority)
                Some(response) = server_response_rx.recv() => {
                    tracing::trace!("Sending response to server request id={}", response.id);
                    if let Err(e) = state.write_message(&response).await {
                        tracing::error!("Failed to send response to server: {}", e);
                    }
                }
                // Handle commands from the editor
                Some(cmd) = command_rx.recv() => {
                    tracing::trace!("LspTask received command: {:?}", cmd);
                    match cmd {
                        LspCommand::Initialize { root_uri, initialization_options, response } => {
                            // Send initializing status
                            let _ = async_tx.send(AsyncMessage::LspStatusUpdate {
                                language: language_clone.clone(),
                                status: LspServerStatus::Initializing,
                            });
                            tracing::info!("Processing Initialize command");
                            let result =
                                state.handle_initialize_sequential(root_uri, initialization_options, &pending).await;
                            let success = result.is_ok();
                            let _ = response.send(result);

                            // After successful initialization, replay pending commands
                            if success {
                                let queued = std::mem::take(&mut pending_commands);
                                state.replay_pending_commands(queued, &pending).await;
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
                                tracing::trace!(
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
                                tracing::trace!("Processing DidChange for {}", uri.as_str());
                                let _ = state
                                    .handle_did_change_sequential(uri, content_changes, &pending)
                                    .await;
                            } else {
                                tracing::trace!(
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
                                tracing::trace!(
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
                                tracing::trace!("LSP not initialized, sending empty completion");
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
                                tracing::trace!("LSP not initialized, sending empty locations");
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
                                tracing::trace!("LSP not initialized, cannot rename");
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
                                tracing::trace!("LSP not initialized, cannot get hover");
                                let _ = state.async_tx.send(AsyncMessage::LspHover {
                                    request_id,
                                    contents: String::new(),
                                    is_markdown: false,
                                    range: None,
                                });
                            }
                        }
                        LspCommand::References {
                            request_id,
                            uri,
                            line,
                            character,
                        } => {
                            if state.initialized {
                                tracing::info!("Processing References request for {}", uri.as_str());
                                let _ = state
                                    .handle_references(request_id, uri, line, character, &pending)
                                    .await;
                            } else {
                                tracing::trace!("LSP not initialized, cannot get references");
                                let _ = state.async_tx.send(AsyncMessage::LspReferences {
                                    request_id,
                                    locations: Vec::new(),
                                });
                            }
                        }
                        LspCommand::SignatureHelp {
                            request_id,
                            uri,
                            line,
                            character,
                        } => {
                            if state.initialized {
                                tracing::info!("Processing SignatureHelp request for {}", uri.as_str());
                                let _ = state
                                    .handle_signature_help(request_id, uri, line, character, &pending)
                                    .await;
                            } else {
                                tracing::trace!("LSP not initialized, cannot get signature help");
                                let _ = state.async_tx.send(AsyncMessage::LspSignatureHelp {
                                    request_id,
                                    signature_help: None,
                                });
                            }
                        }
                        LspCommand::CodeActions {
                            request_id,
                            uri,
                            start_line,
                            start_char,
                            end_line,
                            end_char,
                            diagnostics,
                        } => {
                            if state.initialized {
                                tracing::info!("Processing CodeActions request for {}", uri.as_str());
                                let _ = state
                                    .handle_code_actions(
                                        request_id,
                                        uri,
                                        start_line,
                                        start_char,
                                        end_line,
                                        end_char,
                                        diagnostics,
                                        &pending,
                                    )
                                    .await;
                            } else {
                                tracing::trace!("LSP not initialized, cannot get code actions");
                                let _ = state.async_tx.send(AsyncMessage::LspCodeActions {
                                    request_id,
                                    actions: Vec::new(),
                                });
                            }
                        }
                        LspCommand::DocumentDiagnostic {
                            request_id,
                            uri,
                            previous_result_id,
                        } => {
                            if state.initialized {
                                tracing::info!(
                                    "Processing DocumentDiagnostic request for {}",
                                    uri.as_str()
                                );
                                let _ = state
                                    .handle_document_diagnostic(
                                        request_id,
                                        uri,
                                        previous_result_id,
                                        &pending,
                                    )
                                    .await;
                            } else {
                                tracing::trace!(
                                    "LSP not initialized, cannot get document diagnostics"
                                );
                                let _ = state.async_tx.send(AsyncMessage::LspPulledDiagnostics {
                                    request_id,
                                    uri: uri.as_str().to_string(),
                                    result_id: None,
                                    diagnostics: Vec::new(),
                                    unchanged: false,
                                });
                            }
                        }
                        LspCommand::InlayHints {
                            request_id,
                            uri,
                            start_line,
                            start_char,
                            end_line,
                            end_char,
                        } => {
                            if state.initialized {
                                tracing::info!(
                                    "Processing InlayHints request for {}",
                                    uri.as_str()
                                );
                                let _ = state
                                    .handle_inlay_hints(
                                        request_id,
                                        uri,
                                        start_line,
                                        start_char,
                                        end_line,
                                        end_char,
                                        &pending,
                                    )
                                    .await;
                            } else {
                                tracing::trace!(
                                    "LSP not initialized, cannot get inlay hints"
                                );
                                let _ = state.async_tx.send(AsyncMessage::LspInlayHints {
                                    request_id,
                                    uri: uri.as_str().to_string(),
                                    hints: Vec::new(),
                                });
                            }
                        }
                        LspCommand::SemanticTokensFull { request_id, uri } => {
                            if state.initialized {
                                tracing::info!(
                                    "Processing SemanticTokens request for {}",
                                    uri.as_str()
                                );
                                let _ = state
                                    .handle_semantic_tokens_full(request_id, uri, &pending)
                                    .await;
                            } else {
                                tracing::trace!(
                                    "LSP not initialized, cannot get semantic tokens"
                                );
                                let _ = state.async_tx.send(AsyncMessage::LspSemanticTokens {
                                    request_id,
                                    uri: uri.as_str().to_string(),
                                    response: LspSemanticTokensResponse::Full(Err(
                                        "LSP not initialized".to_string(),
                                    )),
                                });
                            }
                        }
                        LspCommand::SemanticTokensFullDelta {
                            request_id,
                            uri,
                            previous_result_id,
                        } => {
                            if state.initialized {
                                tracing::info!(
                                    "Processing SemanticTokens delta request for {}",
                                    uri.as_str()
                                );
                                let _ = state
                                    .handle_semantic_tokens_full_delta(
                                        request_id,
                                        uri,
                                        previous_result_id,
                                        &pending,
                                    )
                                    .await;
                            } else {
                                tracing::trace!(
                                    "LSP not initialized, cannot get semantic tokens"
                                );
                                let _ = state.async_tx.send(AsyncMessage::LspSemanticTokens {
                                    request_id,
                                    uri: uri.as_str().to_string(),
                                    response: LspSemanticTokensResponse::FullDelta(Err(
                                        "LSP not initialized".to_string(),
                                    )),
                                });
                            }
                        }
                        LspCommand::SemanticTokensRange {
                            request_id,
                            uri,
                            range,
                        } => {
                            if state.initialized {
                                tracing::info!(
                                    "Processing SemanticTokens range request for {}",
                                    uri.as_str()
                                );
                                let _ = state
                                    .handle_semantic_tokens_range(request_id, uri, range, &pending)
                                    .await;
                            } else {
                                tracing::trace!(
                                    "LSP not initialized, cannot get semantic tokens"
                                );
                                let _ = state.async_tx.send(AsyncMessage::LspSemanticTokens {
                                    request_id,
                                    uri: uri.as_str().to_string(),
                                    response: LspSemanticTokensResponse::Range(Err(
                                        "LSP not initialized".to_string(),
                                    )),
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
                        LspCommand::PluginRequest {
                            request_id,
                            method,
                            params,
                        } => {
                            if state.initialized {
                                tracing::trace!(
                                    "Processing plugin request {} ({})",
                                    request_id,
                                    method
                                );
                                let _ = state
                                    .handle_plugin_request(
                                        request_id,
                                        method,
                                        params,
                                        &pending,
                                    )
                                    .await;
                            } else {
                                tracing::trace!(
                                    "Plugin LSP request {} received before initialization",
                                    request_id
                                );
                                let _ = state.async_tx.send(AsyncMessage::PluginLspResponse {
                                    language: language_clone.clone(),
                                    request_id,
                                    result: Err("LSP not initialized".to_string()),
                                });
                            }
                        }
                        LspCommand::Shutdown => {
                            tracing::info!("Processing Shutdown command");
                            // Set flag before shutdown to prevent spurious error messages
                            shutting_down.store(true, Ordering::SeqCst);
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
    server_command: &str,
    server_response_tx: &mpsc::Sender<JsonRpcResponse>,
) -> Result<(), String> {
    match message {
        JsonRpcMessage::Response(response) => {
            tracing::trace!("Received LSP response for request id={}", response.id);
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
                    tracing::trace!("LSP response success for request id={}", response.id);
                    // null is a valid result for many LSP methods (e.g., inlay hints with no hints)
                    Ok(response.result.unwrap_or(serde_json::Value::Null))
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
            tracing::trace!("Received LSP notification: {}", notification.method);
            handle_notification_dispatch(notification, async_tx, language).await?;
        }
        JsonRpcMessage::Request(request) => {
            // Handle server-to-client requests - MUST respond to avoid timeouts
            tracing::trace!("Received request from server: {}", request.method);
            let response = match request.method.as_str() {
                "window/workDoneProgress/create" => {
                    // Server wants to create a progress token - acknowledge it
                    tracing::trace!("Acknowledging workDoneProgress/create (id={})", request.id);
                    JsonRpcResponse {
                        jsonrpc: "2.0".to_string(),
                        id: request.id,
                        result: Some(Value::Null),
                        error: None,
                    }
                }
                "workspace/configuration" => {
                    // Return configuration with inlay hints enabled for rust-analyzer
                    // The request contains items asking for configuration sections
                    // We return an array with one config object per requested item
                    tracing::trace!(
                        "Responding to workspace/configuration with inlay hints enabled"
                    );

                    // Parse request params to see how many items are requested
                    let num_items = request
                        .params
                        .as_ref()
                        .and_then(|p| p.get("items"))
                        .and_then(|items| items.as_array())
                        .map(|arr| arr.len())
                        .unwrap_or(1);

                    // rust-analyzer configuration with inlay hints enabled
                    let ra_config = serde_json::json!({
                        "inlayHints": {
                            "typeHints": {
                                "enable": true
                            },
                            "parameterHints": {
                                "enable": true
                            },
                            "chainingHints": {
                                "enable": true
                            },
                            "closureReturnTypeHints": {
                                "enable": "always"
                            }
                        }
                    });

                    // Return one config object for each requested item
                    let configs: Vec<Value> = (0..num_items).map(|_| ra_config.clone()).collect();

                    JsonRpcResponse {
                        jsonrpc: "2.0".to_string(),
                        id: request.id,
                        result: Some(Value::Array(configs)),
                        error: None,
                    }
                }
                "client/registerCapability" => {
                    // Server wants to register a capability dynamically - acknowledge
                    tracing::trace!(
                        "Acknowledging client/registerCapability (id={})",
                        request.id
                    );
                    JsonRpcResponse {
                        jsonrpc: "2.0".to_string(),
                        id: request.id,
                        result: Some(Value::Null),
                        error: None,
                    }
                }
                _ => {
                    // For unknown methods, notify plugins and return null to acknowledge receipt
                    tracing::debug!("Server request for plugins: {}", request.method);
                    let _ = async_tx.send(AsyncMessage::LspServerRequest {
                        language: language.to_string(),
                        server_command: server_command.to_string(),
                        method: request.method.clone(),
                        params: request.params.clone(),
                    });
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

                tracing::trace!(
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
                        LspMessageType::Log => tracing::trace!("LSP ({}): {}", language, message),
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
                        LspMessageType::Log => tracing::trace!("LSP ({}): {}", language, message),
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

                                tracing::trace!(
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
        "experimental/serverStatus" => {
            // rust-analyzer specific: server status notification
            // When quiescent is true, the project is fully loaded
            if let Some(params) = notification.params {
                if let Ok(status) = serde_json::from_value::<serde_json::Map<String, Value>>(params)
                {
                    let quiescent = status
                        .get("quiescent")
                        .and_then(|v| v.as_bool())
                        .unwrap_or(false);

                    tracing::info!("LSP ({}) server status: quiescent={}", language, quiescent);

                    if quiescent {
                        // Project is fully loaded - notify editor to re-request inlay hints
                        let _ = async_tx.send(AsyncMessage::LspServerQuiescent {
                            language: language.to_string(),
                        });
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

/// Counter for generating unique LSP handle IDs
static NEXT_HANDLE_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);

/// Synchronous handle to an async LSP task
pub struct LspHandle {
    /// Unique identifier for this handle instance
    id: u64,

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

        // Create stderr log path in XDG state directory
        let stderr_log_path = crate::services::log_dirs::lsp_log_path(&language);

        // Send starting status
        let _ = async_tx.send(AsyncMessage::LspStatusUpdate {
            language: language.clone(),
            status: LspServerStatus::Starting,
        });

        let state_clone = state.clone();
        let stderr_log_path_clone = stderr_log_path.clone();
        runtime.spawn(async move {
            match LspTask::spawn(
                &command,
                &args,
                language_clone.clone(),
                async_tx.clone(),
                &process_limits,
                stderr_log_path_clone.clone(),
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
                        stderr_log_path: Some(stderr_log_path_clone),
                    });
                }
            }
        });

        let id = NEXT_HANDLE_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        Ok(Self {
            id,
            command_tx,
            state,
            runtime: runtime.clone(),
        })
    }

    /// Get the unique ID for this handle instance
    pub fn id(&self) -> u64 {
        self.id
    }

    /// Initialize the server (non-blocking)
    ///
    /// This sends the initialize request asynchronously. The server will be ready
    /// when `is_initialized()` returns true. Other methods that require initialization
    /// will fail gracefully until then.
    ///
    /// The `initialization_options` are passed to the server during initialization.
    /// Some servers like Deno require specific options (e.g., `{"enable": true}`).
    pub fn initialize(
        &self,
        root_uri: Option<Uri>,
        initialization_options: Option<Value>,
    ) -> Result<(), String> {
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
                initialization_options,
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

    /// Request find references
    pub fn references(
        &self,
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
    ) -> Result<(), String> {
        self.command_tx
            .try_send(LspCommand::References {
                request_id,
                uri,
                line,
                character,
            })
            .map_err(|_| "Failed to send references command".to_string())
    }

    /// Request signature help
    pub fn signature_help(
        &self,
        request_id: u64,
        uri: Uri,
        line: u32,
        character: u32,
    ) -> Result<(), String> {
        self.command_tx
            .try_send(LspCommand::SignatureHelp {
                request_id,
                uri,
                line,
                character,
            })
            .map_err(|_| "Failed to send signature_help command".to_string())
    }

    /// Request code actions
    pub fn code_actions(
        &self,
        request_id: u64,
        uri: Uri,
        start_line: u32,
        start_char: u32,
        end_line: u32,
        end_char: u32,
        diagnostics: Vec<lsp_types::Diagnostic>,
    ) -> Result<(), String> {
        self.command_tx
            .try_send(LspCommand::CodeActions {
                request_id,
                uri,
                start_line,
                start_char,
                end_line,
                end_char,
                diagnostics,
            })
            .map_err(|_| "Failed to send code_actions command".to_string())
    }

    /// Request document diagnostics (pull model)
    ///
    /// This sends a textDocument/diagnostic request to fetch diagnostics on demand.
    /// Use `previous_result_id` for incremental updates (server may return unchanged).
    pub fn document_diagnostic(
        &self,
        request_id: u64,
        uri: Uri,
        previous_result_id: Option<String>,
    ) -> Result<(), String> {
        self.command_tx
            .try_send(LspCommand::DocumentDiagnostic {
                request_id,
                uri,
                previous_result_id,
            })
            .map_err(|_| "Failed to send document_diagnostic command".to_string())
    }

    /// Request inlay hints for a range (LSP 3.17+)
    ///
    /// Inlay hints are virtual text annotations displayed inline (e.g., type hints, parameter names).
    pub fn inlay_hints(
        &self,
        request_id: u64,
        uri: Uri,
        start_line: u32,
        start_char: u32,
        end_line: u32,
        end_char: u32,
    ) -> Result<(), String> {
        self.command_tx
            .try_send(LspCommand::InlayHints {
                request_id,
                uri,
                start_line,
                start_char,
                end_line,
                end_char,
            })
            .map_err(|_| "Failed to send inlay_hints command".to_string())
    }

    /// Request semantic tokens for an entire document
    pub fn semantic_tokens_full(&self, request_id: u64, uri: Uri) -> Result<(), String> {
        self.command_tx
            .try_send(LspCommand::SemanticTokensFull { request_id, uri })
            .map_err(|_| "Failed to send semantic_tokens command".to_string())
    }

    /// Request semantic tokens delta for an entire document
    pub fn semantic_tokens_full_delta(
        &self,
        request_id: u64,
        uri: Uri,
        previous_result_id: String,
    ) -> Result<(), String> {
        self.command_tx
            .try_send(LspCommand::SemanticTokensFullDelta {
                request_id,
                uri,
                previous_result_id,
            })
            .map_err(|_| "Failed to send semantic_tokens delta command".to_string())
    }

    /// Request semantic tokens for a range
    pub fn semantic_tokens_range(
        &self,
        request_id: u64,
        uri: Uri,
        range: lsp_types::Range,
    ) -> Result<(), String> {
        self.command_tx
            .try_send(LspCommand::SemanticTokensRange {
                request_id,
                uri,
                range,
            })
            .map_err(|_| "Failed to send semantic_tokens_range command".to_string())
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

    /// Send a custom LSP request initiated by a plugin
    pub fn send_plugin_request(
        &self,
        request_id: u64,
        method: String,
        params: Option<Value>,
    ) -> Result<(), String> {
        tracing::trace!(
            "LspHandle sending plugin request {}: method={}",
            request_id,
            method
        );
        match self.command_tx.try_send(LspCommand::PluginRequest {
            request_id,
            method,
            params,
        }) {
            Ok(()) => {
                tracing::trace!(
                    "LspHandle enqueued plugin request {} successfully",
                    request_id
                );
                Ok(())
            }
            Err(e) => {
                tracing::error!("Failed to enqueue plugin request {}: {}", request_id, e);
                Err("Failed to send plugin LSP request".to_string())
            }
        }
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
        let result = handle.initialize(None, None);

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
        let init_result = handle.initialize(None, None);
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
