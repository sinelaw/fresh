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

use crate::async_bridge::{AsyncBridge, AsyncMessage};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidOpenTextDocument, DidSaveTextDocument, Initialized,
        Notification, PublishDiagnostics,
    },
    request::{Initialize, Request, Shutdown},
    ClientCapabilities, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, InitializeParams, InitializeResult, InitializedParams,
    PublishDiagnosticsParams, ServerCapabilities, TextDocumentContentChangeEvent,
    TextDocumentIdentifier, TextDocumentItem, Url, VersionedTextDocumentIdentifier,
    WorkspaceFolder,
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

/// Commands sent from the main loop to the LSP task
#[derive(Debug)]
enum LspCommand {
    /// Initialize the server
    Initialize {
        root_uri: Option<Url>,
        response: oneshot::Sender<Result<InitializeResult, String>>,
    },

    /// Notify document opened
    DidOpen {
        uri: Url,
        text: String,
        language_id: String,
    },

    /// Notify document changed
    DidChange {
        uri: Url,
        content_changes: Vec<TextDocumentContentChangeEvent>,
    },

    /// Notify document saved
    DidSave {
        uri: Url,
        text: Option<String>,
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

    /// Handle initialize command
    async fn handle_initialize_sequential(
        &mut self,
        root_uri: Option<Url>,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<InitializeResult, String> {
        tracing::info!("Initializing async LSP server with root_uri: {:?}", root_uri);

        let workspace_folders = root_uri.as_ref().map(|uri| {
            vec![WorkspaceFolder {
                uri: uri.clone(),
                name: uri
                    .path()
                    .split('/')
                    .last()
                    .unwrap_or("workspace")
                    .to_string(),
            }]
        });

        let params = InitializeParams {
            process_id: Some(std::process::id()),
            root_uri: root_uri.clone(),
            capabilities: ClientCapabilities::default(),
            workspace_folders,
            ..Default::default()
        };

        let result: InitializeResult = self.send_request_sequential(Initialize::METHOD, Some(params), pending).await?;

        self.capabilities = Some(result.capabilities.clone());

        // Send initialized notification
        self.send_notification::<Initialized>(InitializedParams {}).await?;

        self.initialized = true;

        // Notify main loop
        let _ = self.async_tx.send(AsyncMessage::LspInitialized {
            language: self.language.clone(),
        });

        tracing::info!("Async LSP server initialized successfully");

        Ok(result)
    }

    /// Handle did_open command
    async fn handle_did_open_sequential(
        &mut self,
        uri: Url,
        text: String,
        language_id: String,
        _pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        tracing::debug!("LSP: did_open for {}", uri);

        let params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id,
                version: 0,
                text,
            },
        };

        self.document_versions.insert(PathBuf::from(uri.path()), 0);

        self.send_notification::<DidOpenTextDocument>(params).await
    }

    /// Handle did_change command
    async fn handle_did_change_sequential(
        &mut self,
        uri: Url,
        content_changes: Vec<TextDocumentContentChangeEvent>,
        _pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        tracing::debug!("LSP: did_change for {}", uri);

        let path = PathBuf::from(uri.path());
        let version = self.document_versions.entry(path).or_insert(0);
        *version += 1;

        let params = DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: *version as i32,
            },
            content_changes,
        };

        self.send_notification::<DidChangeTextDocument>(params).await
    }

    /// Handle did_save command
    async fn handle_did_save(
        &mut self,
        uri: Url,
        text: Option<String>,
    ) -> Result<(), String> {
        tracing::debug!("LSP: did_save for {}", uri);

        let params = DidSaveTextDocumentParams {
            text_document: TextDocumentIdentifier { uri },
            text,
        };

        self.send_notification::<DidSaveTextDocument>(params).await
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

impl LspTask {
    /// Create a new LSP task
    async fn spawn(
        command: &str,
        args: &[String],
        language: String,
        async_tx: std_mpsc::Sender<AsyncMessage>,
    ) -> Result<Self, String> {
        tracing::info!("Spawning async LSP server: {} {:?}", command, args);

        let mut process = Command::new(command)
            .args(args)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .kill_on_drop(true)
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
    async fn run(mut self, mut command_rx: mpsc::Receiver<LspCommand>) {
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
        };

        // Move stdout out, share pending
        let mut stdout = self.stdout;
        let pending = Arc::new(Mutex::new(self.pending));

        let async_tx = state.async_tx.clone();
        let language_clone = state.language.clone();

        // Spawn stdout reader task - continuously reads and dispatches messages
        let pending_clone = pending.clone();
        tokio::spawn(async move {
            tracing::info!("LSP stdout reader task started for {}", language_clone);
            loop {
                match read_message_from_stdout(&mut stdout).await {
                    Ok(message) => {
                        tracing::debug!("Read message from LSP server: {:?}", message);
                        if let Err(e) = handle_message_dispatch(message, &pending_clone, &async_tx, &language_clone).await {
                            tracing::error!("Error handling LSP message: {}", e);
                        }
                    }
                    Err(e) => {
                        tracing::error!("Error reading from LSP server: {}", e);
                        let _ = async_tx.send(AsyncMessage::LspError {
                            language: language_clone.clone(),
                            error: format!("Read error: {}", e),
                        });
                        break;
                    }
                }
            }
            tracing::info!("LSP stdout reader task exiting for {}", language_clone);
        });

        // Sequential command processing loop
        let mut pending_commands = Vec::new();
        loop {
            match command_rx.recv().await {
                Some(cmd) => {
                    tracing::debug!("LspTask received command: {:?}", cmd);
                    match cmd {
                        LspCommand::Initialize { root_uri, response } => {
                            tracing::info!("Processing Initialize command");
                            let result = state.handle_initialize_sequential(root_uri, &pending).await;
                            let success = result.is_ok();
                            let _ = response.send(result);

                            // After successful initialization, replay pending commands
                            if success {
                                let queued = std::mem::take(&mut pending_commands);
                                if !queued.is_empty() {
                                    tracing::info!("Replaying {} pending commands after initialization", queued.len());
                                    for queued_cmd in queued {
                                        match queued_cmd {
                                            LspCommand::DidOpen { uri, text, language_id } => {
                                                tracing::info!("Replaying DidOpen for {}", uri);
                                                let _ = state.handle_did_open_sequential(uri, text, language_id, &pending).await;
                                            }
                                            LspCommand::DidChange { uri, content_changes } => {
                                                tracing::info!("Replaying DidChange for {}", uri);
                                                let _ = state.handle_did_change_sequential(uri, content_changes, &pending).await;
                                            }
                                            LspCommand::DidSave { uri, text } => {
                                                tracing::info!("Replaying DidSave for {}", uri);
                                                let _ = state.handle_did_save(uri, text).await;
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                            }
                        }
                        LspCommand::DidOpen { uri, text, language_id } => {
                            if state.initialized {
                                tracing::info!("Processing DidOpen for {}", uri);
                                let _ = state.handle_did_open_sequential(uri, text, language_id, &pending).await;
                            } else {
                                tracing::debug!("Queueing DidOpen for {} until initialization completes", uri);
                                pending_commands.push(LspCommand::DidOpen { uri, text, language_id });
                            }
                        }
                        LspCommand::DidChange { uri, content_changes } => {
                            if state.initialized {
                                tracing::debug!("Processing DidChange for {}", uri);
                                let _ = state.handle_did_change_sequential(uri, content_changes, &pending).await;
                            } else {
                                tracing::debug!("Queueing DidChange for {} until initialization completes", uri);
                                pending_commands.push(LspCommand::DidChange { uri, content_changes });
                            }
                        }
                        LspCommand::DidSave { uri, text } => {
                            if state.initialized {
                                tracing::info!("Processing DidSave for {}", uri);
                                let _ = state.handle_did_save(uri, text).await;
                            } else {
                                tracing::debug!("Queueing DidSave for {} until initialization completes", uri);
                                pending_commands.push(LspCommand::DidSave { uri, text });
                            }
                        }
                        LspCommand::Shutdown => {
                            tracing::info!("Processing Shutdown command");
                            let _ = state.handle_shutdown().await;
                            break;
                        }
                    }
                }
                None => {
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
        root_uri: Option<Url>,
        pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<InitializeResult, String> {
        tracing::info!("Initializing async LSP server with root_uri: {:?}", root_uri);

        let workspace_folders = root_uri.as_ref().map(|uri| {
            vec![WorkspaceFolder {
                uri: uri.clone(),
                name: uri
                    .path()
                    .split('/')
                    .last()
                    .unwrap_or("workspace")
                    .to_string(),
            }]
        });

        let params = InitializeParams {
            process_id: Some(std::process::id()),
            root_uri: root_uri.clone(),
            capabilities: ClientCapabilities::default(),
            workspace_folders,
            ..Default::default()
        };

        let result: InitializeResult = self.send_request_sequential(Initialize::METHOD, Some(params), pending).await?;

        self.capabilities = Some(result.capabilities.clone());

        // Send initialized notification
        self.send_notification::<Initialized>(InitializedParams {}).await?;

        self.initialized = true;

        // Notify main loop
        let _ = self.async_tx.send(AsyncMessage::LspInitialized {
            language: self.language.clone(),
        });

        tracing::info!("Async LSP server initialized successfully");

        Ok(result)
    }

    /// Sequential version of handle_did_open
    async fn handle_did_open_sequential(
        &mut self,
        uri: Url,
        text: String,
        language_id: String,
        _pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        tracing::debug!("LSP: did_open for {}", uri);

        let params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id,
                version: 0,
                text,
            },
        };

        self.document_versions.insert(PathBuf::from(uri.path()), 0);

        self.send_notification::<DidOpenTextDocument>(params).await
    }

    /// Sequential version of handle_did_change
    async fn handle_did_change_sequential(
        &mut self,
        uri: Url,
        content_changes: Vec<TextDocumentContentChangeEvent>,
        _pending: &Arc<Mutex<HashMap<i64, oneshot::Sender<Result<Value, String>>>>>,
    ) -> Result<(), String> {
        tracing::debug!("LSP: did_change for {}", uri);

        let path = PathBuf::from(uri.path());
        let version = self.document_versions.entry(path).or_insert(0);
        *version += 1;

        let params = DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: *version as i32,
            },
            content_changes,
        };

        self.send_notification::<DidChangeTextDocument>(params).await
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
                        Err(format!("LSP error: {} (code {})", error.message, error.code))
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
    async fn handle_notification(&mut self, notification: JsonRpcNotification) -> Result<(), String> {
        match notification.method.as_str() {
            PublishDiagnostics::METHOD => {
                if let Some(params) = notification.params {
                    let params: PublishDiagnosticsParams = serde_json::from_value(params)
                        .map_err(|e| format!("Failed to deserialize diagnostics: {}", e))?;

                    tracing::debug!(
                        "Received {} diagnostics for {}",
                        params.diagnostics.len(),
                        params.uri
                    );

                    // Send to main loop
                    let _ = self.async_tx.send(AsyncMessage::LspDiagnostics {
                        uri: params.uri.to_string(),
                        diagnostics: params.diagnostics,
                    });
                }
            }
            "window/showMessage" | "window/logMessage" => {
                if let Some(params) = notification.params {
                    if let Ok(msg) = serde_json::from_value::<serde_json::Map<String, Value>>(params)
                    {
                        let message_type = msg.get("type").and_then(|v| v.as_i64()).unwrap_or(0);
                        let message = msg
                            .get("message")
                            .and_then(|v| v.as_str())
                            .unwrap_or("(no message)");

                        match message_type {
                            1 => tracing::error!("LSP: {}", message),
                            2 => tracing::warn!("LSP: {}", message),
                            3 => tracing::info!("LSP: {}", message),
                            4 => tracing::debug!("LSP: {}", message),
                            _ => tracing::trace!("LSP: {}", message),
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
        stdout
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
) -> Result<(), String> {
    match message {
        JsonRpcMessage::Response(response) => {
            tracing::debug!("Received LSP response for request id={}", response.id);
            if let Some(tx) = pending.lock().unwrap().remove(&response.id) {
                let result = if let Some(error) = response.error {
                    tracing::warn!("LSP response error: {} (code {})", error.message, error.code);
                    Err(format!("LSP error: {} (code {})", error.message, error.code))
                } else {
                    tracing::debug!("LSP response success for request id={}", response.id);
                    response
                        .result
                        .ok_or_else(|| "No result in response".to_string())
                };
                let _ = tx.send(result);
            } else {
                tracing::warn!("Received LSP response for unknown request id={}", response.id);
            }
        }
        JsonRpcMessage::Notification(notification) => {
            tracing::debug!("Received LSP notification: {}", notification.method);
            handle_notification_dispatch(notification, async_tx, language).await?;
        }
        JsonRpcMessage::Request(_) => {
            tracing::warn!("Received request from server, ignoring");
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
                    params.uri
                );

                // Send to main loop
                let _ = async_tx.send(AsyncMessage::LspDiagnostics {
                    uri: params.uri.to_string(),
                    diagnostics: params.diagnostics,
                });
            }
        }
        "window/showMessage" | "window/logMessage" => {
            if let Some(params) = notification.params {
                if let Ok(msg) = serde_json::from_value::<serde_json::Map<String, Value>>(params)
                {
                    let message_type = msg.get("type").and_then(|v| v.as_i64()).unwrap_or(0);
                    let message = msg
                        .get("message")
                        .and_then(|v| v.as_str())
                        .unwrap_or("(no message)");

                    match message_type {
                        1 => tracing::error!("LSP ({}): {}", language, message),
                        2 => tracing::warn!("LSP ({}): {}", language, message),
                        3 => tracing::info!("LSP ({}): {}", language, message),
                        4 => tracing::debug!("LSP ({}): {}", language, message),
                        _ => tracing::trace!("LSP ({}): {}", language, message),
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

    /// Language ID
    language: String,

    /// Whether initialized
    initialized: Arc<Mutex<bool>>,

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
    ) -> Result<Self, String> {
        let (command_tx, command_rx) = mpsc::channel(100); // Buffer up to 100 commands
        let async_tx = async_bridge.sender();
        let language_clone = language.clone();
        let command = command.to_string();
        let args = args.to_vec();
        let initialized = Arc::new(Mutex::new(false));
        let initialized_clone = initialized.clone();

        runtime.spawn(async move {
            match LspTask::spawn(&command, &args, language_clone.clone(), async_tx.clone()).await {
                Ok(task) => {
                    task.run(command_rx).await;
                }
                Err(e) => {
                    tracing::error!("Failed to spawn LSP task: {}", e);
                    let _ = async_tx.send(AsyncMessage::LspError {
                        language: language_clone,
                        error: e,
                    });
                }
            }
        });

        Ok(Self {
            command_tx,
            language,
            initialized,
            runtime: runtime.clone(),
        })
    }

    /// Initialize the server (non-blocking)
    ///
    /// This sends the initialize request asynchronously. The server will be ready
    /// when `is_initialized()` returns true. Other methods that require initialization
    /// will fail gracefully until then.
    pub fn initialize(&self, root_uri: Option<Url>) -> Result<(), String> {
        let initialized = self.initialized.clone();

        // Create a channel for the response, but don't wait for it
        let (tx, rx) = oneshot::channel();

        self.command_tx
            .blocking_send(LspCommand::Initialize {
                root_uri,
                response: tx,
            })
            .map_err(|_| "Failed to send initialize command".to_string())?;

        // Spawn a task to wait for the response and update the initialized flag
        let runtime = self.runtime.clone();
        runtime.spawn(async move {
            match tokio::time::timeout(std::time::Duration::from_secs(10), rx).await {
                Ok(Ok(Ok(_))) => {
                    *initialized.lock().unwrap() = true;
                    tracing::info!("LSP initialization completed successfully");
                }
                Ok(Ok(Err(e))) => {
                    tracing::error!("LSP initialization failed: {}", e);
                }
                Ok(Err(_)) => {
                    tracing::error!("LSP initialization response channel closed");
                }
                Err(_) => {
                    tracing::error!("LSP initialization timed out after 10 seconds");
                }
            }
        });

        Ok(())
    }

    /// Check if the server is initialized
    pub fn is_initialized(&self) -> bool {
        *self.initialized.lock().unwrap()
    }

    /// Notify document opened
    pub fn did_open(&self, uri: Url, text: String, language_id: String) -> Result<(), String> {
        // Send command to LspTask which will queue it if not initialized yet
        self.command_tx
            .blocking_send(LspCommand::DidOpen {
                uri,
                text,
                language_id,
            })
            .map_err(|_| "Failed to send did_open command".to_string())
    }

    /// Notify document changed
    pub fn did_change(
        &self,
        uri: Url,
        content_changes: Vec<TextDocumentContentChangeEvent>,
    ) -> Result<(), String> {
        // Send command to LspTask which will queue it if not initialized yet
        self.command_tx
            .blocking_send(LspCommand::DidChange {
                uri,
                content_changes,
            })
            .map_err(|_| "Failed to send did_change command".to_string())
    }

    /// Send didSave notification
    pub fn did_save(&self, uri: Url, text: Option<String>) -> Result<(), String> {
        self.command_tx
            .blocking_send(LspCommand::DidSave { uri, text })
            .map_err(|_| "Failed to send did_save command".to_string())
    }

    /// Shutdown the server
    pub fn shutdown(&self) -> Result<(), String> {
        self.command_tx
            .blocking_send(LspCommand::Shutdown)
            .map_err(|_| "Failed to send shutdown command".to_string())
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
        let json = r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"rootUri":"file:///test"}}"#;
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
        let json = r#"{"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"Invalid request"}}"#;
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
        let result = LspHandle::spawn(&runtime, "cat", &[], "test".to_string(), &async_bridge);

        // Should succeed in spawning
        assert!(result.is_ok());

        let handle = result.unwrap();

        // Let handle drop (which calls shutdown via Drop impl)
        drop(handle);

        // Give task time to receive shutdown and exit
        tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
    }

    #[tokio::test]
    async fn test_lsp_handle_did_open_requires_initialization() {
        let runtime = tokio::runtime::Handle::current();
        let async_bridge = AsyncBridge::new();

        let handle = LspHandle::spawn(&runtime, "cat", &[], "test".to_string(), &async_bridge)
            .unwrap();

        // did_open should fail because server is not initialized
        let result = handle.did_open(
            Url::parse("file:///test.rs").unwrap(),
            "fn main() {}".to_string(),
            "rust".to_string(),
        );

        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .contains("LSP client not initialized"));
    }

    #[tokio::test]
    async fn test_lsp_handle_did_change_requires_initialization() {
        let runtime = tokio::runtime::Handle::current();
        let async_bridge = AsyncBridge::new();

        let handle = LspHandle::spawn(&runtime, "cat", &[], "test".to_string(), &async_bridge)
            .unwrap();

        // did_change should fail because server is not initialized
        let result = handle.did_change(
            Url::parse("file:///test.rs").unwrap(),
            vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "fn main() {}".to_string(),
            }],
        );

        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .contains("LSP client not initialized"));
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
        );

        // Should succeed in creating handle (error happens asynchronously)
        // The error will be sent to async_bridge
        assert!(result.is_ok());

        // Give the task time to fail
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

        // Check that we received an error message
        let messages = async_bridge.try_recv_all();
        assert!(!messages.is_empty());

        let has_error = messages.iter().any(|msg| matches!(msg, AsyncMessage::LspError { .. }));
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
                LspHandle::spawn(&runtime, "cat", &[], "test".to_string(), &async_bridge).unwrap()
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
}
