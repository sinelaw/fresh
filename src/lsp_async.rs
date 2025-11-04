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
    notification::{Notification, PublishDiagnostics},
    request::{Initialize, Request, Shutdown},
    ClientCapabilities, Diagnostic, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    InitializeParams, InitializeResult, InitializedParams, PublishDiagnosticsParams,
    ServerCapabilities, TextDocumentContentChangeEvent, TextDocumentItem, Url,
    VersionedTextDocumentIdentifier, WorkspaceFolder,
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

    /// Shutdown the server
    Shutdown,
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
        loop {
            tokio::select! {
                // Handle commands from main loop
                Some(cmd) = command_rx.recv() => {
                    match cmd {
                        LspCommand::Initialize { root_uri, response } => {
                            let result = self.handle_initialize(root_uri).await;
                            let _ = response.send(result);
                        }
                        LspCommand::DidOpen { uri, text, language_id } => {
                            let _ = self.handle_did_open(uri, text, language_id).await;
                        }
                        LspCommand::DidChange { uri, content_changes } => {
                            let _ = self.handle_did_change(uri, content_changes).await;
                        }
                        LspCommand::Shutdown => {
                            let _ = self.handle_shutdown().await;
                            break;
                        }
                    }
                }

                // Read messages from server
                result = self.read_message() => {
                    match result {
                        Ok(message) => {
                            if let Err(e) = self.handle_message(message).await {
                                tracing::error!("Error handling LSP message: {}", e);
                            }
                        }
                        Err(e) => {
                            tracing::error!("Error reading from LSP server: {}", e);
                            let _ = self.async_tx.send(AsyncMessage::LspError {
                                language: self.language.clone(),
                                error: format!("Read error: {}", e),
                            });
                            break;
                        }
                    }
                }
            }
        }

        tracing::info!("LSP task exiting for language: {}", self.language);
    }

    /// Handle initialize command
    async fn handle_initialize(&mut self, root_uri: Option<Url>) -> Result<InitializeResult, String> {
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

        let result: InitializeResult = self
            .send_request(Initialize::METHOD, Some(params))
            .await?;

        self.capabilities = Some(result.capabilities.clone());

        // Send initialized notification
        self.send_notification("initialized", Some(InitializedParams {}))
            .await?;

        self.initialized = true;

        // Notify main loop
        let _ = self.async_tx.send(AsyncMessage::LspInitialized {
            language: self.language.clone(),
        });

        tracing::info!("Async LSP server initialized successfully");

        Ok(result)
    }

    /// Handle did_open command
    async fn handle_did_open(
        &mut self,
        uri: Url,
        text: String,
        language_id: String,
    ) -> Result<(), String> {
        if !self.initialized {
            return Err("LSP client not initialized".to_string());
        }

        tracing::debug!("LSP: did_open for {}", uri);

        let version: i64 = 1;
        if let Ok(path) = uri.to_file_path() {
            self.document_versions.insert(path, version);
        }

        let params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri,
                language_id,
                version: version as i32,
                text,
            },
        };

        self.send_notification("textDocument/didOpen", Some(params))
            .await
    }

    /// Handle did_change command
    async fn handle_did_change(
        &mut self,
        uri: Url,
        content_changes: Vec<TextDocumentContentChangeEvent>,
    ) -> Result<(), String> {
        if !self.initialized {
            return Err("LSP client not initialized".to_string());
        }

        tracing::debug!("LSP: did_change for {}", uri);

        // Increment version
        let version = if let Ok(path) = uri.to_file_path() {
            let v = self.document_versions.entry(path).or_insert(0);
            *v += 1;
            *v
        } else {
            1
        };

        let params = DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri,
                version: version as i32,
            },
            content_changes,
        };

        self.send_notification("textDocument/didChange", Some(params))
            .await
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

        // Send exit notification
        self.send_notification("exit", Option::<()>::None).await?;

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

    /// Send a notification
    async fn send_notification<P: Serialize>(
        &mut self,
        method: &str,
        params: Option<P>,
    ) -> Result<(), String> {
        let notification = JsonRpcNotification {
            jsonrpc: "2.0".to_string(),
            method: method.to_string(),
            params: params.map(|p| serde_json::to_value(p).expect("Failed to serialize params")),
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

/// Synchronous handle to an async LSP task
pub struct LspHandle {
    /// Channel for sending commands to the task
    command_tx: mpsc::Sender<LspCommand>,

    /// Language ID
    language: String,

    /// Whether initialized
    initialized: Arc<Mutex<bool>>,
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
        })
    }

    /// Initialize the server
    pub fn initialize(&self, root_uri: Option<Url>) -> Result<InitializeResult, String> {
        let (tx, rx) = oneshot::channel();

        self.command_tx
            .blocking_send(LspCommand::Initialize {
                root_uri,
                response: tx,
            })
            .map_err(|_| "Failed to send initialize command".to_string())?;

        let result = rx
            .blocking_recv()
            .map_err(|_| "Failed to receive initialize response".to_string())??;

        *self.initialized.lock().unwrap() = true;

        Ok(result)
    }

    /// Notify document opened
    pub fn did_open(&self, uri: Url, text: String, language_id: String) -> Result<(), String> {
        if !*self.initialized.lock().unwrap() {
            return Err("LSP client not initialized".to_string());
        }

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
        if !*self.initialized.lock().unwrap() {
            return Err("LSP client not initialized".to_string());
        }

        self.command_tx
            .blocking_send(LspCommand::DidChange {
                uri,
                content_changes,
            })
            .map_err(|_| "Failed to send did_change command".to_string())
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
        let _ = self.shutdown();
    }
}
