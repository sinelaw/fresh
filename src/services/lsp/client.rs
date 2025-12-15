// Re-export LspServerConfig from the shared types module
pub use crate::types::LspServerConfig;

use lsp_types::{
    notification::{Notification, PublishDiagnostics},
    request::{Initialize, Request, Shutdown},
    ClientCapabilities, Diagnostic, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    InitializeParams, InitializeResult, InitializedParams, PublishDiagnosticsParams,
    ServerCapabilities, TextDocumentContentChangeEvent, TextDocumentItem, Uri,
    VersionedTextDocumentIdentifier, WorkspaceFolder,
};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::io::{BufRead, BufReader, BufWriter, Read, Write};
use std::path::PathBuf;
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};

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

/// LSP client for communicating with a language server
pub struct LspClient {
    /// Process handle for the language server
    process: Child,

    /// Stdin writer for sending requests
    stdin: BufWriter<ChildStdin>,

    /// Stdout reader for receiving responses
    stdout: BufReader<ChildStdout>,

    /// Next request ID
    next_id: i64,

    /// Pending requests waiting for response (request ID tracking)
    pending: HashMap<i64, ()>,

    /// Server capabilities after initialization
    capabilities: Option<ServerCapabilities>,

    /// Current document versions (for incremental sync)
    document_versions: HashMap<PathBuf, i64>,

    /// Diagnostics per file
    diagnostics: HashMap<Uri, Vec<Diagnostic>>,

    /// Whether the server has been initialized
    initialized: bool,
}

impl LspClient {
    /// Spawn a new language server process
    pub fn spawn(command: &str, args: &[String]) -> std::io::Result<Self> {
        tracing::info!("Spawning LSP server: {} {:?}", command, args);

        let mut process = Command::new(command)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        let stdin = BufWriter::new(process.stdin.take().ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::Other, "Failed to get stdin")
        })?);

        let stdout = BufReader::new(process.stdout.take().ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::Other, "Failed to get stdout")
        })?);

        Ok(Self {
            process,
            stdin,
            stdout,
            next_id: 0,
            pending: HashMap::new(),
            capabilities: None,
            document_versions: HashMap::new(),
            diagnostics: HashMap::new(),
            initialized: false,
        })
    }

    /// Initialize the language server
    pub fn initialize(&mut self, root_uri: Option<Uri>) -> Result<InitializeResult, String> {
        tracing::info!("Initializing LSP server with root_uri: {:?}", root_uri);

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
            capabilities: ClientCapabilities::default(),
            workspace_folders,
            ..Default::default()
        };

        let result: InitializeResult = self.send_request(Initialize::METHOD, Some(params))?;

        self.capabilities = Some(result.capabilities.clone());

        // Send initialized notification
        self.send_notification("initialized", Some(InitializedParams {}))?;

        self.initialized = true;

        tracing::info!("LSP server initialized successfully");

        Ok(result)
    }

    /// Notify server of document open
    pub fn did_open(&mut self, uri: Uri, text: String, language_id: String) -> Result<(), String> {
        if !self.initialized {
            return Err("LSP client not initialized".to_string());
        }

        tracing::debug!("LSP: did_open for {}", uri.as_str());

        let version: i64 = 1;
        if let Some(path) = url::Url::parse(uri.as_str())
            .ok()
            .and_then(|u| u.to_file_path().ok())
        {
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
    }

    /// Notify server of document change
    pub fn did_change(
        &mut self,
        uri: Uri,
        content_changes: Vec<TextDocumentContentChangeEvent>,
    ) -> Result<(), String> {
        if !self.initialized {
            return Err("LSP client not initialized".to_string());
        }

        tracing::debug!("LSP: did_change for {}", uri.as_str());

        // Increment version
        let version = if let Some(path) = url::Url::parse(uri.as_str())
            .ok()
            .and_then(|u| u.to_file_path().ok())
        {
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
    }

    /// Get diagnostics for a file
    pub fn diagnostics(&self, uri: &Uri) -> Vec<Diagnostic> {
        self.diagnostics.get(uri).cloned().unwrap_or_default()
    }

    /// Shutdown the language server
    pub fn shutdown(&mut self) -> Result<(), String> {
        if !self.initialized {
            return Ok(());
        }

        tracing::info!("Shutting down LSP server");

        // Send shutdown request
        let _: Value = self.send_request(Shutdown::METHOD, Option::<()>::None)?;

        // Send exit notification
        self.send_notification("exit", Option::<()>::None)?;

        // Kill the process if it doesn't exit gracefully
        let _ = self.process.kill();

        Ok(())
    }

    /// Send a request and wait for response
    fn send_request<P: Serialize, R: for<'de> Deserialize<'de>>(
        &mut self,
        method: &str,
        params: Option<P>,
    ) -> Result<R, String> {
        let id = self.next_id;
        self.next_id += 1;

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

        self.pending.insert(id, ());

        self.write_message(&request)?;

        // Wait for response
        loop {
            let message = self.read_message()?;

            match message {
                JsonRpcMessage::Response(response) if response.id == id => {
                    self.pending.remove(&id);

                    if let Some(error) = response.error {
                        return Err(format!(
                            "LSP error: {} (code {})",
                            error.message, error.code
                        ));
                    }

                    let result = response
                        .result
                        .ok_or_else(|| "No result in response".to_string())?;

                    return serde_json::from_value(result)
                        .map_err(|e| format!("Failed to deserialize response: {}", e));
                }
                JsonRpcMessage::Notification(notification) => {
                    self.handle_notification(notification)?;
                }
                JsonRpcMessage::Request(_) => {
                    // Ignore server requests for now
                    tracing::warn!("Received request from server, ignoring");
                }
                JsonRpcMessage::Response(_) => {
                    // Response for a different request, ignore
                }
            }
        }
    }

    /// Send a notification (no response expected)
    fn send_notification<P: Serialize>(
        &mut self,
        method: &str,
        params: Option<P>,
    ) -> Result<(), String> {
        let params_value = params
            .map(|p| serde_json::to_value(p))
            .transpose()
            .map_err(|e| format!("Failed to serialize params: {}", e))?;
        let notification = JsonRpcNotification {
            jsonrpc: "2.0".to_string(),
            method: method.to_string(),
            params: params_value,
        };

        self.write_message(&notification)
    }

    /// Write a message to the server
    fn write_message<T: Serialize>(&mut self, message: &T) -> Result<(), String> {
        let json =
            serde_json::to_string(message).map_err(|e| format!("Serialization error: {}", e))?;

        let content = format!("Content-Length: {}\r\n\r\n{}", json.len(), json);

        self.stdin
            .write_all(content.as_bytes())
            .map_err(|e| format!("Failed to write to stdin: {}", e))?;

        self.stdin
            .flush()
            .map_err(|e| format!("Failed to flush stdin: {}", e))?;

        tracing::trace!("Sent LSP message: {}", json);

        Ok(())
    }

    /// Read a message from the server
    fn read_message(&mut self) -> Result<JsonRpcMessage, String> {
        // Read headers
        let mut content_length: Option<usize> = None;

        loop {
            let mut line = String::new();
            self.stdout
                .read_line(&mut line)
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
            .map_err(|e| format!("Failed to read content: {}", e))?;

        let json = String::from_utf8(content).map_err(|e| format!("Invalid UTF-8: {}", e))?;

        tracing::trace!("Received LSP message: {}", json);

        serde_json::from_str(&json).map_err(|e| format!("Failed to deserialize message: {}", e))
    }

    /// Handle a notification from the server
    fn handle_notification(&mut self, notification: JsonRpcNotification) -> Result<(), String> {
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

                    self.diagnostics.insert(params.uri, params.diagnostics);
                }
            }
            "window/showMessage" | "window/logMessage" => {
                if let Some(params) = notification.params {
                    if let Ok(msg) =
                        serde_json::from_value::<serde_json::Map<String, Value>>(params)
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

    /// Check for incoming messages without blocking
    pub fn poll(&mut self) -> Result<(), String> {
        // For now, we don't poll - we only read responses synchronously
        // In a real implementation, we'd use non-blocking I/O or a separate thread
        Ok(())
    }
}

impl Drop for LspClient {
    fn drop(&mut self) {
        let _ = self.shutdown();
    }
}

/// Manager for multiple language servers
pub struct LspManager {
    /// Map from language ID to LSP client
    clients: HashMap<String, LspClient>,

    /// Configuration for each language
    config: HashMap<String, LspServerConfig>,

    /// Root URI for workspace
    root_uri: Option<Uri>,
}

impl LspManager {
    /// Create a new LSP manager
    pub fn new(root_uri: Option<Uri>) -> Self {
        Self {
            clients: HashMap::new(),
            config: HashMap::new(),
            root_uri,
        }
    }

    /// Set configuration for a language
    pub fn set_language_config(&mut self, language: String, config: LspServerConfig) {
        self.config.insert(language, config);
    }

    /// Get or spawn an LSP client for a language
    pub fn get_or_spawn(&mut self, language: &str) -> Option<&mut LspClient> {
        // Return existing client if available
        if self.clients.contains_key(language) {
            return self.clients.get_mut(language);
        }

        // Get config for this language
        let config = self.config.get(language)?;

        if !config.enabled {
            return None;
        }

        // Spawn new client
        tracing::info!("Spawning LSP server for language: {}", language);

        match LspClient::spawn(&config.command, &config.args) {
            Ok(mut client) => {
                // Initialize the client
                match client.initialize(self.root_uri.clone()) {
                    Ok(_) => {
                        self.clients.insert(language.to_string(), client);
                        self.clients.get_mut(language)
                    }
                    Err(e) => {
                        tracing::error!("Failed to initialize LSP server for {}: {}", language, e);
                        None
                    }
                }
            }
            Err(e) => {
                tracing::error!("Failed to spawn LSP server for {}: {}", language, e);
                None
            }
        }
    }

    /// Shutdown all language servers
    pub fn shutdown_all(&mut self) {
        for (language, client) in self.clients.iter_mut() {
            tracing::info!("Shutting down LSP server for {}", language);
            let _ = client.shutdown();
        }
        self.clients.clear();
    }

    /// Get diagnostics for a file from all servers
    pub fn diagnostics(&self, uri: &Uri) -> Vec<Diagnostic> {
        let mut all_diagnostics = Vec::new();
        for client in self.clients.values() {
            all_diagnostics.extend(client.diagnostics(uri));
        }
        all_diagnostics
    }
}

impl Drop for LspManager {
    fn drop(&mut self) {
        self.shutdown_all();
    }
}
