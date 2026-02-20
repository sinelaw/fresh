//! Server runner - main loop for the daemon process
//!
//! The server:
//! 1. Binds to IPC sockets (data + control)
//! 2. Accepts client connections
//! 3. Performs handshake (version check, environment exchange)
//! 4. Relays input from clients to the editor
//! 5. Sends rendered output back to clients

use std::io;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use crate::server::capture_backend::{terminal_setup_sequences, terminal_teardown_sequences};
use crate::server::input_parser::InputParser;
use crate::server::ipc::{ServerConnection, ServerListener, SocketPaths};
use crate::server::protocol::{
    ClientControl, ServerControl, ServerHello, TermSize, VersionMismatch, PROTOCOL_VERSION,
};
use crossterm::event::Event;

/// Server configuration
#[derive(Debug, Clone)]
pub struct ServerConfig {
    /// Working directory for this session
    pub working_dir: std::path::PathBuf,
    /// Optional session name (otherwise derived from working_dir)
    pub session_name: Option<String>,
    /// Idle timeout before auto-shutdown (None = never)
    pub idle_timeout: Option<Duration>,
}

/// Server state
pub struct Server {
    config: ServerConfig,
    listener: ServerListener,
    clients: Vec<ConnectedClient>,
    /// Last time a client was connected (for idle timeout)
    last_client_activity: Instant,
    /// Flag to signal shutdown
    shutdown: Arc<AtomicBool>,
}

/// A connected client
pub struct ConnectedClient {
    /// Client connection (data + control sockets)
    conn: ServerConnection,
    /// Client's terminal size
    term_size: TermSize,
    /// Client's environment
    #[allow(dead_code)]
    env: std::collections::HashMap<String, Option<String>>,
    /// Client ID for logging
    id: u64,
    /// Input parser for converting raw bytes to events
    input_parser: InputParser,
}

impl Server {
    /// Create a new server for the given working directory
    pub fn new(config: ServerConfig) -> io::Result<Self> {
        let socket_paths = if let Some(ref name) = config.session_name {
            SocketPaths::for_session_name(name)?
        } else {
            SocketPaths::for_working_dir(&config.working_dir)?
        };

        let listener = ServerListener::bind(socket_paths)?;

        Ok(Self {
            config,
            listener,
            clients: Vec::new(),
            last_client_activity: Instant::now(),
            shutdown: Arc::new(AtomicBool::new(false)),
        })
    }

    /// Get a handle to request shutdown
    pub fn shutdown_handle(&self) -> Arc<AtomicBool> {
        self.shutdown.clone()
    }

    /// Run the server main loop
    ///
    /// This is a basic implementation that handles connections and handshakes.
    /// The full implementation will integrate with the Editor.
    pub fn run(&mut self) -> io::Result<()> {
        tracing::info!("Server starting for {:?}", self.config.working_dir);

        let mut next_client_id = 1u64;

        loop {
            // Check for shutdown
            if self.shutdown.load(Ordering::SeqCst) {
                tracing::info!("Shutdown requested");
                break;
            }

            // Check idle timeout
            if let Some(timeout) = self.config.idle_timeout {
                if self.clients.is_empty() && self.last_client_activity.elapsed() > timeout {
                    tracing::info!("Idle timeout reached, shutting down");
                    break;
                }
            }

            // Try to accept new connections
            match self.listener.accept() {
                Ok(Some(conn)) => match self.handle_new_connection(conn, next_client_id) {
                    Ok(client) => {
                        tracing::info!("Client {} connected", client.id);
                        self.clients.push(client);
                        self.last_client_activity = Instant::now();
                        next_client_id += 1;
                    }
                    Err(e) => {
                        tracing::warn!("Failed to complete handshake: {}", e);
                    }
                },
                Ok(None) => {
                    // No pending connection
                }
                Err(e) => {
                    tracing::error!("Accept error: {}", e);
                }
            }

            // Process existing clients
            let input_events = self.process_clients()?;

            // TODO: Send input events to the Editor
            // For now, just update activity timestamp if we got events
            if !input_events.is_empty() {
                self.last_client_activity = Instant::now();
            }

            // Brief sleep to avoid busy-waiting
            std::thread::sleep(Duration::from_millis(10));
        }

        // Send quit message to all clients
        self.disconnect_all_clients("Server shutting down")?;

        Ok(())
    }

    /// Handle a new client connection (perform handshake)
    fn handle_new_connection(
        &self,
        conn: ServerConnection,
        client_id: u64,
    ) -> io::Result<ConnectedClient> {
        // Read client hello
        conn.control.set_nonblocking(false)?;
        let hello_json = conn
            .read_control()?
            .ok_or_else(|| io::Error::new(io::ErrorKind::UnexpectedEof, "No hello received"))?;

        let client_msg: ClientControl = serde_json::from_str(&hello_json)
            .map_err(|e| io::Error::other(format!("Invalid hello: {}", e)))?;

        let hello = match client_msg {
            ClientControl::Hello(h) => h,
            _ => {
                return Err(io::Error::other("Expected Hello message"));
            }
        };

        // Check protocol version
        if hello.protocol_version != PROTOCOL_VERSION {
            let mismatch = VersionMismatch {
                server_version: env!("CARGO_PKG_VERSION").to_string(),
                client_version: hello.client_version.clone(),
                action: if hello.protocol_version > PROTOCOL_VERSION {
                    "upgrade_server".to_string()
                } else {
                    "restart_server".to_string()
                },
                message: format!(
                    "Protocol version mismatch: server={}, client={}",
                    PROTOCOL_VERSION, hello.protocol_version
                ),
            };

            let response = serde_json::to_string(&ServerControl::VersionMismatch(mismatch))
                .map_err(|e| io::Error::other(e.to_string()))?;
            conn.write_control(&response)?;

            return Err(io::Error::other("Version mismatch"));
        }

        // Send server hello
        let session_id = self.config.session_name.clone().unwrap_or_else(|| {
            crate::workspace::encode_path_for_filename(&self.config.working_dir)
        });

        let server_hello = ServerHello::new(session_id);
        let response = serde_json::to_string(&ServerControl::Hello(server_hello))
            .map_err(|e| io::Error::other(e.to_string()))?;
        conn.write_control(&response)?;

        tracing::debug!(
            "Client {} hello: term_size={}x{}, TERM={:?}",
            client_id,
            hello.term_size.cols,
            hello.term_size.rows,
            hello.term()
        );

        // Set sockets back to non-blocking for normal operation
        conn.control.set_nonblocking(true)?;

        // Send terminal setup sequences to initialize the client's terminal
        let setup = terminal_setup_sequences();
        conn.write_data(&setup)?;

        Ok(ConnectedClient {
            conn,
            term_size: hello.term_size,
            env: hello.env,
            id: client_id,
            input_parser: InputParser::new(),
        })
    }

    /// Process messages from connected clients
    ///
    /// Returns a vector of (client_id, events) for the caller to handle.
    fn process_clients(&mut self) -> io::Result<Vec<(u64, Vec<Event>)>> {
        let mut disconnected = Vec::new();
        let mut control_messages: Vec<(usize, ClientControl)> = Vec::new();
        let mut input_events: Vec<(u64, Vec<Event>)> = Vec::new();

        for (idx, client) in self.clients.iter_mut().enumerate() {
            // Read from data socket (input from client)
            let mut buf = [0u8; 4096];
            match client.conn.read_data(&mut buf) {
                Ok(0) => {
                    // On Windows named pipes, Ok(0) in non-blocking mode might mean "no data"
                    // Only treat as EOF on Unix where it definitively means disconnected
                    #[cfg(not(windows))]
                    {
                        disconnected.push(idx);
                        continue;
                    }
                    // On Windows, ignore Ok(0) - we'll detect disconnection via write errors
                }
                Ok(n) => {
                    // Parse raw bytes into events
                    let events = client.input_parser.parse(&buf[..n]);
                    if !events.is_empty() {
                        tracing::trace!(
                            "Client {} sent {} bytes -> {} events",
                            client.id,
                            n,
                            events.len()
                        );
                        input_events.push((client.id, events));
                    }
                }
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                    // No data available
                }
                Err(e) => {
                    tracing::warn!("Client {} read error: {}", client.id, e);
                    disconnected.push(idx);
                    continue;
                }
            }

            // Try to read a control message (non-blocking)
            // Use try_read which internally handles non-blocking mode and Windows pipe quirks
            let mut buf = [0u8; 4096];
            match client.conn.control.try_read(&mut buf) {
                Ok(0) => {
                    // On Windows, Ok(0) in non-blocking mode might mean "no data"
                    // Only treat as EOF if we're sure the pipe is closed
                    #[cfg(not(windows))]
                    {
                        // On Unix, Ok(0) means EOF
                        disconnected.push(idx);
                    }
                    // On Windows, just ignore - we'll detect actual disconnection
                    // when write fails or we get a real error
                }
                Ok(n) => {
                    // Parse the bytes as a line (looking for newline)
                    let data = &buf[..n];
                    if let Ok(text) = std::str::from_utf8(data) {
                        for line in text.lines() {
                            let line = line.trim();
                            if !line.is_empty() {
                                if let Ok(msg) = serde_json::from_str::<ClientControl>(line) {
                                    control_messages.push((idx, msg));
                                }
                            }
                        }
                    }
                }
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                    // No message available
                }
                Err(e) => {
                    tracing::warn!("Client {} control read error: {}", client.id, e);
                }
            }
        }

        // Update activity timestamp if we had any messages
        if !control_messages.is_empty() {
            self.last_client_activity = Instant::now();
        }

        // Process control messages (now we can borrow self mutably)
        for (idx, msg) in control_messages {
            if !disconnected.contains(&idx) {
                self.handle_control_message(idx, msg)?;
            }
        }

        // Remove disconnected clients (in reverse order to preserve indices)
        for idx in disconnected.into_iter().rev() {
            let client = self.clients.remove(idx);
            tracing::info!("Client {} disconnected", client.id);
        }

        Ok(input_events)
    }

    /// Handle a control message from a client
    fn handle_control_message(&mut self, client_idx: usize, msg: ClientControl) -> io::Result<()> {
        let client = match self.clients.get_mut(client_idx) {
            Some(c) => c,
            None => return Ok(()), // Client already disconnected
        };

        match msg {
            ClientControl::Hello(_) => {
                // Shouldn't happen after initial handshake
                tracing::warn!("Client {} sent unexpected Hello", client.id);
            }
            ClientControl::Resize { cols, rows } => {
                client.term_size = TermSize::new(cols, rows);
                tracing::debug!("Client {} resized to {}x{}", client.id, cols, rows);
                // TODO: Trigger re-render
            }
            ClientControl::Ping => {
                let pong = serde_json::to_string(&ServerControl::Pong)
                    .map_err(|e| io::Error::other(e.to_string()))?;
                client.conn.write_control(&pong)?;
            }
            ClientControl::Detach => {
                tracing::info!("Client {} detached", client.id);
                // Will be removed on next process_clients() call
            }
            ClientControl::Quit => {
                let client_id = client.id;
                tracing::info!("Client {} requested quit", client_id);
                // TODO: Check for unsaved changes, prompt, etc.
                self.shutdown.store(true, Ordering::SeqCst);
            }
            ClientControl::OpenFiles { .. } => {
                // This runner doesn't have an editor, so we can't open files
                tracing::warn!(
                    "Client {} sent OpenFiles but no editor is running",
                    client.id
                );
            }
        }
        Ok(())
    }

    /// Disconnect all clients
    fn disconnect_all_clients(&mut self, reason: &str) -> io::Result<()> {
        let teardown = terminal_teardown_sequences();
        for client in &mut self.clients {
            // Send terminal teardown sequences to restore client's terminal
            drop(client.conn.write_data(&teardown));
            // Send quit control message
            let quit_msg = serde_json::to_string(&ServerControl::Quit {
                reason: reason.to_string(),
            })
            .unwrap_or_default();
            drop(client.conn.write_control(&quit_msg));
        }
        self.clients.clear();
        Ok(())
    }

    /// Send output to all connected clients
    #[allow(dead_code)]
    pub fn broadcast_output(&mut self, data: &[u8]) -> io::Result<()> {
        for client in &mut self.clients {
            if let Err(e) = client.conn.write_data(data) {
                tracing::warn!("Failed to send to client {}: {}", client.id, e);
            }
        }
        Ok(())
    }

    /// Get the number of connected clients
    pub fn client_count(&self) -> usize {
        self.clients.len()
    }

    /// Get the socket paths
    pub fn socket_paths(&self) -> &SocketPaths {
        self.listener.paths()
    }
}

#[allow(dead_code)]
impl ConnectedClient {
    /// Get the client's terminal size
    pub fn term_size(&self) -> TermSize {
        self.term_size
    }

    /// Get the client's TERM environment variable
    pub fn term(&self) -> Option<&str> {
        self.env.get("TERM").and_then(|v| v.as_deref())
    }

    /// Check if the client supports truecolor
    pub fn supports_truecolor(&self) -> bool {
        self.env
            .get("COLORTERM")
            .and_then(|v| v.as_deref())
            .map(|v| v == "truecolor" || v == "24bit")
            .unwrap_or(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_server_config_defaults() {
        let config = ServerConfig {
            working_dir: PathBuf::from("/tmp/test"),
            session_name: None,
            idle_timeout: Some(Duration::from_secs(3600)),
        };
        assert_eq!(config.working_dir, PathBuf::from("/tmp/test"));
        assert_eq!(config.idle_timeout, Some(Duration::from_secs(3600)));
    }

    #[test]
    fn test_connected_client_term_helpers() {
        use crate::server::protocol::{ClientHello, TermSize};
        use std::collections::HashMap;

        // Test TERM accessor and truecolor detection via ClientHello
        // (same logic used by ConnectedClient)
        let mut env = HashMap::new();
        env.insert("TERM".to_string(), Some("xterm-256color".to_string()));
        env.insert("COLORTERM".to_string(), Some("truecolor".to_string()));

        let mut hello = ClientHello::new(TermSize::new(80, 24));
        hello.env = env;

        assert_eq!(hello.term(), Some("xterm-256color"));
        assert!(hello.supports_truecolor());
    }

    #[test]
    fn test_term_size() {
        use crate::server::protocol::TermSize;

        let size = TermSize::new(120, 40);
        assert_eq!(size.cols, 120);
        assert_eq!(size.rows, 40);
    }
}
