//! Editor integration with the session server
//!
//! This module bridges the Editor with the server infrastructure:
//! - Creates Editor with CaptureBackend for rendering
//! - Processes input events from clients
//! - Broadcasts rendered output to all clients

use std::io;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use crossterm::event::{Event, KeyEventKind};
use ratatui::Terminal;

use crate::app::Editor;
use crate::config::Config;
use crate::config_io::DirectoryContext;
use crate::model::filesystem::{FileSystem, StdFileSystem};
use crate::server::capture_backend::{
    terminal_setup_sequences, terminal_teardown_sequences, CaptureBackend,
};
use crate::server::input_parser::InputParser;
use crate::server::ipc::{ServerConnection, ServerListener, SocketPaths};
use crate::server::protocol::{
    ClientControl, ServerControl, ServerHello, TermSize, VersionMismatch, PROTOCOL_VERSION,
};
use crate::view::color_support::ColorCapability;

/// Configuration for the editor server
#[derive(Debug, Clone)]
pub struct EditorServerConfig {
    /// Working directory for this session
    pub working_dir: PathBuf,
    /// Optional session name
    pub session_name: Option<String>,
    /// Idle timeout before auto-shutdown
    pub idle_timeout: Option<Duration>,
    /// Editor configuration
    pub editor_config: Config,
    /// Directory context for config/data paths
    pub dir_context: DirectoryContext,
    /// Whether plugins are enabled
    pub plugins_enabled: bool,
}

/// Editor server that manages editor state and client connections
pub struct EditorServer {
    config: EditorServerConfig,
    listener: ServerListener,
    clients: Vec<ConnectedClient>,
    editor: Option<Editor>,
    terminal: Option<Terminal<CaptureBackend>>,
    last_client_activity: Instant,
    shutdown: Arc<AtomicBool>,
    /// Effective terminal size (from the primary/first client)
    term_size: TermSize,
    /// Index of the client that most recently provided input (for per-client detach)
    last_input_client: Option<usize>,
}

/// A connected client with its own input parser
struct ConnectedClient {
    conn: ServerConnection,
    term_size: TermSize,
    env: std::collections::HashMap<String, Option<String>>,
    id: u64,
    input_parser: InputParser,
    /// Whether this client needs a full screen render on next frame
    needs_full_render: bool,
}

impl EditorServer {
    /// Create a new editor server
    pub fn new(config: EditorServerConfig) -> io::Result<Self> {
        let socket_paths = if let Some(ref name) = config.session_name {
            SocketPaths::for_session_name(name)?
        } else {
            SocketPaths::for_working_dir(&config.working_dir)?
        };

        let listener = ServerListener::bind(socket_paths)?;

        // Write PID file so clients can detect stale sessions
        let pid = std::process::id();
        if let Err(e) = listener.paths().write_pid(pid) {
            tracing::warn!("Failed to write PID file: {}", e);
        }

        Ok(Self {
            config,
            listener,
            clients: Vec::new(),
            editor: None,
            terminal: None,
            last_client_activity: Instant::now(),
            shutdown: Arc::new(AtomicBool::new(false)),
            term_size: TermSize::new(80, 24), // Default until first client connects
            last_input_client: None,
        })
    }

    /// Get a handle to request shutdown
    pub fn shutdown_handle(&self) -> Arc<AtomicBool> {
        self.shutdown.clone()
    }

    /// Get the socket paths
    pub fn socket_paths(&self) -> &SocketPaths {
        self.listener.paths()
    }

    /// Run the editor server main loop
    pub fn run(&mut self) -> io::Result<()> {
        tracing::info!("Editor server starting for {:?}", self.config.working_dir);

        let mut next_client_id = 1u64;
        let mut needs_render = true;
        let mut last_render = Instant::now();
        const FRAME_DURATION: Duration = Duration::from_millis(16); // 60fps

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

            // Accept new connections
            tracing::debug!("[server] main loop: calling accept()");
            match self.listener.accept() {
                Ok(Some(conn)) => {
                    // Get current cursor style from editor if it exists, otherwise from config
                    let cursor_style = self
                        .editor
                        .as_ref()
                        .map(|e| e.config().editor.cursor_style)
                        .unwrap_or(self.config.editor_config.editor.cursor_style);
                    match self.handle_new_connection(conn, next_client_id, cursor_style) {
                        Ok(client) => {
                            tracing::info!("Client {} connected", client.id);

                            // Initialize editor on first-ever client, or update size if reconnecting
                            if self.editor.is_none() {
                                // First time - initialize editor
                                self.term_size = client.term_size;
                                self.initialize_editor()?;
                            } else if self.clients.is_empty() {
                                // Reconnecting after all clients disconnected - update terminal size
                                if self.term_size != client.term_size {
                                    self.term_size = client.term_size;
                                    self.update_terminal_size()?;
                                }
                            }
                            // Note: full redraw is handled via client.needs_full_render flag

                            self.clients.push(client);
                            self.last_client_activity = Instant::now();
                            next_client_id += 1;
                            needs_render = true;
                        }
                        Err(e) => {
                            tracing::warn!("Failed to complete handshake: {}", e);
                        }
                    }
                }
                Ok(None) => {}
                Err(e) => {
                    tracing::error!("Accept error: {}", e);
                }
            }

            // Process client messages and get input events
            tracing::debug!("[server] main loop: calling process_clients");
            let (input_events, resize_occurred, input_source) = self.process_clients()?;
            if let Some(idx) = input_source {
                self.last_input_client = Some(idx);
            }
            if !input_events.is_empty() {
                tracing::debug!(
                    "[server] process_clients returned {} events",
                    input_events.len()
                );
            }

            // Check if editor should quit
            if let Some(ref editor) = self.editor {
                if editor.should_quit() {
                    tracing::info!("Editor requested quit");
                    self.shutdown.store(true, Ordering::SeqCst);
                    continue;
                }
            }

            // Check if client should detach (keep server running)
            let detach_requested = self
                .editor
                .as_ref()
                .map(|e| e.should_detach())
                .unwrap_or(false);
            if detach_requested {
                // Detach only the client that triggered it (via last input)
                if let Some(idx) = self.last_input_client.take() {
                    if idx < self.clients.len() {
                        tracing::info!("Client {} requested detach", self.clients[idx].id);
                        let client = self.clients.remove(idx);
                        let teardown = terminal_teardown_sequences();
                        let _ = client.conn.write_data(&teardown);
                        let quit_msg = serde_json::to_string(&ServerControl::Quit {
                            reason: "Detached".to_string(),
                        })
                        .unwrap_or_default();
                        let _ = client.conn.write_control(&quit_msg);
                    }
                } else {
                    // Fallback: if we can't determine which client, detach all
                    tracing::info!("Detach requested but no input source, detaching all");
                    self.disconnect_all_clients("Detached")?;
                }
                // Reset the detach flag
                if let Some(ref mut editor) = self.editor {
                    editor.clear_detach();
                }
                continue;
            }

            // Handle resize
            if resize_occurred {
                self.update_terminal_size()?;
                needs_render = true;
            }

            // Process input events
            if !input_events.is_empty() {
                self.last_client_activity = Instant::now();
                for event in input_events {
                    if self.handle_event(event)? {
                        needs_render = true;
                    }
                }
            }

            // Process async messages from editor
            if let Some(ref mut editor) = self.editor {
                if editor.process_async_messages() {
                    needs_render = true;
                }
                if editor.process_pending_file_opens() {
                    needs_render = true;
                }
                if editor.check_mouse_hover_timer() {
                    needs_render = true;
                }
            }

            // Render and broadcast if needed
            if needs_render && last_render.elapsed() >= FRAME_DURATION {
                self.render_and_broadcast()?;
                last_render = Instant::now();
                needs_render = false;
            }

            // Brief sleep to avoid busy-waiting
            std::thread::sleep(Duration::from_millis(5));
        }

        // Clean shutdown
        self.disconnect_all_clients("Server shutting down")?;

        Ok(())
    }

    /// Initialize the editor with the current terminal size
    fn initialize_editor(&mut self) -> io::Result<()> {
        let backend = CaptureBackend::new(self.term_size.cols, self.term_size.rows);
        let terminal = Terminal::new(backend)
            .map_err(|e| io::Error::other(format!("Failed to create terminal: {}", e)))?;

        let filesystem: Arc<dyn FileSystem + Send + Sync> = Arc::new(StdFileSystem);
        let color_capability = ColorCapability::TrueColor; // Assume truecolor for now

        let mut editor = Editor::with_working_dir(
            self.config.editor_config.clone(),
            self.term_size.cols,
            self.term_size.rows,
            Some(self.config.working_dir.clone()),
            self.config.dir_context.clone(),
            self.config.plugins_enabled,
            color_capability,
            filesystem,
        )
        .map_err(|e| io::Error::other(format!("Failed to create editor: {}", e)))?;

        // Enable session mode - use hardware cursor only, no REVERSED software cursor
        editor.set_session_mode(true);

        // Set session name for status bar display
        let session_display_name = self.config.session_name.clone().unwrap_or_else(|| {
            // Use the directory name as a short display name for unnamed sessions
            self.config
                .working_dir
                .file_name()
                .and_then(|n| n.to_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| "session".to_string())
        });
        editor.set_session_name(Some(session_display_name));

        self.terminal = Some(terminal);
        self.editor = Some(editor);

        tracing::info!(
            "Editor initialized with size {}x{}",
            self.term_size.cols,
            self.term_size.rows
        );

        Ok(())
    }

    /// Handle a new client connection
    fn handle_new_connection(
        &self,
        conn: ServerConnection,
        client_id: u64,
        cursor_style: crate::config::CursorStyle,
    ) -> io::Result<ConnectedClient> {
        // Read client hello
        // On Windows, don't toggle blocking mode - named pipes don't support mode switching
        // after connection. The read_control() method handles this internally.
        #[cfg(not(windows))]
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

        // Set sockets back to non-blocking
        // On Windows, don't toggle mode - named pipes don't support mode switching
        #[cfg(not(windows))]
        conn.control.set_nonblocking(true)?;

        // Send terminal setup sequences
        let setup = terminal_setup_sequences();
        conn.write_data(&setup)?;

        // Send cursor style (from editor if running, otherwise from config)
        conn.write_data(cursor_style.to_escape_sequence())?;

        tracing::debug!(
            "Client {} connected: {}x{}, TERM={:?}",
            client_id,
            hello.term_size.cols,
            hello.term_size.rows,
            hello.term()
        );

        Ok(ConnectedClient {
            conn,
            term_size: hello.term_size,
            env: hello.env,
            id: client_id,
            input_parser: InputParser::new(),
            needs_full_render: true,
        })
    }

    /// Process messages from connected clients
    /// Returns (input_events, resize_occurred, index of client that provided input)
    fn process_clients(&mut self) -> io::Result<(Vec<Event>, bool, Option<usize>)> {
        let mut disconnected = Vec::new();
        let mut input_source_client: Option<usize> = None;
        let mut input_events = Vec::new();
        let mut resize_occurred = false;
        let mut control_messages: Vec<(usize, ClientControl)> = Vec::new();

        for (idx, client) in self.clients.iter_mut().enumerate() {
            // Read from data socket
            let mut buf = [0u8; 4096];
            let mut data_eof = false;
            tracing::debug!("[server] reading from client {} data socket", client.id);
            match client.conn.read_data(&mut buf) {
                Ok(0) => {
                    tracing::debug!("[server] Client {} data stream closed (EOF)", client.id);
                    disconnected.push(idx);
                    data_eof = true;
                    // Don't continue - still need to check control socket for pending messages
                }
                Ok(n) => {
                    tracing::debug!(
                        "[server] Client {} read {} bytes from data socket",
                        client.id,
                        n
                    );
                    let events = client.input_parser.parse(&buf[..n]);
                    tracing::debug!(
                        "[server] Client {} parsed {} events",
                        client.id,
                        events.len()
                    );
                    if !events.is_empty() {
                        input_source_client = Some(idx);
                    }
                    input_events.extend(events);
                }
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => {}
                Err(e) => {
                    tracing::warn!("[server] Client {} data read error: {}", client.id, e);
                    disconnected.push(idx);
                    data_eof = true;
                    // Don't continue - still need to check control socket for pending messages
                }
            }
            let _ = data_eof; // Suppress unused warning

            // Check control socket
            // On Windows, don't toggle nonblocking mode - it fails on named pipes
            #[cfg(not(windows))]
            let _ = client.conn.control.set_nonblocking(true);

            // On Windows, use try_read pattern instead of blocking read_line
            #[cfg(windows)]
            {
                let mut buf = [0u8; 1024];
                match client.conn.control.try_read(&mut buf) {
                    Ok(0) => {
                        tracing::debug!("Client {} control stream closed (EOF)", client.id);
                        disconnected.push(idx);
                    }
                    Ok(n) => {
                        // Try to parse as control message
                        if let Ok(s) = std::str::from_utf8(&buf[..n]) {
                            for line in s.lines() {
                                if !line.trim().is_empty() {
                                    if let Ok(msg) = serde_json::from_str::<ClientControl>(line) {
                                        control_messages.push((idx, msg));
                                    }
                                }
                            }
                        }
                    }
                    Err(e) if e.kind() == io::ErrorKind::WouldBlock => {}
                    Err(e) => {
                        tracing::warn!("Client {} control read error: {}", client.id, e);
                    }
                }
            }

            #[cfg(not(windows))]
            {
                let mut reader = std::io::BufReader::new(&client.conn.control);
                let mut line = String::new();
                match std::io::BufRead::read_line(&mut reader, &mut line) {
                    Ok(0) => {
                        tracing::debug!("Client {} control stream closed (EOF)", client.id);
                        disconnected.push(idx);
                    }
                    Ok(_) if !line.trim().is_empty() => {
                        if let Ok(msg) = serde_json::from_str::<ClientControl>(&line) {
                            control_messages.push((idx, msg));
                        }
                    }
                    Ok(_) => {}
                    Err(e) if e.kind() == io::ErrorKind::WouldBlock => {}
                    Err(e) => {
                        tracing::warn!("Client {} control read error: {}", client.id, e);
                    }
                }
            }
        }

        // Process control messages
        eprintln!(
            "[server] Processing {} control messages",
            control_messages.len()
        );
        for (idx, msg) in control_messages {
            eprintln!("[server] Control message from client {}: {:?}", idx, msg);
            // Always process Quit, even from disconnected clients
            if let ClientControl::Quit = msg {
                tracing::info!("Client requested quit, shutting down");
                self.shutdown.store(true, Ordering::SeqCst);
                continue;
            }

            // Always process OpenFiles - it's a one-shot command from clients that disconnect immediately
            if let ClientControl::OpenFiles { .. } = msg {
                // Fall through to process it
            } else if disconnected.contains(&idx) {
                // Skip other messages from disconnected clients
                continue;
            }

            match msg {
                ClientControl::Hello(_) => {
                    tracing::warn!("Unexpected Hello from client");
                }
                ClientControl::Resize { cols, rows } => {
                    if let Some(client) = self.clients.get_mut(idx) {
                        client.term_size = TermSize::new(cols, rows);
                        // Update server size to match first client
                        if idx == 0 {
                            self.term_size = TermSize::new(cols, rows);
                            resize_occurred = true;
                        }
                    }
                }
                ClientControl::Ping => {
                    if let Some(client) = self.clients.get_mut(idx) {
                        let pong = serde_json::to_string(&ServerControl::Pong).unwrap_or_default();
                        let _ = client.conn.write_control(&pong);
                    }
                }
                ClientControl::Detach => {
                    tracing::info!("Client {} detached", idx);
                    disconnected.push(idx);
                }
                ClientControl::OpenFiles { files } => {
                    if let Some(ref mut editor) = self.editor {
                        for file_req in &files {
                            let path = std::path::PathBuf::from(&file_req.path);
                            tracing::debug!(
                                "Queuing file open: {:?} line={:?} col={:?}",
                                path,
                                file_req.line,
                                file_req.column
                            );
                            editor.queue_file_open(path, file_req.line, file_req.column);
                        }
                        resize_occurred = true; // Force re-render
                    }
                }
                ClientControl::Quit => unreachable!(), // Handled above
            }
        }

        // Remove disconnected clients
        for idx in disconnected.into_iter().rev() {
            let client = self.clients.remove(idx);
            // Send teardown sequences
            let teardown = terminal_teardown_sequences();
            let _ = client.conn.write_data(&teardown);
            tracing::info!("Client {} disconnected", client.id);
            // Invalidate input source if that client disconnected
            if input_source_client == Some(idx) {
                input_source_client = None;
            }
        }

        Ok((input_events, resize_occurred, input_source_client))
    }

    /// Update terminal size after resize
    fn update_terminal_size(&mut self) -> io::Result<()> {
        if let Some(ref mut terminal) = self.terminal {
            let backend = terminal.backend_mut();
            backend.resize(self.term_size.cols, self.term_size.rows);
        }

        if let Some(ref mut editor) = self.editor {
            editor.resize(self.term_size.cols, self.term_size.rows);
        }

        Ok(())
    }

    /// Handle an input event
    fn handle_event(&mut self, event: Event) -> io::Result<bool> {
        let Some(ref mut editor) = self.editor else {
            return Ok(false);
        };

        match event {
            Event::Key(key_event) => {
                if key_event.kind == KeyEventKind::Press {
                    editor
                        .handle_key(key_event.code, key_event.modifiers)
                        .map_err(|e| io::Error::other(e.to_string()))?;
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Event::Mouse(mouse_event) => editor
                .handle_mouse(mouse_event)
                .map_err(|e| io::Error::other(e.to_string())),
            Event::Resize(w, h) => {
                editor.resize(w, h);
                Ok(true)
            }
            Event::Paste(text) => {
                editor.paste_text(text);
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    /// Render the editor and broadcast output to all clients
    fn render_and_broadcast(&mut self) -> io::Result<()> {
        let Some(ref mut editor) = self.editor else {
            return Ok(());
        };

        let Some(ref mut terminal) = self.terminal else {
            return Ok(());
        };

        // Check if any client needs a full render (e.g., newly connected)
        let any_needs_full = self.clients.iter().any(|c| c.needs_full_render);
        if any_needs_full {
            tracing::info!(
                "Full render requested for {} client(s)",
                self.clients.iter().filter(|c| c.needs_full_render).count()
            );
            // Force full redraw by invalidating terminal state
            terminal.backend_mut().reset_style_state();
            let _ = terminal.clear();
        }

        // Take any pending escape sequences (e.g., cursor style changes)
        let pending_sequences = editor.take_pending_escape_sequences();

        // Render to capture backend
        terminal
            .draw(|frame| editor.render(frame))
            .map_err(|e| io::Error::other(e.to_string()))?;

        // Get the captured output
        let output = terminal.backend_mut().take_buffer();

        if output.is_empty() && pending_sequences.is_empty() {
            return Ok(());
        }

        // Broadcast to all clients (pending sequences first, then rendered output)
        for client in &mut self.clients {
            if !pending_sequences.is_empty() {
                if let Err(e) = client.conn.write_data(&pending_sequences) {
                    tracing::warn!(
                        "Failed to send pending sequences to client {}: {}",
                        client.id,
                        e
                    );
                }
            }
            if !output.is_empty() {
                if let Err(e) = client.conn.write_data(&output) {
                    tracing::warn!("Failed to send to client {}: {}", client.id, e);
                }
            }
            // Clear full render flag after sending
            client.needs_full_render = false;
        }

        Ok(())
    }

    /// Disconnect all clients
    fn disconnect_all_clients(&mut self, reason: &str) -> io::Result<()> {
        let teardown = terminal_teardown_sequences();
        for client in &mut self.clients {
            let _ = client.conn.write_data(&teardown);
            let quit_msg = serde_json::to_string(&ServerControl::Quit {
                reason: reason.to_string(),
            })
            .unwrap_or_default();
            let _ = client.conn.write_control(&quit_msg);
        }
        self.clients.clear();
        Ok(())
    }
}

impl ConnectedClient {
    /// Get the client's TERM environment variable
    #[allow(dead_code)]
    pub fn term(&self) -> Option<&str> {
        self.env.get("TERM").and_then(|v| v.as_deref())
    }

    /// Check if the client supports truecolor
    #[allow(dead_code)]
    pub fn supports_truecolor(&self) -> bool {
        self.env
            .get("COLORTERM")
            .and_then(|v| v.as_deref())
            .map(|v| v == "truecolor" || v == "24bit")
            .unwrap_or(false)
    }
}
