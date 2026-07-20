//! Editor integration with the daemon server
//!
//! This module bridges the Editor with the server infrastructure:
//! - Creates Editor with CaptureBackend for rendering
//! - Processes input events from clients
//! - Broadcasts rendered output to all clients

use std::io;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc;
use std::sync::Arc;
use std::time::{Duration, Instant};

use crossterm::event::{Event, KeyEventKind};
use ratatui::Terminal;

use crate::app::Editor;
use crate::config::Config;
use crate::config_io::DirectoryContext;
// Filesystem is now owned by `self.current_authority`; the server no
// longer constructs a `StdFileSystem` directly.
use crate::server::capture_backend::{
    terminal_setup_sequences, terminal_teardown_sequences, CaptureBackend,
};
use crate::server::command_access;
use crate::server::input_parser::InputParser;
use crate::server::ipc::{ServerConnection, ServerListener, SocketPaths, StreamWrapper};
use crate::server::protocol::{
    ClientControl, ServerControl, ServerHello, TermSize, VersionMismatch, PROTOCOL_VERSION,
};
use crate::view::color_support::ColorCapability;

/// Configuration for the editor server
pub struct EditorServerConfig {
    /// Working directory for this daemon
    pub working_dir: PathBuf,
    /// Optional daemon name
    pub session_name: Option<String>,
    /// Idle timeout before auto-shutdown
    pub idle_timeout: Option<Duration>,
    /// Editor configuration
    pub editor_config: Config,
    /// Directory context for config/data paths
    pub dir_context: DirectoryContext,
    /// Whether plugins are enabled
    pub plugins_enabled: bool,
    /// Whether to auto-load ~/.config/fresh/init.ts (requires `plugins_enabled`).
    pub init_enabled: bool,
    /// Authority to install at boot.  `None` means `Authority::local()`,
    /// which is the standard daemon-mode default (principle 6 of
    /// `AUTHORITY_DESIGN.md`).  The CLI `ssh://` / `user@host:path`
    /// forms construct an `Authority::ssh(...)` and pass it here so
    /// the daemon boots already attached to the remote host.  Plugins
    /// can still replace this post-boot via `setAuthority`.
    pub startup_authority: Option<crate::services::authority::Authority>,
    /// Workspace Trust handle, created by the caller (`main.rs`) before the
    /// startup authority so the same `Arc` backs both the authority's spawners
    /// and the server. Mandatory: every spawner holds it, so there's no
    /// ungated path. Tests pass `Arc::new(WorkspaceTrust::permissive())`.
    pub workspace_trust: Arc<crate::services::workspace_trust::WorkspaceTrust>,
    /// Live environment provider, created by the caller (`main.rs`) before the
    /// startup authority so the same `Arc` backs the authority's spawners and
    /// the server. Tests pass `Arc::new(EnvProvider::inactive())`.
    pub env_provider: Arc<crate::services::env_provider::EnvProvider>,
    /// Opaque handle kept alive for the server's lifetime alongside
    /// `startup_authority`.  SSH authorities back this with the Tokio
    /// runtime, the `SshConnection`, and the reconnect task — dropping
    /// any of those tears down the remote backend — so the caller
    /// bundles them here and the server just holds on until shutdown.
    /// Local authorities leave this `None`.
    pub session_keepalive: Option<Box<dyn std::any::Any + Send>>,
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
    /// Next wait ID for --wait tracking
    next_wait_id: u64,
    /// Maps wait_id → client_id for clients waiting on file events
    waiting_clients: std::collections::HashMap<u64, u64>,
    /// Current authority. Carried across editor rebuilds so plugin-
    /// installed authorities (e.g. a devcontainer attach) survive the
    /// restart-based transition: the old editor is dropped, a new one
    /// is built with this authority in effect, and clients stay
    /// connected the whole time. Starts as
    /// `config.startup_authority.unwrap_or_else(Authority::local)`.
    current_authority: crate::services::authority::Authority,
    /// Workspace Trust state, gating process execution. Held here (not on
    /// the `Editor`) so the chosen level survives editor rebuilds. Shared
    /// by `Arc` into the authority's guarding spawners on every build via
    /// `Authority::with_trust`; its root is updated when the working
    /// directory changes.
    workspace_trust: Arc<crate::services::workspace_trust::WorkspaceTrust>,
    /// Live env provider, shared into the authority's spawners across rebuilds.
    env_provider: Arc<crate::services::env_provider::EnvProvider>,
    /// Keepalive bundle paired with the startup authority — held for
    /// the server's lifetime so SSH runtimes, reconnect tasks, and
    /// similar resources outlive the editor rebuilds that happen on
    /// authority transitions.  Never inspected; dropped only when the
    /// server is dropped.
    #[allow(dead_code)]
    session_keepalive: Option<Box<dyn std::any::Any + Send>>,
}

/// Buffered writer for sending data to a client without blocking the server loop.
///
/// Spawns a background thread that receives data via a bounded channel and
/// writes it to the client's data pipe. If the channel fills up (client is
/// too slow to read), frames are dropped. If the pipe breaks, the `pipe_broken`
/// flag is set so the main loop can disconnect the client.
struct ClientDataWriter {
    sender: mpsc::SyncSender<Vec<u8>>,
    pipe_broken: Arc<AtomicBool>,
}

impl ClientDataWriter {
    /// Create a new writer that spawns a background thread to write to the data stream.
    fn new(data: StreamWrapper, client_id: u64) -> Self {
        // 16 frames of buffer (~270ms at 60fps before dropping frames)
        let (tx, rx) = mpsc::sync_channel::<Vec<u8>>(16);
        let pipe_broken = Arc::new(AtomicBool::new(false));
        let pipe_broken_clone = pipe_broken.clone();

        std::thread::Builder::new()
            .name(format!("client-{}-writer", client_id))
            .spawn(move || {
                while let Ok(buf) = rx.recv() {
                    if let Err(e) = data.write_all(&buf) {
                        tracing::debug!("Client {} writer pipe error: {}", client_id, e);
                        pipe_broken_clone.store(true, Ordering::Relaxed);
                        break;
                    }
                    if let Err(e) = data.flush() {
                        tracing::debug!("Client {} writer flush error: {}", client_id, e);
                        pipe_broken_clone.store(true, Ordering::Relaxed);
                        break;
                    }
                }
                tracing::debug!("Client {} writer thread exiting", client_id);
            })
            .expect("Failed to spawn client writer thread");

        Self {
            sender: tx,
            pipe_broken,
        }
    }

    /// Try to send data without blocking. Returns false if the channel is full
    /// (client too slow) or the writer thread has exited.
    fn try_write(&self, data: &[u8]) -> bool {
        self.sender.try_send(data.to_vec()).is_ok()
    }

    /// Check if the writer thread detected a broken pipe.
    fn is_broken(&self) -> bool {
        self.pipe_broken.load(Ordering::Relaxed)
    }
}

/// A connected client with its own input parser
struct ConnectedClient {
    conn: ServerConnection,
    /// Background writer for non-blocking data output
    data_writer: ClientDataWriter,
    term_size: TermSize,
    env: std::collections::HashMap<String, Option<String>>,
    id: u64,
    input_parser: InputParser,
    /// Whether this client needs a full screen render on next frame
    needs_full_render: bool,
    /// If set, this client is waiting for a --wait completion signal
    wait_id: Option<u64>,
    /// Per-workspace capability token presented in this client's `Hello`
    /// (from `$FRESH_CMD_TOKEN`). Authorizes `ListCommands` / `RunCommand`
    /// against the token's allowlist; `None` for clients that carry no token.
    cmd_token: Option<String>,
}

impl EditorServer {
    /// Create a new editor server
    pub fn new(mut config: EditorServerConfig) -> io::Result<Self> {
        let socket_paths = if let Some(ref name) = config.session_name {
            SocketPaths::for_session_name(name)?
        } else {
            SocketPaths::for_working_dir(&config.working_dir)?
        };

        let listener = ServerListener::bind(socket_paths)?;

        // Write PID file so clients can detect stale daemons
        let pid = std::process::id();
        if let Err(e) = listener.paths().write_pid(pid) {
            tracing::warn!("Failed to write PID file: {}", e);
        }

        // Workspace Trust is born in `main.rs` (already rooted at the working
        // dir, store attached, persisted level loaded) and threaded in via the
        // config — the same `Arc` the startup authority's spawners hold. The
        // server just keeps a handle so rebuilds can re-anchor it on a
        // working-dir change and the prompt can read it.
        let workspace_trust = Arc::clone(&config.workspace_trust);
        let env_provider = Arc::clone(&config.env_provider);

        // Move the startup authority + its keepalive off the config —
        // they are consumed once and belong to the server from here on.
        // A missing startup authority defaults to a local one carrying the
        // same trust + env handles.
        let current_authority = config.startup_authority.take().unwrap_or_else(|| {
            crate::services::authority::Authority::local(
                Arc::clone(&workspace_trust),
                Arc::clone(&env_provider),
            )
        });
        let session_keepalive = config.session_keepalive.take();

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
            next_wait_id: 1,
            waiting_clients: std::collections::HashMap::new(),
            current_authority,
            workspace_trust,
            env_provider,
            session_keepalive,
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

    /// Access the editor instance (available after initialize_editor).
    pub fn editor(&self) -> Option<&Editor> {
        self.editor.as_ref()
    }

    /// Mutable access to the editor instance (available after initialize_editor).
    pub fn editor_mut(&mut self) -> Option<&mut Editor> {
        self.editor.as_mut()
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

            // Check if editor should quit. `should_quit` is set both
            // by a genuine user quit and by `request_restart`
            // (triggered by `change_working_dir` and by
            // `install_authority`).  Distinguish the two by peeking at
            // the editor's pending-restart fields: if either carries a
            // value, rebuild the editor in place and keep clients
            // attached; otherwise this is a real shutdown.
            if let Some(ref mut editor) = self.editor {
                if editor.should_quit() {
                    let pending_authority = editor.take_pending_authority();
                    let pending_keepalive = editor.take_pending_keepalive();
                    let restart_dir = editor.take_restart_dir();
                    if pending_authority.is_some() || restart_dir.is_some() {
                        tracing::info!(
                            "Session rebuild requested (authority={}, dir={})",
                            pending_authority.is_some(),
                            restart_dir.is_some()
                        );
                        if let Err(e) =
                            self.rebuild_editor(restart_dir, pending_authority, pending_keepalive)
                        {
                            tracing::error!("Session rebuild failed, shutting down: {}", e);
                            self.shutdown.store(true, Ordering::SeqCst);
                            continue;
                        }
                        needs_render = true;
                        continue;
                    }
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
                        // Best-effort: client may already be disconnected
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = client.data_writer.try_write(&teardown);
                        let quit_msg = serde_json::to_string(&ServerControl::Quit {
                            reason: "Detached".to_string(),
                        })
                        .unwrap_or_default();
                        // Best-effort: client may already be disconnected
                        #[allow(clippy::let_underscore_must_use)]
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

            // Check if the client should suspend itself (SIGTSTP). The server
            // keeps running — only the client drops back to the shell. On
            // resume, the client sends a Resize to nudge a full redraw; we
            // pre-mark `needs_full_render` so the next rendered frame after
            // resume re-emits setup sequences and a complete paint.
            let suspend_requested = self
                .editor
                .as_mut()
                .map(|e| e.take_suspend_request())
                .unwrap_or(false);
            if suspend_requested {
                if let Some(idx) = self.last_input_client {
                    if idx < self.clients.len() {
                        let client_id = self.clients[idx].id;
                        tracing::info!("Client {} requested suspend", client_id);
                        let suspend_msg = serde_json::to_string(&ServerControl::SuspendClient)
                            .unwrap_or_default();
                        // Best-effort: client may already be disconnected
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = self.clients[idx].conn.write_control(&suspend_msg);
                        // Mark so the next render re-emits setup sequences and a
                        // full paint once the client resumes and reconnects its
                        // terminal.
                        self.clients[idx].needs_full_render = true;
                    }
                } else {
                    tracing::warn!("Suspend requested but no input source; ignoring");
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

                // Process completed --wait operations
                for wait_id in editor.take_completed_waits() {
                    if let Some(client_id) = self.waiting_clients.remove(&wait_id) {
                        // Find the client and send WaitComplete
                        if let Some(client) = self.clients.iter_mut().find(|c| c.id == client_id) {
                            let msg = serde_json::to_string(&ServerControl::WaitComplete)
                                .unwrap_or_default();
                            #[allow(clippy::let_underscore_must_use)]
                            let _ = client.conn.write_control(&msg);
                            client.wait_id = None;
                        }
                    }
                }

                // Send pending clipboard data to clients via control message
                if let Some(cb) = editor.take_pending_clipboard() {
                    let msg = serde_json::to_string(&ServerControl::SetClipboard {
                        text: cb.text,
                        use_osc52: cb.use_osc52,
                        use_system_clipboard: cb.use_system_clipboard,
                    })
                    .unwrap_or_default();
                    for client in &mut self.clients {
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = client.conn.write_control(&msg);
                    }
                }

                if editor.check_mouse_hover_timer() {
                    needs_render = true;
                }

                // Active animations force a render every FRAME_DURATION so
                // the slide settles on its own. Without this the loop only
                // ticks when an external event (input, resize, async
                // message) flips `needs_render`, so under tmux a buffer
                // switch paints its first frame and then freezes mid-slide
                // until the user nudges the terminal. Mirrors the direct
                // (non-server) loop in `main.rs`.
                if editor.active_window().animations.is_active() {
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

        // Perform the same shutdown sequence as the normal (non-daemon) exit path
        // in run_event_loop_common: auto-save, end recovery session, save workspace.
        if let Some(ref mut editor) = self.editor {
            // Auto-save file-backed buffers to disk before exiting
            if editor.config().editor.auto_save_enabled {
                match editor.save_all_on_exit() {
                    Ok(count) if count > 0 => {
                        tracing::info!("Auto-saved {} buffer(s) on exit", count);
                    }
                    Ok(_) => {}
                    Err(e) => {
                        tracing::warn!("Failed to auto-save on exit: {}", e);
                    }
                }
            }

            // End recovery session first (flushes dirty buffers + assigns recovery IDs),
            // then save workspace (captures those IDs for next workspace restore).
            if let Err(e) = editor.end_recovery_session() {
                tracing::warn!("Failed to end recovery session: {}", e);
            }
            if let Err(e) = editor.save_all_windows_workspaces() {
                tracing::warn!("Failed to save workspaces: {}", e);
            } else {
                tracing::debug!("Workspaces saved successfully");
            }
        }

        // Clean shutdown
        self.disconnect_all_clients("Server shutting down")?;

        Ok(())
    }

    /// Build a fresh `Editor` instance using the current configuration
    /// and stored authority.  Shared between first-boot initialization
    /// and post-restart rebuild.
    fn build_editor_instance(&mut self) -> io::Result<(Editor, Terminal<CaptureBackend>)> {
        let backend = CaptureBackend::new(self.term_size.cols, self.term_size.rows);
        let terminal = Terminal::new(backend)
            .map_err(|e| io::Error::other(format!("Failed to create terminal: {}", e)))?;

        // The editor is constructed with the real authority it runs under, so
        // plugins and init.ts load against the correct backend from the first
        // tick — no post-construction swap. The authority already carries the
        // shared trust handle (every spawner holds it).
        let color_capability = ColorCapability::TrueColor; // Assume truecolor for now

        let mut editor = Editor::with_working_dir_opts(
            self.config.editor_config.clone(),
            self.term_size.cols,
            self.term_size.rows,
            Some(self.config.working_dir.clone()),
            self.config.dir_context.clone(),
            self.config.plugins_enabled,
            color_capability,
            // `Authority` is single-owner (non-`Clone`): move the current one
            // into the rebuilt editor, leaving a local placeholder behind. A
            // real authority transition overwrites it just below; otherwise
            // each window's own backend spec drives restore/reconnect, so the
            // placeholder only governs the active window until it reconnects.
            std::mem::replace(
                &mut self.current_authority,
                crate::services::authority::Authority::local(
                    std::sync::Arc::clone(&self.workspace_trust),
                    std::sync::Arc::clone(&self.env_provider),
                ),
            ),
            false,
        )
        .map_err(|e| io::Error::other(format!("Failed to create editor: {}", e)))?;

        // Auto-load init.ts via the same pipeline as the non-server entry point.
        editor.load_init_script(self.config.init_enabled);

        // Enable session mode - use hardware cursor only, no REVERSED software cursor
        editor.set_session_mode(true);

        // Set session name for status bar display
        let session_display_name = self.config.session_name.clone().unwrap_or_else(|| {
            // Use the directory name as a short display name for unnamed daemons
            self.config
                .working_dir
                .file_name()
                .and_then(|n| n.to_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| "session".to_string())
        });
        editor.set_session_name(Some(session_display_name));

        Ok((editor, terminal))
    }

    /// Initialize the editor on first client connection.
    ///
    /// Performs the full first-boot sequence: build editor, restore
    /// workspace, recover buffers from hot exit, start recovery
    /// session.  Subsequent rebuilds (on authority/working-dir change)
    /// go through [`rebuild_editor`].
    pub fn initialize_editor(&mut self) -> io::Result<()> {
        let (mut editor, terminal) = self.build_editor_instance()?;

        // Restore workspace and recovery data (mirrors the standalone startup
        // path in handle_first_run_setup in main.rs).
        match editor.try_restore_workspace() {
            Ok(true) => {
                tracing::info!("Session workspace restored successfully");
            }
            Ok(false) => {
                tracing::debug!("No previous session workspace found");
            }
            Err(e) => {
                tracing::warn!("Failed to restore session workspace: {}", e);
            }
        }

        // Recover buffers from hot exit recovery files
        if editor.has_recovery_files().unwrap_or(false) {
            tracing::info!("Recovery files found for session, recovering...");
            match editor.recover_all_buffers() {
                Ok(count) if count > 0 => {
                    tracing::info!("Recovered {} buffer(s) for session", count);
                }
                Ok(_) => {
                    tracing::info!("No buffers to recover for session");
                }
                Err(e) => {
                    tracing::warn!("Failed to recover session buffers: {}", e);
                }
            }
        }

        // Start the recovery session (periodic saves of dirty buffers)
        if let Err(e) = editor.start_recovery_session() {
            tracing::warn!("Failed to start recovery session: {}", e);
        }

        self.terminal = Some(terminal);
        self.editor = Some(editor);

        // Initial open-time gate: secondary is Quit (nothing to fall back to).
        self.maybe_prompt_workspace_trust(false);

        tracing::info!(
            "Editor initialized with size {}x{}",
            self.term_size.cols,
            self.term_size.rows
        );

        Ok(())
    }

    /// Surface the workspace-trust prompt after the editor is built. Delegates
    /// to `Editor::maybe_prompt_workspace_trust` (single source of truth shared
    /// with the in-process run path). `cancellable` selects the secondary
    /// button: `false` (Quit) for the initial open-time gate, `true` (Cancel)
    /// for a re-evaluation after a working-directory change on a running
    /// editor.
    fn maybe_prompt_workspace_trust(&mut self, cancellable: bool) {
        if let Some(editor) = self.editor.as_mut() {
            editor.maybe_prompt_workspace_trust(cancellable);
        }
    }

    /// Rebuild the editor in place after an authority transition or a
    /// working-directory change.
    ///
    /// Mirrors the restart loop in `main.rs`: save the workspace so
    /// open buffers come back, drop the old editor (which cascades
    /// into shutting down terminals, LSP servers, and plugin state),
    /// swap in any new authority / working-dir, build a fresh editor,
    /// and restore the workspace under the new backend.  The TCP
    /// clients stay connected throughout; each is flagged for a full
    /// redraw on the next frame so they see the new editor from a
    /// clean state rather than a mid-transition frame.
    pub(crate) fn rebuild_editor(
        &mut self,
        new_working_dir: Option<PathBuf>,
        new_authority: Option<crate::services::authority::Authority>,
        new_keepalive: Option<Box<dyn std::any::Any + Send>>,
    ) -> io::Result<()> {
        // Flush buffer saves + workspace before dropping the old editor,
        // mirroring the standalone exit path.  On failure we log and
        // continue — rebuild should still succeed.
        if let Some(ref mut editor) = self.editor {
            if editor.config().editor.auto_save_enabled {
                if let Err(e) = editor.save_all_on_exit() {
                    tracing::warn!("Rebuild: failed to auto-save on exit: {}", e);
                }
            }
            if let Err(e) = editor.end_recovery_session() {
                tracing::warn!("Rebuild: failed to end recovery session: {}", e);
            }
            if let Err(e) = editor.save_all_windows_workspaces() {
                tracing::warn!("Rebuild: failed to save workspaces: {}", e);
            }
        }

        // Non-transition rebuild (working-dir change, config reload): carry the
        // active workspace's own backend forward by moving it out of the old
        // editor, so a remote workspace isn't dropped to the local placeholder
        // `build_editor_instance` leaves behind. A real authority transition
        // (`new_authority`) overwrites `current_authority` just below.
        if new_authority.is_none() {
            if let Some(ref mut editor) = self.editor {
                self.current_authority = editor.take_active_authority();
            }
        }

        // Drop old editor + terminal.  Drop impls shut down PTYs, LSP
        // servers, and plugin threads.
        self.editor = None;
        self.terminal = None;

        // Apply the pending changes before building the next editor.
        if let Some(dir) = new_working_dir {
            tracing::info!("Rebuild: switching working dir to {}", dir.display());
            self.config.working_dir = dir;
            // Re-anchor trust to the new workspace: move the containment
            // root and repoint persistence at the new project's trust file,
            // adopting that project's stored decision.
            self.workspace_trust
                .set_root(Some(self.config.working_dir.clone()));
            self.workspace_trust.set_store(Some(
                crate::services::workspace_trust::TrustStore::for_project_dir(
                    &self
                        .config
                        .dir_context
                        .project_state_dir(&self.config.working_dir),
                ),
            ));
            // New project ⇒ the old env recipe no longer applies; deactivate
            // and let the env-manager plugin re-detect for the new workspace.
            self.env_provider.clear();
        }
        if let Some(auth) = new_authority {
            tracing::info!(
                "Rebuild: installing authority with label {:?}",
                auth.display_label
            );
            self.current_authority = auth;
        }
        // Adopt the keepalive that rode with a connection-backed authority
        // (remote agent / K8s) so its carrier + reconnect/heartbeat tasks
        // survive the rebuild; the previous keepalive drops, tearing down
        // any prior remote backend. A local/docker transition carries
        // none, leaving the current workspace untouched.
        if let Some(keepalive) = new_keepalive {
            self.session_keepalive = Some(keepalive);
        }

        let (mut editor, terminal) = self.build_editor_instance()?;

        // Bring buffers back under the new backend.  `try_restore_workspace`
        // reads the workspace file we wrote above and re-opens the
        // same splits/buffers.
        match editor.try_restore_workspace() {
            Ok(true) => tracing::info!("Rebuild: workspace restored"),
            Ok(false) => tracing::debug!("Rebuild: no workspace to restore"),
            Err(e) => tracing::warn!("Rebuild: failed to restore workspace: {}", e),
        }

        if let Err(e) = editor.start_recovery_session() {
            tracing::warn!("Rebuild: failed to start recovery session: {}", e);
        }

        self.terminal = Some(terminal);
        self.editor = Some(editor);

        // A working-dir change lands us in a possibly-undecided project;
        // re-evaluate the trust prompt. (A rebuild triggered by a trust
        // decision just recorded one, so this is a no-op there.) This is an
        // activation on an already-running editor, so the secondary is Cancel
        // (`cancellable = true`) — dismissing must not quit the server.
        self.maybe_prompt_workspace_trust(true);

        // Force every attached client to repaint from scratch — the
        // previous frame described the old editor's screen.
        for client in &mut self.clients {
            client.needs_full_render = true;
        }

        tracing::info!(
            "Rebuild: complete, {} clients kept attached",
            self.clients.len()
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
        let mouse_hover_enabled = self.config.editor_config.editor.mouse_hover_enabled;
        let setup = terminal_setup_sequences(mouse_hover_enabled);
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

        // Create background writer for non-blocking render output
        let data_writer = ClientDataWriter::new(conn.data.clone(), client_id);

        Ok(ConnectedClient {
            conn,
            data_writer,
            term_size: hello.term_size,
            env: hello.env,
            id: client_id,
            input_parser: InputParser::new(),
            needs_full_render: true,
            wait_id: None,
            cmd_token: hello.cmd_token,
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
                    // Don't disconnect waiting clients on data EOF - they're not sending data
                    if client.wait_id.is_none() {
                        disconnected.push(idx);
                    }
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
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                    // No data available
                }
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
            // Best-effort: nonblocking mode for control socket polling
            #[cfg(not(windows))]
            #[allow(clippy::let_underscore_must_use)]
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
        if !control_messages.is_empty() {
            tracing::debug!(
                "[server] Processing {} control messages",
                control_messages.len()
            );
        }
        for (idx, msg) in control_messages {
            tracing::debug!("[server] Control message from client {}: {:?}", idx, msg);
            // Always process Quit, even from disconnected clients
            if let ClientControl::Quit = msg {
                tracing::info!("Client requested quit, shutting down");
                self.shutdown.store(true, Ordering::SeqCst);
                continue;
            }

            // Always process OpenFiles / OpenWindow - they're one-shot
            // commands from clients that disconnect immediately
            if let ClientControl::OpenFiles { .. } | ClientControl::OpenWindow { .. } = msg {
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
                        // Best-effort pong reply
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = client.conn.write_control(&pong);
                    }
                }
                ClientControl::Detach => {
                    tracing::info!("Client {} detached", idx);
                    disconnected.push(idx);
                }
                ClientControl::OpenFiles { files, wait } => {
                    if let Some(ref mut editor) = self.editor {
                        // Assign a wait_id if --wait was requested
                        let wait_id = if wait {
                            let id = self.next_wait_id;
                            self.next_wait_id += 1;
                            Some(id)
                        } else {
                            None
                        };

                        let file_count = files.len();
                        for (i, file_req) in files.iter().enumerate() {
                            let path = std::path::PathBuf::from(&file_req.path);
                            tracing::debug!(
                                "Queuing file open: {:?} line={:?} col={:?} end_line={:?} end_col={:?} message={:?}",
                                path,
                                file_req.line,
                                file_req.column,
                                file_req.end_line,
                                file_req.end_column,
                                file_req.message,
                            );
                            // Only the last file gets the wait_id (it's the one that will be active)
                            let file_wait_id = if i == file_count - 1 { wait_id } else { None };
                            editor.queue_file_open(
                                path,
                                file_req.line,
                                file_req.column,
                                file_req.end_line,
                                file_req.end_column,
                                file_req.message.clone(),
                                file_wait_id,
                            );
                        }

                        // Track the waiting client
                        if let Some(wait_id) = wait_id {
                            if let Some(client) = self.clients.get_mut(idx) {
                                self.waiting_clients.insert(wait_id, client.id);
                                client.wait_id = Some(wait_id);
                            }
                        }

                        resize_occurred = true; // Force re-render
                    }
                }
                ClientControl::OpenWindow { path } => {
                    if let Some(ref mut editor) = self.editor {
                        let path = std::path::PathBuf::from(path);
                        if path.is_absolute() {
                            let label = path
                                .file_name()
                                .map(|s| s.to_string_lossy().into_owned())
                                .unwrap_or_else(|| path.to_string_lossy().into_owned());
                            let id = editor.create_window_at(path, label);
                            editor.set_active_window(id);
                            resize_occurred = true; // Force re-render
                        } else {
                            tracing::warn!(
                                "OpenWindow rejected: path must be absolute: {:?}",
                                path
                            );
                        }
                    }
                }
                ClientControl::ListCommands { include_args } => {
                    // Scope the reply to the caller's token allowlist; a
                    // client with no/unknown token gets an empty list (the
                    // channel must not double as a capability probe).
                    let token = self.clients.get(idx).and_then(|c| c.cmd_token.clone());
                    let commands = match (
                        token.as_deref().and_then(command_access::lookup),
                        self.editor.as_ref(),
                    ) {
                        (Some(grant), Some(editor)) => {
                            command_access::list_allowed_commands(editor, &grant, include_args)
                        }
                        _ => Vec::new(),
                    };
                    if let Some(client) = self.clients.get_mut(idx) {
                        let json =
                            serde_json::to_string(&ServerControl::CommandList { commands })
                                .unwrap_or_default();
                        // Best-effort reply
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = client.conn.write_control(&json);
                    }
                }
                ClientControl::RunCommand { id, args } => {
                    let token = self.clients.get(idx).and_then(|c| c.cmd_token.clone());
                    let (ok, error) = command_access::run_command_by_id(
                        self.editor.as_mut(),
                        token.as_deref(),
                        &id,
                        &args,
                    );
                    if ok {
                        resize_occurred = true; // Force re-render
                    }
                    if let Some(client) = self.clients.get_mut(idx) {
                        let json = serde_json::to_string(&ServerControl::CommandResult {
                            ok,
                            error,
                            output: None,
                        })
                        .unwrap_or_default();
                        // Best-effort reply
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = client.conn.write_control(&json);
                    }
                }
                ClientControl::Quit => unreachable!(), // Handled above
            }
        }

        // Check for clients with broken write pipes
        for (idx, client) in self.clients.iter().enumerate() {
            if client.data_writer.is_broken() && !disconnected.contains(&idx) {
                tracing::info!("Client {} write pipe broken, disconnecting", client.id);
                disconnected.push(idx);
            }
        }

        // Deduplicate and sort for safe reverse removal
        disconnected.sort_unstable();
        disconnected.dedup();

        // Remove disconnected clients
        for idx in disconnected.into_iter().rev() {
            let client = self.clients.remove(idx);
            // Clean up --wait tracking if this client was waiting
            if let Some(wait_id) = client.wait_id {
                self.waiting_clients.remove(&wait_id);
                // Also clean up editor wait_tracking for this wait_id
                if let Some(ref mut editor) = self.editor {
                    editor.remove_wait_tracking(wait_id);
                }
            }
            // Best-effort teardown via the non-blocking writer
            let teardown = terminal_teardown_sequences();
            let _ = client.data_writer.try_write(&teardown);
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

        // The interactive wave animation runs until the user does anything:
        // the first key press or mouse activity dismisses it and is consumed
        // (it only stops the show, it doesn't also act on the editor). This
        // mirrors the local terminal loop in `main.rs` so the animation is
        // dismissable in daemon mode too.
        if editor.maybe_dismiss_wave_animation(&event) {
            return Ok(true);
        }

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
            // Best-effort terminal clear for full redraw
            #[allow(clippy::let_underscore_must_use)]
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

        // Broadcast to all clients via non-blocking writer threads (skip waiting clients)
        for client in &mut self.clients {
            if client.wait_id.is_some() {
                continue;
            }
            // Combine pending sequences and output into a single frame
            let frame = if !pending_sequences.is_empty() && !output.is_empty() {
                let mut combined = Vec::with_capacity(pending_sequences.len() + output.len());
                combined.extend_from_slice(&pending_sequences);
                combined.extend_from_slice(&output);
                combined
            } else if !pending_sequences.is_empty() {
                pending_sequences.clone()
            } else {
                output.clone()
            };

            if !frame.is_empty() && !client.data_writer.try_write(&frame) {
                tracing::warn!("Client {} output buffer full, dropping frame", client.id);
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
            // Best-effort: client may already be disconnected
            #[allow(clippy::let_underscore_must_use)]
            let _ = client.data_writer.try_write(&teardown);
            let quit_msg = serde_json::to_string(&ServerControl::Quit {
                reason: reason.to_string(),
            })
            .unwrap_or_default();
            // Best-effort: client may already be disconnected
            #[allow(clippy::let_underscore_must_use)]
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

#[cfg(test)]
mod wave_dismiss_tests {
    //! Regression tests for issue #2530: the interactive wave animation must be
    //! dismissable in daemon (server) mode. The bug was that the server's input
    //! path (`handle_event`) forwarded the abort key straight to the editor and
    //! never cancelled the animation, so the wave could only be stopped by
    //! killing the process. These tests drive the real `handle_event` and assert
    //! the first key / mouse event consumes the input and stops the wave —
    //! matching the local terminal loop in `main.rs`.

    use super::{EditorServer, EditorServerConfig};
    use crate::config::Config;
    use crate::config_io::DirectoryContext;
    use crossterm::event::{
        Event, KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind,
    };
    use std::sync::Arc;
    use std::time::Duration;

    fn unique_session_name(prefix: &str) -> String {
        format!(
            "{}-{}-{}",
            prefix,
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        )
    }

    /// Build a server with an initialized editor, ready to receive input.
    fn server_with_editor(prefix: &str) -> EditorServer {
        let temp_dir = std::env::temp_dir().join(unique_session_name(prefix));
        std::fs::create_dir_all(&temp_dir).unwrap();
        let config = EditorServerConfig {
            working_dir: temp_dir.clone(),
            session_name: Some(unique_session_name(prefix)),
            idle_timeout: Some(Duration::from_secs(30)),
            editor_config: Config::default(),
            dir_context: DirectoryContext::for_testing(&temp_dir),
            plugins_enabled: false,
            init_enabled: false,
            startup_authority: None,
            workspace_trust: Arc::new(
                crate::services::workspace_trust::WorkspaceTrust::permissive(),
            ),
            env_provider: Arc::new(crate::services::env_provider::EnvProvider::inactive()),
            session_keepalive: None,
        };
        let mut server = EditorServer::new(config).expect("EditorServer::new");
        server.initialize_editor().expect("initialize_editor");
        server
    }

    /// A key press while the wave is running stops it and is consumed — it does
    /// not also get typed into the buffer.
    #[test]
    fn key_press_dismisses_wave_in_daemon_mode() {
        let mut server = server_with_editor("wave-key");

        let editor = server.editor_mut().expect("editor present");
        editor.trigger_wave_animation();
        assert!(
            editor.wave_animation_active(),
            "precondition: the wave should be running after triggering it"
        );

        let consumed = server
            .handle_event(Event::Key(KeyEvent::new(
                KeyCode::Char('a'),
                KeyModifiers::empty(),
            )))
            .expect("handle_event");

        let editor = server.editor().expect("editor present");
        assert!(consumed, "the dismiss key should request a re-render");
        assert!(
            !editor.wave_animation_active(),
            "the wave must be dismissable by a key press in daemon mode (issue #2530)"
        );
        assert_eq!(
            editor.active_state().buffer.len(),
            0,
            "the dismiss key must be consumed, not inserted into the buffer"
        );
    }

    /// Mouse activity dismisses the wave too, mirroring the local loop.
    #[test]
    fn mouse_event_dismisses_wave_in_daemon_mode() {
        let mut server = server_with_editor("wave-mouse");

        let editor = server.editor_mut().expect("editor present");
        editor.trigger_wave_animation();
        assert!(editor.wave_animation_active(), "precondition: wave running");

        let consumed = server
            .handle_event(Event::Mouse(MouseEvent {
                kind: MouseEventKind::Down(MouseButton::Left),
                column: 1,
                row: 1,
                modifiers: KeyModifiers::empty(),
            }))
            .expect("handle_event");

        assert!(consumed, "the dismiss event should request a re-render");
        assert!(
            !server
                .editor()
                .expect("editor present")
                .wave_animation_active(),
            "the wave must be dismissable by mouse activity in daemon mode (issue #2530)"
        );
    }
}
