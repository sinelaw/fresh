//! Terminal Manager - manages multiple terminal sessions
//!
//! This module provides a manager for terminal sessions that:
//! - Spawns PTY processes with proper shell detection
//! - Manages multiple concurrent terminals
//! - Routes input/output between the editor and terminal processes
//! - Handles terminal resize events
//!
//! # Role in Incremental Streaming Architecture
//!
//! The manager owns the PTY read loop which is the entry point for incremental
//! scrollback streaming. See `super` module docs for the full architecture overview.
//!
//! ## PTY Read Loop
//!
//! The read loop in `spawn()` performs incremental streaming: for each PTY read,
//! it calls `process_output()` to update the terminal grid, then `flush_new_scrollback()`
//! to append any new scrollback lines to the backing file. This ensures scrollback is
//! written incrementally as lines scroll off screen, avoiding O(n) work on mode switches.

use super::term::TerminalState;
use crate::services::async_bridge::AsyncBridge;
use crate::services::authority::TerminalWrapper;
use portable_pty::{native_pty_system, CommandBuilder, PtySize};
use std::borrow::Cow;
use std::collections::HashMap;
use std::io::{Read, Write};
use std::sync::atomic::AtomicBool;
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::thread;

pub use fresh_core::TerminalId;

/// Messages sent to terminal I/O thread
enum TerminalCommand {
    /// Write data to PTY
    Write(Vec<u8>),
    /// Resize the PTY
    Resize { cols: u16, rows: u16 },
    /// Shutdown the terminal
    Shutdown,
}

/// The `(window, terminal)` identity stamped on this terminal's async
/// messages, shared with the reader and wait threads. A `Mutex` (rather
/// than a plain captured copy) so the tag can be rewritten when another
/// window's manager adopts a live terminal — see
/// [`TerminalManager::adopt`]; the threads read it at each send.
type SharedWtId = Arc<Mutex<fresh_core::WindowTerminalId>>;

/// Handle to a running terminal session
pub struct TerminalHandle {
    /// Terminal state (grid, cursor, etc.)
    pub state: Arc<Mutex<TerminalState>>,
    /// Command sender to I/O thread
    command_tx: mpsc::Sender<TerminalCommand>,
    /// Whether the terminal is still alive
    alive: Arc<std::sync::atomic::AtomicBool>,
    /// Current dimensions
    cols: u16,
    rows: u16,
    /// Working directory used for the terminal
    cwd: Option<std::path::PathBuf>,
    /// Shell executable used to spawn the terminal
    shell: String,
    /// PID of the shell child process at the head of the pty's
    /// session. `kill(-pid, signal)` (note the negation) signals
    /// the entire process group, which catches subprocesses the
    /// shell or agent forked. `None` on Windows or when
    /// portable_pty couldn't report the pid.
    pid: Option<u32>,
    /// PTY master file descriptor, captured at spawn. Used to read the
    /// terminal's foreground process group via `tcgetpgrp` for tmux-style
    /// tab auto-naming. `None` on Windows or when the platform doesn't
    /// expose it. Only read on Linux (the only `/proc`-backed target).
    #[cfg_attr(not(target_os = "linux"), allow(dead_code))]
    master_fd: Option<i32>,
    /// The identity tag shared with this terminal's reader/wait threads,
    /// rewritten when the handle moves to another window's manager.
    wt_id: SharedWtId,
}

impl TerminalHandle {
    /// Write data to the terminal (sends to PTY)
    pub fn write(&self, data: &[u8]) {
        // Receiver may be dropped if terminal exited; nothing to do in that case.
        #[allow(clippy::let_underscore_must_use)]
        let _ = self.command_tx.send(TerminalCommand::Write(data.to_vec()));
    }

    /// Resize the terminal
    pub fn resize(&mut self, cols: u16, rows: u16) {
        if cols != self.cols || rows != self.rows {
            self.cols = cols;
            self.rows = rows;
            // Receiver may be dropped if terminal exited; nothing to do in that case.
            #[allow(clippy::let_underscore_must_use)]
            let _ = self.command_tx.send(TerminalCommand::Resize { cols, rows });
            // Also resize the terminal state
            if let Ok(mut state) = self.state.lock() {
                state.resize(cols, rows);
            }
        }
    }

    /// Check if the terminal is still running
    pub fn is_alive(&self) -> bool {
        self.alive.load(std::sync::atomic::Ordering::Relaxed)
    }

    /// Shutdown the terminal
    pub fn shutdown(&self) {
        // Receiver may be dropped if terminal already exited; nothing to do in that case.
        #[allow(clippy::let_underscore_must_use)]
        let _ = self.command_tx.send(TerminalCommand::Shutdown);
    }

    /// Pid of the shell at the head of the pty session, when
    /// portable_pty was able to report it. Returns `None` on
    /// platforms / configurations that don't expose a pid.
    pub fn pid(&self) -> Option<u32> {
        self.pid
    }

    /// Name of the command currently in the foreground of this terminal,
    /// e.g. `"bash"` at the prompt or `"python3"` while a REPL runs.
    ///
    /// Derived from the PTY's foreground process *group* (`tcgetpgrp` on
    /// the master fd) rather than the shell pid, so it tracks whatever the
    /// user is actually interacting with — the same signal tmux uses for
    /// `#{pane_current_command}`. This is how a tab can read `python3`
    /// even though `python3` never emits an OSC title sequence.
    ///
    /// Only implemented on Linux (via `/proc/<pgid>/comm`); returns `None`
    /// elsewhere so callers fall back to the OSC title or default name.
    pub fn foreground_process_name(&self) -> Option<String> {
        #[cfg(target_os = "linux")]
        {
            let fd = self.master_fd?;
            // SAFETY: `fd` is the PTY master, kept open by the writer
            // thread for the terminal's lifetime. `tcgetpgrp` only reads.
            let pgid = unsafe { libc::tcgetpgrp(fd) };
            if pgid <= 0 {
                return None;
            }
            // Local OS introspection of a local fd. The `FileSystem` trait
            // abstracts the *editing* filesystem (possibly remote); it does
            // not apply to reading this host's `/proc`.
            let comm = std::fs::read_to_string(format!("/proc/{pgid}/comm")).ok()?;
            let name = comm.trim();
            if name.is_empty() {
                None
            } else {
                Some(name.to_string())
            }
        }
        #[cfg(not(target_os = "linux"))]
        {
            None
        }
    }

    /// Send `signal` to the terminal's process group. Returns
    /// `Ok(false)` when the terminal has no recorded pid
    /// (Windows, or platforms where portable_pty didn't report
    /// one) — caller can fall back to `shutdown()` (SIGKILL via
    /// child_killer). The shell is always its own session
    /// leader inside a pty, so `kill(-pid, …)` reaches the
    /// shell *and* any subprocesses it forked.
    ///
    /// Recognised signal names: `"SIGTERM"`, `"SIGKILL"`,
    /// `"SIGINT"`, `"SIGHUP"`. Unknown names return an Err
    /// instead of dropping silently.
    #[cfg(unix)]
    pub fn signal(&self, signal_name: &str) -> Result<bool, String> {
        let Some(pid) = self.pid else {
            return Ok(false);
        };
        let sig = match signal_name {
            "SIGTERM" => libc::SIGTERM,
            "SIGKILL" => libc::SIGKILL,
            "SIGINT" => libc::SIGINT,
            "SIGHUP" => libc::SIGHUP,
            other => return Err(format!("unsupported signal: {}", other)),
        };
        // `kill(-pid, sig)` targets the process group whose
        // leader is `pid`. The pty puts the spawned shell at
        // the head of its own session, so this catches
        // sub-processes the shell or agent forked.
        let rc = unsafe { libc::kill(-(pid as i32), sig) };
        if rc == 0 {
            Ok(true)
        } else {
            let err = std::io::Error::last_os_error();
            // ESRCH = no such process group. Treat as
            // "nothing to signal" rather than an error so the
            // caller's stop flow stays idempotent.
            if err.raw_os_error() == Some(libc::ESRCH) {
                Ok(false)
            } else {
                Err(format!("kill(-{}, {}): {}", pid, signal_name, err))
            }
        }
    }

    /// Windows fallback: no real signal semantics. SIGKILL is
    /// modelled as the existing `shutdown()` (which calls the
    /// pty child killer); other signals are unsupported and
    /// return Ok(false).
    #[cfg(windows)]
    pub fn signal(&self, signal_name: &str) -> Result<bool, String> {
        if signal_name == "SIGKILL" {
            self.shutdown();
            return Ok(true);
        }
        Ok(false)
    }

    /// Get current dimensions
    pub fn size(&self) -> (u16, u16) {
        (self.cols, self.rows)
    }

    /// Get the working directory configured for the terminal
    pub fn cwd(&self) -> Option<std::path::PathBuf> {
        self.cwd.clone()
    }

    /// The shell's *live* working directory — where the user has `cd`'d to,
    /// not where the terminal was spawned. Read from `/proc/<pid>/cwd` on
    /// Linux; other platforms (and dead pids) fall back to the spawn cwd.
    pub fn current_working_dir(&self) -> Option<std::path::PathBuf> {
        #[cfg(target_os = "linux")]
        {
            if let Some(pid) = self.pid {
                // Local OS introspection, same rationale as
                // `foreground_process_name`: this reads the host's /proc,
                // not the (possibly remote) editing filesystem.
                if let Ok(cwd) = std::fs::read_link(format!("/proc/{pid}/cwd")) {
                    return Some(cwd);
                }
            }
        }
        self.cwd.clone()
    }

    /// Get the shell executable path used for this terminal
    pub fn shell(&self) -> &str {
        &self.shell
    }
}

/// Manager for multiple terminal sessions
pub struct TerminalManager {
    /// The window that owns this manager. Terminal IDs are only unique
    /// within a single manager (each starts numbering at 0), so output
    /// messages are tagged with `(window_id, terminal_id)` — see
    /// [`fresh_core::WindowTerminalId`] — to stay unambiguous once they
    /// leave this window's context (e.g. on the async bus).
    window_id: fresh_core::WindowId,
    /// Map from terminal ID to handle
    terminals: HashMap<TerminalId, TerminalHandle>,
    /// Next terminal ID
    next_id: usize,
    /// Async bridge for sending notifications to main loop
    async_bridge: Option<AsyncBridge>,
}

impl TerminalManager {
    /// Create a new terminal manager owned by `window_id`. The owner is
    /// required (not defaulted) so output can never be attributed to the
    /// wrong window: every terminal this manager spawns is tagged with
    /// it.
    pub fn new(window_id: fresh_core::WindowId) -> Self {
        Self {
            window_id,
            terminals: HashMap::new(),
            next_id: 0,
            async_bridge: None,
        }
    }

    /// The window that owns this manager.
    pub fn window_id(&self) -> fresh_core::WindowId {
        self.window_id
    }

    /// Set the async bridge for communication with main loop
    pub fn set_async_bridge(&mut self, bridge: AsyncBridge) {
        self.async_bridge = Some(bridge);
    }

    /// Peek at the next terminal ID that would be assigned.
    pub fn next_terminal_id(&self) -> TerminalId {
        TerminalId(self.next_id)
    }

    /// Spawn a new terminal session
    ///
    /// # Arguments
    /// * `cols` - Initial terminal width in columns
    /// * `rows` - Initial terminal height in rows
    /// * `cwd` - Optional working directory (defaults to current directory)
    /// * `log_path` - Optional path for raw PTY log (for session restore)
    /// * `backing_path` - Optional path for rendered scrollback (incremental streaming)
    ///
    /// # Returns
    /// The terminal ID if successful
    #[allow(clippy::too_many_arguments)]
    pub fn spawn(
        &mut self,
        cols: u16,
        rows: u16,
        cwd: Option<std::path::PathBuf>,
        log_path: Option<std::path::PathBuf>,
        backing_path: Option<std::path::PathBuf>,
        terminal_wrapper: crate::services::authority::TerminalWrapper,
        env_delta: crate::services::env_provider::EnvDelta,
    ) -> Result<TerminalId, String> {
        let id = TerminalId(self.next_id);
        self.next_id += 1;

        let handle = self.build_terminal(
            id,
            cols,
            rows,
            cwd,
            log_path,
            backing_path,
            terminal_wrapper,
            env_delta,
        )?;

        self.terminals.insert(id, handle);
        tracing::info!("Created terminal {:?} ({}x{})", id, cols, rows);

        Ok(id)
    }

    /// Build a PTY-backed terminal: open the pty, launch the shell, and wire up
    /// the three background threads (reader, wait, writer) that drive it. Kept
    /// separate from [`TerminalManager::spawn`] so the happy path reads
    /// top-to-bottom with `?` instead of being buried in an error-handling
    /// closure.
    #[allow(clippy::too_many_arguments)]
    fn build_terminal(
        &self,
        id: TerminalId,
        cols: u16,
        rows: u16,
        cwd: Option<std::path::PathBuf>,
        log_path: Option<std::path::PathBuf>,
        backing_path: Option<std::path::PathBuf>,
        terminal_wrapper: TerminalWrapper,
        env_delta: crate::services::env_provider::EnvDelta,
    ) -> Result<TerminalHandle, String> {
        let pty_pair = open_pty(cols, rows)?;

        // The active authority's terminal wrapper drives the shell command
        // unconditionally — local wraps `detect_shell()` with no args;
        // container/remote authorities re-parent into `docker exec -w …`,
        // `ssh …`, etc.
        let (cmd, shell) = build_shell_command(terminal_wrapper, cwd.as_deref(), &env_delta);

        // Spawn the shell process.
        let child = pty_pair
            .slave
            .spawn_command(cmd)
            .map_err(|e| format!("Failed to spawn shell '{}': {}", shell, e))?;
        tracing::debug!("Shell process spawned successfully");

        // Capture the pid (for process-group signalling) and a killer handle
        // before `child` moves into the wait-thread below.
        let child_pid = child.process_id();
        let child_killer = child.clone_killer();

        let state = Arc::new(Mutex::new(TerminalState::new(cols, rows)));

        // If the backing file already exists (session restore), seed the history
        // end so entering terminal mode doesn't truncate it to 0.
        if let Some(p) = backing_path.as_ref() {
            if let Ok(metadata) = std::fs::metadata(p) {
                if metadata.len() > 0 {
                    if let Ok(mut s) = state.lock() {
                        s.set_backing_file_history_end(metadata.len());
                    }
                }
            }
        }

        let (command_tx, command_rx) = mpsc::channel::<TerminalCommand>();
        let alive = Arc::new(AtomicBool::new(true));

        let master_writer = pty_pair
            .master
            .take_writer()
            .map_err(|e| format!("Failed to get PTY writer: {}", e))?;
        let reader = pty_pair
            .master
            .try_clone_reader()
            .map_err(|e| format!("Failed to get PTY reader: {}", e))?;

        let log_writer = open_log_writer(log_path.as_deref());
        let backing_writer = open_backing_writer(backing_path.as_deref());

        // Tag output/exit with the owning window so the main loop never has to
        // guess which session a `Terminal-N` belongs to (ids collide across
        // windows). See `fresh_core::WindowTerminalId`. Shared (not copied)
        // with the reader/wait threads so `adopt` can retag a live terminal
        // when it moves to another window's manager.
        let wt_id: SharedWtId = Arc::new(Mutex::new(fresh_core::WindowTerminalId::new(
            self.window_id,
            id,
        )));

        // Reader thread: drains PTY output, feeds the emulator, streams
        // scrollback / raw log to disk, and pings the main loop to redraw.
        let reader_loop = ReaderLoop {
            reader,
            state: state.clone(),
            response_tx: command_tx.clone(),
            backing_writer,
            log_writer,
            async_bridge: self.async_bridge.clone(),
            wt_id: wt_id.clone(),
            terminal_id: id,
            alive: alive.clone(),
        };
        thread::spawn(move || reader_loop.run());

        // Wait thread: blocks on `child.wait()` and fires `TerminalExited`
        // exactly once with the real exit code.
        spawn_wait_thread(child, self.async_bridge.clone(), wt_id.clone(), id);

        // Capture the PTY master fd before the master moves into the writer
        // thread. Used later by `foreground_process_name` (tab auto-naming).
        let master_fd: Option<i32> = {
            #[cfg(unix)]
            {
                pty_pair.master.as_raw_fd()
            }
            #[cfg(not(unix))]
            {
                None
            }
        };

        // Writer thread: owns the master, applies queued writes/resizes, and
        // kills the child on shutdown.
        spawn_writer_thread(command_rx, master_writer, pty_pair.master, child_killer);

        Ok(TerminalHandle {
            state,
            command_tx,
            alive,
            cols,
            rows,
            cwd,
            shell,
            pid: child_pid,
            master_fd,
            wt_id,
        })
    }

    /// Remove a live terminal from this manager *without* shutting it down,
    /// so another window's manager can [`Self::adopt`] it. The PTY, its
    /// reader/writer/wait threads, and the running process are untouched.
    pub fn release(&mut self, id: TerminalId) -> Option<TerminalHandle> {
        self.terminals.remove(&id)
    }

    /// Adopt a live terminal released from another window's manager.
    ///
    /// Assigns the handle a fresh id in this manager's namespace (ids are
    /// per-window and would otherwise collide) and rewrites the shared
    /// `(window, terminal)` tag, so output/exit messages the terminal's
    /// threads send from now on are attributed to this window. Returns the
    /// new id.
    pub fn adopt(&mut self, handle: TerminalHandle) -> TerminalId {
        let id = TerminalId(self.next_id);
        self.next_id += 1;
        if let Ok(mut wt_id) = handle.wt_id.lock() {
            *wt_id = fresh_core::WindowTerminalId::new(self.window_id, id);
        }
        self.terminals.insert(id, handle);
        id
    }

    /// Get a terminal handle by ID
    pub fn get(&self, id: TerminalId) -> Option<&TerminalHandle> {
        self.terminals.get(&id)
    }

    /// Get a mutable terminal handle by ID
    pub fn get_mut(&mut self, id: TerminalId) -> Option<&mut TerminalHandle> {
        self.terminals.get_mut(&id)
    }

    /// Close a terminal
    pub fn close(&mut self, id: TerminalId) -> bool {
        if let Some(handle) = self.terminals.remove(&id) {
            handle.shutdown();
            true
        } else {
            false
        }
    }

    /// Get all terminal IDs
    pub fn terminal_ids(&self) -> Vec<TerminalId> {
        self.terminals.keys().copied().collect()
    }

    /// Get count of open terminals
    pub fn count(&self) -> usize {
        self.terminals.len()
    }

    /// Shutdown all terminals
    pub fn shutdown_all(&mut self) {
        for (_, handle) in self.terminals.drain() {
            handle.shutdown();
        }
    }

    /// Clean up dead terminals
    pub fn cleanup_dead(&mut self) -> Vec<TerminalId> {
        let dead: Vec<TerminalId> = self
            .terminals
            .iter()
            .filter(|(_, h)| !h.is_alive())
            .map(|(id, _)| *id)
            .collect();

        for id in &dead {
            self.terminals.remove(id);
        }

        dead
    }
}

/// Open a native PTY of the given size, mapping the platform error into a
/// human-readable string (with a ConPTY hint on Windows).
fn open_pty(cols: u16, rows: u16) -> Result<portable_pty::PtyPair, String> {
    native_pty_system()
        .openpty(PtySize {
            rows,
            cols,
            pixel_width: 0,
            pixel_height: 0,
        })
        .map_err(|e| {
            #[cfg(windows)]
            {
                format!(
                    "Failed to open PTY: {}. Note: Terminal requires Windows 10 version 1809 or later with ConPTY support.",
                    e
                )
            }
            #[cfg(not(windows))]
            {
                format!("Failed to open PTY: {}", e)
            }
        })
}

/// Build the shell `CommandBuilder` for a terminal from the active authority's
/// wrapper, returning the command plus the shell executable name (for the
/// handle / diagnostics). `manages_cwd` wrappers (docker/ssh) already establish
/// cwd in their own args, so both cwd and the local `FRESH_SESSION`
/// advertisement are skipped for them — their inner shell runs on another host
/// this `CommandBuilder`'s env can't reach.
fn build_shell_command(
    terminal_wrapper: TerminalWrapper,
    cwd: Option<&std::path::Path>,
    env_delta: &crate::services::env_provider::EnvDelta,
) -> (CommandBuilder, String) {
    let TerminalWrapper {
        command: shell,
        args: cmd_args,
        manages_cwd: skip_cwd,
    } = terminal_wrapper;
    tracing::info!("Spawning terminal with shell: {}", shell);

    let mut cmd = CommandBuilder::new(&shell);
    for arg in &cmd_args {
        cmd.arg(arg);
    }
    if !skip_cwd {
        if let Some(dir) = cwd {
            // Hand the shell a non-verbatim path so PowerShell can infer the
            // drive; a verbatim `\\?\C:\…` path yields provider-prefixed prompts.
            cmd.cwd(strip_verbatim_prefix(dir).as_ref());
        }
    }

    // Apply the activated-environment delta (venv/direnv/mise) before the
    // control vars below, so TERM/FRESH_SESSION win over any same-named key
    // (issue #2355).
    for (k, v) in &env_delta.set {
        cmd.env(k, v);
    }
    for k in &env_delta.unset {
        cmd.env_remove(k);
    }

    // Advertise terminal capabilities; the built-in emulator is alacritty-based.
    cmd.env("TERM", "xterm-256color");

    // Advertise this editor's local control socket so a nested `fresh` forwards
    // file/dir opens back to us instead of starting a second editor.
    if !skip_cwd {
        if let Some(session_id) = crate::server::local_control::local_session_id() {
            cmd.env("FRESH_SESSION", session_id);
        }
    }

    // On Windows, ensure PROMPT is set for cmd.exe.
    #[cfg(windows)]
    {
        if shell.to_lowercase().contains("cmd") {
            cmd.env("PROMPT", "$P$G");
        }
    }

    (cmd, shell)
}

/// Open the optional raw-PTY log file (append mode) for full-session capture.
fn open_log_writer(
    log_path: Option<&std::path::Path>,
) -> Option<std::io::BufWriter<std::fs::File>> {
    log_path
        .and_then(|p| {
            std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(p)
                .ok()
        })
        .map(std::io::BufWriter::new)
}

/// Open the optional scrollback backing file. On session restore (the file
/// already has content) we append to continue streaming; otherwise we truncate
/// to start fresh.
fn open_backing_writer(
    backing_path: Option<&std::path::Path>,
) -> Option<std::io::BufWriter<std::fs::File>> {
    backing_path
        .and_then(|p| {
            let existing_has_content =
                p.exists() && std::fs::metadata(p).map(|m| m.len() > 0).unwrap_or(false);
            if existing_has_content {
                std::fs::OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(p)
                    .ok()
            } else {
                std::fs::OpenOptions::new()
                    .create(true)
                    .write(true)
                    .truncate(true)
                    .open(p)
                    .ok()
            }
        })
        .map(std::io::BufWriter::new)
}

/// Wait-thread body: block on the child's exit and fire `TerminalExited` once.
/// Owns `child` so it is the single source of the exit status (the reader
/// thread deliberately doesn't fire it, to avoid a racing `exit_code: None`).
fn spawn_wait_thread(
    mut child: Box<dyn portable_pty::Child + Send + Sync>,
    async_bridge: Option<AsyncBridge>,
    wt_id: SharedWtId,
    terminal_id: TerminalId,
) {
    thread::spawn(move || {
        let exit_code = match child.wait() {
            Ok(status) => Some(status.exit_code() as i32),
            Err(e) => {
                tracing::warn!("child.wait() failed for {:?}: {}", terminal_id, e);
                None
            }
        };
        if let Some(bridge) = &async_bridge {
            // Read the tag at exit time — the terminal may have been
            // adopted by another window since it was spawned.
            let Ok(terminal) = wt_id.lock().map(|id| *id) else {
                return;
            };
            #[allow(clippy::let_underscore_must_use)]
            let _ = bridge.sender().send(
                crate::services::async_bridge::AsyncMessage::TerminalExited {
                    terminal,
                    exit_code,
                },
            );
        }
    });
}

/// Writer-thread body: own the master, apply queued writes/resizes, and kill
/// the child on shutdown. The wait-thread reaps the exit status, so this thread
/// intentionally doesn't call `wait` (which would race it).
fn spawn_writer_thread(
    command_rx: mpsc::Receiver<TerminalCommand>,
    mut master: Box<dyn Write + Send>,
    pty_master: Box<dyn portable_pty::MasterPty + Send>,
    mut child_killer: Box<dyn portable_pty::ChildKiller + Send + Sync>,
) {
    thread::spawn(move || {
        loop {
            match command_rx.recv() {
                Ok(TerminalCommand::Write(data)) => {
                    if let Err(e) = master.write_all(&data) {
                        tracing::error!("Terminal write error: {}", e);
                        break;
                    }
                    // Best-effort flush — PTY write errors are handled above.
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = master.flush();
                }
                Ok(TerminalCommand::Resize { cols, rows }) => {
                    if let Err(e) = pty_master.resize(PtySize {
                        rows,
                        cols,
                        pixel_width: 0,
                        pixel_height: 0,
                    }) {
                        tracing::warn!("Failed to resize PTY: {}", e);
                    }
                }
                Ok(TerminalCommand::Shutdown) | Err(_) => {
                    break;
                }
            }
        }
        // User-initiated shutdown: ask the OS to terminate the child via the
        // cloned killer. The wait-thread owns `child` and reaps the status.
        #[allow(clippy::let_underscore_must_use)]
        let _ = child_killer.kill();
    });
}

/// Owns everything the PTY reader thread needs. Bundled into one struct so the
/// thread body is a readable `run(self)` of small steps instead of a closure
/// capturing a dozen locals at deep nesting.
struct ReaderLoop {
    reader: Box<dyn Read + Send>,
    state: Arc<Mutex<TerminalState>>,
    /// Sends PTY write-responses (e.g. DSR cursor reports) back to the writer.
    response_tx: mpsc::Sender<TerminalCommand>,
    /// Incremental scrollback stream (rendered lines), if a backing file is set.
    backing_writer: Option<std::io::BufWriter<std::fs::File>>,
    /// Raw byte log for session-restore replay, if a log file is set.
    log_writer: Option<std::io::BufWriter<std::fs::File>>,
    async_bridge: Option<AsyncBridge>,
    wt_id: SharedWtId,
    terminal_id: TerminalId,
    alive: Arc<AtomicBool>,
}

impl ReaderLoop {
    /// Drain the PTY until EOF or error, then mark the terminal dead and flush.
    fn run(mut self) {
        tracing::debug!("Terminal {:?} reader thread started", self.terminal_id);
        let mut buf = [0u8; 4096];
        let mut total_bytes = 0usize;
        loop {
            match self.reader.read(&mut buf) {
                Ok(0) => {
                    // EOF - process exited.
                    tracing::info!(
                        "Terminal {:?} EOF after {} total bytes",
                        self.terminal_id,
                        total_bytes
                    );
                    break;
                }
                Ok(n) => {
                    total_bytes += n;
                    // Hot path: a busy terminal reads tens of thousands of
                    // chunks/sec, so this stays at `trace` (off by default) to
                    // avoid flooding the log — and, if that log is tailed into a
                    // terminal this manager owns, a positive-feedback loop.
                    tracing::trace!(
                        "Terminal {:?} received {} bytes (total: {})",
                        self.terminal_id,
                        n,
                        total_bytes
                    );
                    self.process_output(&buf[..n]);
                    self.append_raw_log(&buf[..n]);
                    self.notify_redraw();
                }
                Err(e) => {
                    tracing::error!("Terminal read error: {}", e);
                    break;
                }
            }
        }
        self.alive
            .store(false, std::sync::atomic::Ordering::Relaxed);
        // Best-effort flush of log/backing files during teardown. The
        // wait-thread is the single source of `TerminalExited`, so the reader
        // intentionally does not fire it here (firing from both races and can
        // yield `exit_code: None` despite a clean exit).
        if let Some(mut w) = self.log_writer.take() {
            #[allow(clippy::let_underscore_must_use)]
            let _ = w.flush();
        }
        if let Some(mut w) = self.backing_writer.take() {
            #[allow(clippy::let_underscore_must_use)]
            let _ = w.flush();
        }
    }

    /// Feed `bytes` to the emulator, forward any PTY write-responses, and stream
    /// new scrollback to the backing file. Holds the state lock for the whole
    /// step so scrollback offsets stay consistent with the emulator grid.
    fn process_output(&mut self, bytes: &[u8]) {
        let Ok(mut state) = self.state.lock() else {
            return;
        };
        state.process_output(bytes);

        // Send any PTY write responses (e.g. DSR cursor position). Critical on
        // Windows ConPTY, where PowerShell waits for this before prompting.
        for response in state.drain_pty_write_queue() {
            tracing::debug!(
                "Terminal {:?} sending PTY response: {:?}",
                self.terminal_id,
                response
            );
            // Receiver may be dropped if the writer thread exited.
            #[allow(clippy::let_underscore_must_use)]
            let _ = self
                .response_tx
                .send(TerminalCommand::Write(response.into_bytes()));
        }

        // Incrementally stream new scrollback lines to the backing file.
        if let Some(writer) = self.backing_writer.as_mut() {
            match state.flush_new_scrollback(writer) {
                Ok(lines_written) => {
                    if lines_written > 0 {
                        if let Ok(pos) = writer.get_ref().metadata() {
                            state.set_backing_file_history_end(pos.len());
                        }
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = writer.flush();
                    }
                }
                Err(e) => {
                    tracing::warn!("Terminal backing file write error: {}", e);
                    self.backing_writer = None;
                }
            }
        }
    }

    /// Append raw bytes to the session log (for restore replay), if enabled.
    fn append_raw_log(&mut self, bytes: &[u8]) {
        if let Some(w) = self.log_writer.as_mut() {
            if let Err(e) = w.write_all(bytes) {
                tracing::warn!("Terminal log write error: {}", e);
                self.log_writer = None;
            } else if let Err(e) = w.flush() {
                tracing::warn!("Terminal log flush error: {}", e);
                self.log_writer = None;
            }
        }
    }

    /// Notify the main loop that this terminal produced output (redraw).
    fn notify_redraw(&self) {
        if let Some(bridge) = &self.async_bridge {
            // Read the tag per send — the terminal may have been adopted by
            // another window since it was spawned.
            let Ok(terminal) = self.wt_id.lock().map(|id| *id) else {
                return;
            };
            #[allow(clippy::let_underscore_must_use)]
            let _ = bridge
                .sender()
                .send(crate::services::async_bridge::AsyncMessage::TerminalOutput { terminal });
        }
    }
}

impl Drop for TerminalManager {
    fn drop(&mut self) {
        self.shutdown_all();
    }
}

/// Convert a Windows verbatim path (`\\?\C:\…` or `\\?\UNC\server\share\…`)
/// into its non-verbatim equivalent (`C:\…` or `\\server\share\…`).
///
/// Returns the input unchanged on non-Windows platforms or for paths that
/// have no verbatim prefix.
pub(crate) fn strip_verbatim_prefix(path: &std::path::Path) -> Cow<'_, std::path::Path> {
    #[cfg(windows)]
    {
        use std::path::{Component, Prefix};

        let mut components = path.components();
        let prefix = match components.next() {
            Some(Component::Prefix(p)) => p,
            _ => return Cow::Borrowed(path),
        };

        let mut rebuilt = std::path::PathBuf::new();
        match prefix.kind() {
            Prefix::VerbatimDisk(drive) => {
                rebuilt.push(format!("{}:\\", drive as char));
            }
            Prefix::VerbatimUNC(server, share) => {
                rebuilt.push(format!(
                    r"\\{}\{}\",
                    server.to_string_lossy(),
                    share.to_string_lossy()
                ));
            }
            _ => return Cow::Borrowed(path),
        }
        // Skip the original RootDir (which the rebuilt prefix already includes)
        // and append the rest of the components.
        for component in components {
            if matches!(component, Component::RootDir) {
                continue;
            }
            rebuilt.push(component.as_os_str());
        }
        Cow::Owned(rebuilt)
    }
    #[cfg(not(windows))]
    {
        Cow::Borrowed(path)
    }
}

/// Detect the user's shell
pub fn detect_shell() -> String {
    // Try $SHELL environment variable first
    if let Ok(shell) = std::env::var("SHELL") {
        if !shell.is_empty() {
            return shell;
        }
    }

    // Fall back to platform defaults
    #[cfg(unix)]
    {
        "/bin/sh".to_string()
    }
    #[cfg(windows)]
    {
        super::windows_shell::select_windows_shell()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_terminal_id_display() {
        let id = TerminalId(42);
        assert_eq!(format!("{}", id), "Terminal-42");
    }

    /// Terminal ids are per-window: each manager numbers from 0, so two
    /// windows both hand out `Terminal-0`. The owning window is what
    /// disambiguates them — output messages are tagged with the
    /// `(window, terminal)` pair so a `Terminal-0` from one session can't
    /// be attributed to another session's `Terminal-0`. (Regression
    /// guard for the dock "pending output on the wrong session" bug.)
    #[test]
    fn terminal_ids_collide_across_windows_but_window_disambiguates() {
        use fresh_core::{WindowId, WindowTerminalId};

        let win_a = TerminalManager::new(WindowId(1));
        let win_b = TerminalManager::new(WindowId(2));

        // Both managers would assign the same local id to their first
        // terminal — the namespaces are independent.
        assert_eq!(win_a.next_terminal_id(), win_b.next_terminal_id());
        assert_eq!(win_a.next_terminal_id(), TerminalId(0));

        // Each manager knows its owner, so the global identity differs.
        assert_eq!(win_a.window_id(), WindowId(1));
        assert_eq!(win_b.window_id(), WindowId(2));
        let a0 = WindowTerminalId::new(win_a.window_id(), win_a.next_terminal_id());
        let b0 = WindowTerminalId::new(win_b.window_id(), win_b.next_terminal_id());
        assert_ne!(
            a0, b0,
            "same local terminal id in different windows must be distinct globally"
        );
    }

    #[test]
    fn test_detect_shell() {
        let shell = detect_shell();
        assert!(!shell.is_empty());
    }

    #[cfg(not(windows))]
    #[test]
    fn strip_verbatim_prefix_is_noop_on_unix() {
        use std::path::Path;
        let p = Path::new("/home/user/project");
        assert_eq!(strip_verbatim_prefix(p).as_ref(), p);
    }

    #[cfg(windows)]
    #[test]
    fn strip_verbatim_prefix_removes_verbatim_disk() {
        use std::path::{Path, PathBuf};
        let verbatim = PathBuf::from(r"\\?\C:\Users\HP\OneDrive\Desktop\PY'PGMS");
        let stripped = strip_verbatim_prefix(&verbatim);
        assert_eq!(
            stripped.as_ref(),
            Path::new(r"C:\Users\HP\OneDrive\Desktop\PY'PGMS"),
            "verbatim disk prefix should be replaced with plain drive form"
        );
    }

    #[cfg(windows)]
    #[test]
    fn strip_verbatim_prefix_removes_verbatim_unc() {
        use std::path::{Path, PathBuf};
        let verbatim = PathBuf::from(r"\\?\UNC\server\share\dir\file");
        let stripped = strip_verbatim_prefix(&verbatim);
        assert_eq!(
            stripped.as_ref(),
            Path::new(r"\\server\share\dir\file"),
            "verbatim UNC prefix should be replaced with plain UNC form"
        );
    }

    #[cfg(windows)]
    #[test]
    fn strip_verbatim_prefix_passes_plain_paths_through() {
        use std::path::{Path, PathBuf};
        let plain = PathBuf::from(r"C:\Users\HP\project");
        let result = strip_verbatim_prefix(&plain);
        assert_eq!(result.as_ref(), Path::new(r"C:\Users\HP\project"));
    }
}
