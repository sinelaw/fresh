//! In-process control listener for "direct" (non-server) mode.
//!
//! When Fresh runs as a plain in-process editor (no detached daemon
//! server), it still binds a *control* socket so that a `fresh`
//! invoked from inside its own embedded terminal can forward
//! file/directory open requests back to this process instead of
//! launching a second editor in the terminal.
//!
//! How it fits together:
//!
//! - On startup [`start`] binds a unique per-process control socket and
//!   spawns an accept thread, and records the session id so the embedded
//!   terminal can advertise it to children via `FRESH_SESSION`.
//! - A `fresh` launched with `FRESH_SESSION` set connects as a thin
//!   client (see `try_forward_nested` in `main.rs`) and sends
//!   [`ClientControl::OpenFiles`] / [`ClientControl::OpenWindow`].
//! - Each accepted connection is handled on its own thread, which hands
//!   the decoded request to the editor's main loop via a channel and —
//!   for `--wait`-style file opens — parks until the buffer is closed.
//! - [`pump`] runs once per frame on the editor thread, applying queued
//!   requests through the editor's existing `queue_file_open` /
//!   `create_window_at` machinery and waking parked connections when a
//!   wait completes.
//!
//! This deliberately reuses the same IPC primitives ([`ServerListener`],
//! [`ServerConnection`]) and wire protocol ([`ClientControl`] /
//! [`ServerControl`]) as the full daemon server, but does *not* render:
//! the direct-mode crossterm render/input path is untouched.

use std::collections::HashMap;
use std::io::BufRead;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::mpsc::{self, Receiver, Sender};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::Duration;

use crate::app::Editor;
use crate::server::command_access;
use crate::server::ipc::{ServerConnection, ServerListener, SocketPaths, StreamWrapper};
use crate::server::protocol::{
    ClientControl, FileRequest, ServerControl, ServerHello, VersionMismatch, PROTOCOL_VERSION,
};

/// Time the accept thread sleeps between non-blocking accept polls. Only
/// affects how quickly a freshly-launched nested `fresh` is picked up;
/// nested launches are human-interactive (e.g. `git commit`), so this is
/// imperceptible.
const ACCEPT_POLL_INTERVAL: Duration = Duration::from_millis(20);

/// Backstop on how long a connection handler waits for a dispatched command's
/// answer before giving up and dropping the connection. Generous: the CLI
/// client has its own (much shorter) deadline, so this only exists to stop a
/// handler that never settles from pinning a thread forever.
const COMMAND_REPLY_CEILING: Duration = Duration::from_secs(300);

/// A request decoded from a nested client and handed to the editor thread.
enum LocalControlRequest {
    /// Open files in the current window. `wait_id` is `Some` when the
    /// nested client is blocking until the (last) buffer is closed.
    OpenFiles {
        files: Vec<FileRequest>,
        wait_id: Option<u64>,
    },
    /// Open a directory as a new, focused orchestrator window.
    OpenWindow { path: PathBuf },
    /// Evaluate a script on the workspace `token` is bound to. The editor
    /// thread does the work (it owns the plugin manager) and answers on
    /// `reply`, where the parked handler thread writes it to the client
    /// connection.
    RunScript {
        token: Option<String>,
        source: String,
        as_name: Option<String>,
        reply: Sender<ServerControl>,
    },
}

/// Shared state between the connection-handler threads and the editor
/// thread's [`pump`].
struct Shared {
    /// Requests decoded by handler threads, drained each frame by `pump`.
    req_rx: Mutex<Receiver<LocalControlRequest>>,
    /// `wait_id` -> notifier that wakes the parked handler thread once the
    /// editor reports the matching buffer closed.
    waiters: Arc<Mutex<HashMap<u64, Sender<()>>>>,
    /// `request_id` -> the parked handler thread's reply channel, for a script
    /// that is still running. A script settles on the plugin thread well after
    /// submission, so its reply is held here until the outcome arrives.
    command_replies: Arc<Mutex<HashMap<u64, Sender<ServerControl>>>>,
}

/// Process-global handle. `None` until [`start`] succeeds; the control
/// socket is inherently process-wide (one per editor process).
static GLOBAL: OnceLock<Shared> = OnceLock::new();

/// The session id encoded into the control socket filename and advertised
/// to embedded terminals via `FRESH_SESSION`.
static SESSION_ID: OnceLock<String> = OnceLock::new();

/// The session id of this process's local control socket, if active.
///
/// The embedded-terminal spawner reads this to set `FRESH_SESSION` on
/// local child shells so a nested `fresh` forwards back here.
pub fn local_session_id() -> Option<&'static str> {
    SESSION_ID.get().map(String::as_str)
}

/// Bind the local control socket and start accepting nested-forward
/// connections. Best-effort: on failure the editor simply runs without
/// nested forwarding (a nested `fresh` falls back to running inline).
///
/// Returns the session id on success.
pub fn start() -> std::io::Result<&'static str> {
    if let Some(id) = SESSION_ID.get() {
        // Already started (e.g. a defensive double-call). Reuse it.
        return Ok(id.as_str());
    }

    let session_id = generate_session_id();
    let paths = SocketPaths::for_session_name(&session_id)?;
    let bound = bind_and_spawn(paths)?;

    // Records are set last so `local_session_id()` only ever reports a
    // socket that is actually bound and accepting.
    #[allow(clippy::let_underscore_must_use)]
    let _ = GLOBAL.set(Shared {
        req_rx: Mutex::new(bound.req_rx),
        waiters: bound.waiters,
        command_replies: Arc::new(Mutex::new(HashMap::new())),
    });
    let id = SESSION_ID.get_or_init(|| session_id);
    tracing::info!("Local control socket listening as session {}", id);
    Ok(id.as_str())
}

/// The movable pieces produced by [`bind_and_spawn`]: everything the
/// editor thread (or a test) needs to drive an already-listening control
/// socket.
struct BoundControl {
    req_rx: Receiver<LocalControlRequest>,
    waiters: Arc<Mutex<HashMap<u64, Sender<()>>>>,
    /// Set to stop the accept thread. Production (`start`) drops this and lets
    /// the socket live for the whole process; only the tests hold it to shut
    /// the accept thread down deterministically, so it reads as dead outside
    /// `cfg(test)`.
    #[cfg_attr(not(test), allow(dead_code))]
    shutdown: Arc<AtomicBool>,
}

/// Bind the control socket at `paths`, write the pid file, and spawn the
/// accept thread. Shared by [`start`] (real socket) and the tests
/// (isolated temp-dir socket), so neither path special-cases the other.
fn bind_and_spawn(paths: SocketPaths) -> std::io::Result<BoundControl> {
    // Clear any stale leftovers from a crashed predecessor that happened
    // to reuse this name (astronomically unlikely given the pid+nanos id,
    // but cleanup_if_stale is cheap and keeps bind() from failing).
    paths.cleanup_if_stale();

    let listener = ServerListener::bind(paths.clone())?;
    paths.write_pid(std::process::id())?;

    let (req_tx, req_rx) = mpsc::channel::<LocalControlRequest>();
    let waiters: Arc<Mutex<HashMap<u64, Sender<()>>>> = Arc::new(Mutex::new(HashMap::new()));
    let shutdown = Arc::new(AtomicBool::new(false));

    let accept_waiters = waiters.clone();
    let accept_shutdown = shutdown.clone();
    std::thread::Builder::new()
        .name("fresh-local-control".to_string())
        .spawn(move || {
            accept_loop(listener, req_tx, accept_waiters, accept_shutdown);
        })?;

    Ok(BoundControl {
        req_rx,
        waiters,
        shutdown,
    })
}

/// Apply any pending nested-forward requests to `editor` and wake parked
/// connections whose wait completed. Runs once per frame on the editor
/// thread. Cheap no-op when local control isn't active.
///
/// Returns true if anything changed and a re-render is warranted.
pub fn pump(editor: &mut Editor) -> bool {
    let Some(shared) = GLOBAL.get() else {
        return false;
    };
    pump_shared(shared, editor)
}

/// The body of [`pump`], against an explicit [`Shared`] rather than the
/// process-global one — so a test can drive a socket bound by
/// [`bind_and_spawn`] without touching (or racing on) the global.
fn pump_shared(shared: &Shared, editor: &mut Editor) -> bool {
    let mut changed = false;

    // Drain decoded requests. The lock is only contended for the brief
    // moment a handler thread sends; try_recv never blocks.
    loop {
        let req = {
            let rx = shared.req_rx.lock().unwrap();
            rx.try_recv()
        };
        match req {
            Ok(LocalControlRequest::OpenFiles { files, wait_id }) => {
                let last = files.len().saturating_sub(1);
                for (i, fr) in files.into_iter().enumerate() {
                    // Only the last file carries the wait id (it's the one
                    // left active), mirroring the daemon server.
                    let file_wait_id = if i == last { wait_id } else { None };
                    editor.queue_file_open(
                        PathBuf::from(fr.path),
                        fr.line,
                        fr.column,
                        fr.end_line,
                        fr.end_column,
                        fr.message,
                        file_wait_id,
                    );
                }
                // Open them in the window that is active *now*, before any
                // later OpenWindow request in this same batch switches the
                // active window. ("Open a file → current workspace"; only
                // directories spawn a new one.) This also installs the
                // wait_tracking that `take_completed_waits` reports below.
                editor.process_pending_file_opens();
                changed = true;
            }
            Ok(LocalControlRequest::OpenWindow { path }) => {
                if path.is_absolute() {
                    let label = path
                        .file_name()
                        .map(|s| s.to_string_lossy().into_owned())
                        .unwrap_or_else(|| path.to_string_lossy().into_owned());
                    // A directory that already has a session window is
                    // restored by `set_active_window` → `materialize_window`
                    // and keeps its persisted explorer visibility. Only a
                    // brand-new directory gets the fresh-session default,
                    // mirroring a normal `fresh <dir>` launch instead of
                    // leaving the new window on an empty buffer.
                    let known_session = editor.find_window_by_root(&path).is_some();
                    let id = editor.create_window_at(path, label);
                    editor.set_active_window(id);
                    if !known_session {
                        editor.apply_active_window_explorer_default(false, false);
                        editor.relayout();
                    }
                    changed = true;
                } else {
                    tracing::warn!("OpenWindow ignored: path must be absolute: {:?}", path);
                }
            }
            Ok(LocalControlRequest::RunScript {
                token,
                source,
                as_name,
                reply,
            }) => {
                match command_access::run_script(
                    Some(editor),
                    token.as_deref(),
                    &source,
                    as_name.as_deref(),
                ) {
                    // Only a refusal settles here — a script that started
                    // answers for itself.
                    command_access::CommandDispatch::Settled { ok, error, output } => {
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = reply.send(ServerControl::ScriptResult { ok, error, output });
                    }
                    // The script settles on the plugin thread, long after
                    // submission. Park the client's reply channel under its
                    // request id; the drain below answers it once the outcome
                    // arrives, with whatever the script returned.
                    command_access::CommandDispatch::Pending { request_id } => {
                        shared
                            .command_replies
                            .lock()
                            .unwrap()
                            .insert(request_id, reply);
                        changed = true;
                    }
                }
            }
            Err(_) => break,
        }
    }

    // Answer callers whose script has settled since the last pump.
    // Scoped to this host's own waiters: the daemon drains the same queue for
    // its IPC clients, and an unscoped drain would let each swallow the other's
    // outcomes.
    let settled = {
        let waiting = shared.command_replies.lock().unwrap();
        command_access::take_completed_where(|id| waiting.contains_key(&id))
    };
    for outcome in settled {
        let reply = shared
            .command_replies
            .lock()
            .unwrap()
            .remove(&outcome.request_id);
        if let Some(reply) = reply {
            #[allow(clippy::let_underscore_must_use)]
            let _ = reply.send(ServerControl::ScriptResult {
                ok: outcome.ok,
                error: outcome.error,
                output: outcome.output,
            });
        }
    }

    // Route completed waits back to the parked handler threads so they can
    // send WaitComplete and let the blocked nested `fresh` (e.g. the editor
    // git launched) exit.
    let completed = editor.take_completed_waits();
    if !completed.is_empty() {
        let mut waiters = shared.waiters.lock().unwrap();
        for wait_id in completed {
            if let Some(notifier) = waiters.remove(&wait_id) {
                #[allow(clippy::let_underscore_must_use)]
                let _ = notifier.send(());
            }
        }
    }

    changed
}

/// Accept thread: poll for new connections and spawn a handler per
/// connection so a parked (`--wait`) connection never blocks accepting
/// the next one.
fn accept_loop(
    mut listener: ServerListener,
    req_tx: Sender<LocalControlRequest>,
    waiters: Arc<Mutex<HashMap<u64, Sender<()>>>>,
    shutdown: Arc<AtomicBool>,
) {
    let next_wait_id = Arc::new(AtomicU64::new(1));

    while !shutdown.load(Ordering::SeqCst) {
        match listener.accept() {
            Ok(Some(conn)) => {
                let req_tx = req_tx.clone();
                let waiters = waiters.clone();
                let next_wait_id = next_wait_id.clone();
                // Best-effort: a failed handler spawn just drops the
                // connection; the nested client falls back to inline.
                #[allow(clippy::let_underscore_must_use)]
                let _ = std::thread::Builder::new()
                    .name("fresh-local-control-conn".to_string())
                    .spawn(move || {
                        handle_connection(conn, req_tx, waiters, next_wait_id);
                    });
            }
            Ok(None) => std::thread::sleep(ACCEPT_POLL_INTERVAL),
            Err(e) => {
                tracing::warn!("Local control accept error: {}", e);
                std::thread::sleep(ACCEPT_POLL_INTERVAL);
            }
        }
    }
}

/// Per-connection handler: handshake, decode one command, forward it to
/// the editor thread, and (for waited opens) park until the buffer closes.
fn handle_connection(
    conn: ServerConnection,
    req_tx: Sender<LocalControlRequest>,
    waiters: Arc<Mutex<HashMap<u64, Sender<()>>>>,
    next_wait_id: Arc<AtomicU64>,
) {
    // A dedicated blocking thread, so block on reads. One BufReader for the
    // whole connection: the client sends several commands back-to-back
    // (one OpenWindow per directory, then a final OpenFiles), and a fresh
    // reader per message would drop lines already buffered from the socket.
    let mut reader = std::io::BufReader::new(&conn.control);

    // The capability token (from the client's `Hello`) is captured once and
    // reused for every `RunScript` on this connection.
    let cmd_token = match handshake(&conn, &mut reader) {
        Ok(token) => token,
        Err(e) => {
            tracing::debug!("Local control handshake failed: {}", e);
            return;
        }
    };

    loop {
        // Re-assert blocking mode before every read. `accept()` leaves the
        // socket non-blocking, and `ServerConnection::write_control` (shared
        // with the poll-driven daemon server) flips it back to non-blocking
        // after each reply — so without this a blocking `read_line` would
        // return `WouldBlock` immediately, drop the connection, and leave the
        // nested client to fall back to inline. We read blocking here.
        #[cfg(not(windows))]
        #[allow(clippy::let_underscore_must_use)]
        let _ = conn.control.set_nonblocking(false);

        let msg = match read_msg(&mut reader) {
            Ok(Some(m)) => m,
            Ok(None) => return, // client disconnected
            Err(e) => {
                tracing::debug!("Local control read error: {}", e);
                return;
            }
        };

        match msg {
            ClientControl::OpenWindow { path } => {
                #[allow(clippy::let_underscore_must_use)]
                let _ = req_tx.send(LocalControlRequest::OpenWindow {
                    path: PathBuf::from(path),
                });
            }
            ClientControl::OpenFiles { files, wait } => {
                // Register a waiter *before* handing the request to the
                // editor so the completion notification can't race ahead.
                let wait_slot = if wait {
                    let id = next_wait_id.fetch_add(1, Ordering::SeqCst);
                    let (tx, rx) = mpsc::channel::<()>();
                    waiters.lock().unwrap().insert(id, tx);
                    Some((id, rx))
                } else {
                    None
                };

                #[allow(clippy::let_underscore_must_use)]
                let _ = req_tx.send(LocalControlRequest::OpenFiles {
                    files,
                    wait_id: wait_slot.as_ref().map(|(id, _)| *id),
                });

                if let Some((id, rx)) = wait_slot {
                    // Block until `pump` reports the buffer closed. A recv
                    // error means the editor exited first — treat that as
                    // completion so the nested process is never wedged.
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = rx.recv();
                    let done =
                        serde_json::to_string(&ServerControl::WaitComplete).unwrap_or_default();
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = conn.write_control(&done);
                    waiters.lock().unwrap().remove(&id);
                }
            }
            ClientControl::RunScript { source, as_name } => {
                // Round-trip to the editor thread (it owns the plugin manager)
                // and relay its answer back to the client.
                let (reply_tx, reply_rx) = mpsc::channel::<ServerControl>();
                #[allow(clippy::let_underscore_must_use)]
                let _ = req_tx.send(LocalControlRequest::RunScript {
                    token: cmd_token.clone(),
                    source,
                    as_name,
                    reply: reply_tx,
                });
                // Bounded, unlike the other requests: a script could in
                // principle never settle, and an unbounded wait would leak this
                // thread (and the client's connection) for the life of the
                // editor. The client gives up long before this; the ceiling is
                // only a backstop.
                if let Ok(reply) = reply_rx.recv_timeout(COMMAND_REPLY_CEILING) {
                    let json = serde_json::to_string(&reply).unwrap_or_default();
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = conn.write_control(&json);
                }
            }
            other => {
                tracing::debug!("Local control ignoring unexpected message: {:?}", other);
            }
        }
    }
}

/// Read the `Hello`, version-check, and reply with `ServerHello` — the
/// same shape as the daemon server's handshake, minus the data-channel
/// terminal setup (nested clients never render). Returns the client's
/// capability token (`cmd_token` from the `Hello`, `None` if absent) so the
/// command loop can authorize `RunScript`.
///
/// Shares the connection's [`BufReader`] with the post-handshake command
/// loop so no buffered bytes are lost between the two.
fn handshake(
    conn: &ServerConnection,
    reader: &mut std::io::BufReader<&StreamWrapper>,
) -> std::io::Result<Option<String>> {
    // `accept()` hands us a non-blocking control socket; the Hello read must
    // block until the client sends it (see the command loop for the same
    // reason `write_control` requires re-asserting this).
    #[cfg(not(windows))]
    conn.control.set_nonblocking(false)?;

    let hello_json = read_msg(reader)?
        .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::UnexpectedEof, "no hello"))?;

    let hello = match hello_json {
        ClientControl::Hello(h) => h,
        _ => return Err(std::io::Error::other("expected Hello")),
    };

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
            .map_err(std::io::Error::other)?;
        conn.write_control(&response)?;
        return Err(std::io::Error::other("version mismatch"));
    }

    let session_id = local_session_id().unwrap_or("local").to_string();
    let response = serde_json::to_string(&ServerControl::Hello(ServerHello::new(session_id)))
        .map_err(std::io::Error::other)?;
    conn.write_control(&response)?;
    Ok(hello.cmd_token)
}

/// Read one newline-delimited control message from a shared connection
/// reader, parsed as a [`ClientControl`]. `Ok(None)` signals EOF (client
/// disconnected).
fn read_msg(
    reader: &mut std::io::BufReader<&StreamWrapper>,
) -> std::io::Result<Option<ClientControl>> {
    let mut line = String::new();
    match reader.read_line(&mut line) {
        Ok(0) => Ok(None),
        Ok(_) => {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                return Ok(None);
            }
            let msg = serde_json::from_str::<ClientControl>(trimmed)
                .map_err(|e| std::io::Error::other(format!("invalid control message: {}", e)))?;
            Ok(Some(msg))
        }
        Err(e) => Err(e),
    }
}

/// A unique-per-process session id: `local-<pid>-<nanos>`. The nanosecond
/// component guards against pid reuse across quick successive runs.
///
/// The clock component is truncated to its low 40 bits and printed in hex (10
/// chars instead of 19). The id becomes a socket *filename* under the runtime
/// dir, and a Unix `sockaddr_un` path is capped at ~108 bytes — a shorter name
/// leaves that much more headroom for the runtime dir itself before `bind`
/// fails outright. 40 bits of nanoseconds still spans ~18 minutes, far longer
/// than the window in which a pid could plausibly be recycled.
fn generate_session_id() -> String {
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    format!(
        "local-{}-{:x}",
        std::process::id(),
        (nanos as u64) & 0xFF_FFFF_FFFF
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::server::ipc::ClientConnection;
    use crate::server::protocol::{ClientHello, TermSize};
    use tempfile::TempDir;

    /// Connect a thin client and complete the handshake, returning the
    /// live connection ready to send a command.
    fn connect_client(paths: &SocketPaths) -> ClientConnection {
        let conn = ClientConnection::connect(paths).expect("client connect");
        let hello = ClientHello::new(TermSize::new(80, 24));
        conn.write_control(&serde_json::to_string(&ClientControl::Hello(hello)).unwrap())
            .expect("write hello");
        let resp = conn
            .read_control()
            .expect("read server hello")
            .expect("server hello present");
        match serde_json::from_str::<ServerControl>(&resp).expect("parse server hello") {
            ServerControl::Hello(_) => {}
            other => panic!("expected ServerControl::Hello, got {:?}", other),
        }
        conn
    }

    fn file_req(path: &str) -> FileRequest {
        FileRequest {
            path: path.to_string(),
            line: None,
            column: None,
            end_line: None,
            end_column: None,
            message: None,
        }
    }

    /// A directory argument is forwarded as an `OpenWindow` request that
    /// the editor thread can drain. Without the local-control listener
    /// there is no socket to forward to.
    #[test]
    fn open_window_request_is_received() {
        let dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("open-window-test", dir.path());
        let bound = bind_and_spawn(paths.clone()).expect("bind");

        let conn = connect_client(&paths);
        let msg = ClientControl::OpenWindow {
            path: "/abs/project".to_string(),
        };
        conn.write_control(&serde_json::to_string(&msg).unwrap())
            .unwrap();

        // recv() blocks until the handler thread forwards the decoded
        // request — no fixed timeout (the external runner bounds the test).
        match bound.req_rx.recv().expect("request forwarded") {
            LocalControlRequest::OpenWindow { path } => {
                assert_eq!(path, PathBuf::from("/abs/project"));
            }
            other => panic!("expected OpenWindow, got {:?}", req_kind(&other)),
        }

        bound.shutdown.store(true, Ordering::SeqCst);
    }

    /// A waited file open forwards an `OpenFiles` request carrying a
    /// `wait_id`, and the client stays blocked until the editor thread
    /// reports the wait complete — at which point it receives
    /// `WaitComplete`. This is the `git commit` / `$EDITOR` path.
    #[test]
    fn waited_open_files_completes_after_signal() {
        let dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("wait-open-test", dir.path());
        let bound = bind_and_spawn(paths.clone()).expect("bind");

        let conn = connect_client(&paths);
        let msg = ClientControl::OpenFiles {
            files: vec![file_req("/abs/COMMIT_EDITMSG")],
            wait: true,
        };
        conn.write_control(&serde_json::to_string(&msg).unwrap())
            .unwrap();

        let wait_id = match bound.req_rx.recv().expect("request forwarded") {
            LocalControlRequest::OpenFiles { files, wait_id } => {
                assert_eq!(files.len(), 1);
                assert_eq!(files[0].path, "/abs/COMMIT_EDITMSG");
                wait_id.expect("wait id assigned for waited open")
            }
            other => panic!("expected OpenFiles, got {:?}", req_kind(&other)),
        };

        // Simulate what `pump` does when the buffer is closed: wake the
        // parked handler via its registered waiter.
        let notifier = bound
            .waiters
            .lock()
            .unwrap()
            .remove(&wait_id)
            .expect("waiter registered before request was forwarded");
        notifier.send(()).unwrap();

        // The client must now observe WaitComplete and unblock.
        let line = conn
            .read_control()
            .expect("read wait complete")
            .expect("wait complete present");
        match serde_json::from_str::<ServerControl>(&line).expect("parse") {
            ServerControl::WaitComplete => {}
            other => panic!("expected WaitComplete, got {:?}", other),
        }

        bound.shutdown.store(true, Ordering::SeqCst);
    }

    /// A non-waited open forwards the request and does not register a
    /// waiter (the client returns immediately).
    #[test]
    fn unwaited_open_files_has_no_wait_id() {
        let dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("nowait-open-test", dir.path());
        let bound = bind_and_spawn(paths.clone()).expect("bind");

        let conn = connect_client(&paths);
        let msg = ClientControl::OpenFiles {
            files: vec![file_req("/abs/file.txt")],
            wait: false,
        };
        conn.write_control(&serde_json::to_string(&msg).unwrap())
            .unwrap();

        match bound.req_rx.recv().expect("request forwarded") {
            LocalControlRequest::OpenFiles { wait_id, .. } => {
                assert!(wait_id.is_none());
            }
            other => panic!("expected OpenFiles, got {:?}", req_kind(&other)),
        }
        assert!(bound.waiters.lock().unwrap().is_empty());

        bound.shutdown.store(true, Ordering::SeqCst);
    }

    /// `fresh dirA dirB file.txt` sends several commands back-to-back on a
    /// single connection. All must be forwarded — a handler that reads only
    /// one message (or a fresh reader per message that drops buffered
    /// lines) would lose every command after the first.
    #[test]
    fn multiple_commands_on_one_connection_all_received() {
        let dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("multi-cmd-test", dir.path());
        let bound = bind_and_spawn(paths.clone()).expect("bind");

        let conn = connect_client(&paths);
        let msgs = [
            ClientControl::OpenWindow {
                path: "/abs/a".to_string(),
            },
            ClientControl::OpenWindow {
                path: "/abs/b".to_string(),
            },
            ClientControl::OpenFiles {
                files: vec![file_req("/abs/file.txt")],
                wait: false,
            },
        ];
        for m in &msgs {
            conn.write_control(&serde_json::to_string(m).unwrap())
                .unwrap();
        }

        let r1 = bound.req_rx.recv().expect("first request");
        let r2 = bound.req_rx.recv().expect("second request");
        let r3 = bound.req_rx.recv().expect("third request");

        match r1 {
            LocalControlRequest::OpenWindow { path } => assert_eq!(path, PathBuf::from("/abs/a")),
            other => panic!("expected OpenWindow a, got {}", req_kind(&other)),
        }
        match r2 {
            LocalControlRequest::OpenWindow { path } => assert_eq!(path, PathBuf::from("/abs/b")),
            other => panic!("expected OpenWindow b, got {}", req_kind(&other)),
        }
        match r3 {
            LocalControlRequest::OpenFiles { files, wait_id } => {
                assert_eq!(files[0].path, "/abs/file.txt");
                assert!(wait_id.is_none());
            }
            other => panic!("expected OpenFiles, got {}", req_kind(&other)),
        }

        bound.shutdown.store(true, Ordering::SeqCst);
    }

    /// Regression test for the blocking-mode bug: a real nested `fresh` sends
    /// its command some time *after* the handshake completes (process startup
    /// + socket round-trip), so the server's post-handshake read runs against
    /// an empty socket. `accept()` leaves the socket non-blocking and
    /// `write_control` flips it back to non-blocking after the `ServerHello`,
    /// so without re-asserting blocking mode the handler's `read_line` would
    /// return `WouldBlock`, drop the connection, and the command would never
    /// be forwarded (the client would then fall back to opening inline).
    ///
    /// The earlier tests sent their command immediately, so the bytes were
    /// usually already buffered when the read ran — masking the bug. The
    /// explicit delay here makes the empty-socket read deterministic.
    #[test]
    fn command_sent_after_a_delay_is_still_received() {
        let dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("delayed-cmd-test", dir.path());
        let bound = bind_and_spawn(paths.clone()).expect("bind");

        let conn = connect_client(&paths);
        // Long enough that the server has finished the handshake and is
        // parked in the command-loop read before the command arrives.
        std::thread::sleep(Duration::from_millis(150));
        let msg = ClientControl::OpenWindow {
            path: "/abs/delayed".to_string(),
        };
        conn.write_control(&serde_json::to_string(&msg).unwrap())
            .unwrap();

        // Bounded so a regressed (connection-dropping) build fails fast
        // instead of hanging forever on a request that never arrives.
        match bound
            .req_rx
            .recv_timeout(Duration::from_secs(5))
            .expect("delayed request must still be forwarded")
        {
            LocalControlRequest::OpenWindow { path } => {
                assert_eq!(path, PathBuf::from("/abs/delayed"));
            }
            other => panic!("expected OpenWindow, got {}", req_kind(&other)),
        }

        bound.shutdown.store(true, Ordering::SeqCst);
    }

    fn req_kind(req: &LocalControlRequest) -> &'static str {
        match req {
            LocalControlRequest::OpenFiles { .. } => "OpenFiles",
            LocalControlRequest::OpenWindow { .. } => "OpenWindow",
            LocalControlRequest::RunScript { .. } => "RunScript",
        }
    }

    /// Connect a thin client, completing the handshake with a `cmd_token`
    /// stamped into the `Hello` (as a workspace-spawned agent would).
    fn connect_client_with_token(paths: &SocketPaths, token: &str) -> ClientConnection {
        let conn = ClientConnection::connect(paths).expect("client connect");
        let mut hello = ClientHello::new(TermSize::new(80, 24));
        hello.cmd_token = Some(token.to_string());
        conn.write_control(&serde_json::to_string(&ClientControl::Hello(hello)).unwrap())
            .expect("write hello");
        let resp = conn
            .read_control()
            .expect("read server hello")
            .expect("server hello present");
        match serde_json::from_str::<ServerControl>(&resp).expect("parse server hello") {
            ServerControl::Hello(_) => {}
            other => panic!("expected ServerControl::Hello, got {:?}", other),
        }
        conn
    }

    /// A `RunScript` is forwarded to the editor thread carrying the
    /// connection's `cmd_token` (captured from the `Hello`) and its source —
    /// the thread that owns the editor is the only place it can be authorized
    /// and evaluated, so the handler round-trips it there.
    #[test]
    fn run_script_forwards_request_with_token() {
        let dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("run-script-token-test", dir.path());
        let bound = bind_and_spawn(paths.clone()).expect("bind");

        let conn = connect_client_with_token(&paths, "tok-123");
        let msg = ClientControl::RunScript {
            source: "return 1;".to_string(),
            as_name: None,
        };
        conn.write_control(&serde_json::to_string(&msg).unwrap())
            .unwrap();

        match bound.req_rx.recv().expect("request forwarded") {
            LocalControlRequest::RunScript { token, source, .. } => {
                assert_eq!(token.as_deref(), Some("tok-123"));
                assert_eq!(source, "return 1;");
            }
            other => panic!("expected RunScript, got {}", req_kind(&other)),
        }

        bound.shutdown.store(true, Ordering::SeqCst);
    }

    /// Build a `Shared` over an already-bound control socket, so a test can run
    /// the editor-thread half (`pump_shared`) exactly as a host loop does.
    fn shared_for(bound: BoundControl) -> Shared {
        Shared {
            req_rx: Mutex::new(bound.req_rx),
            waiters: bound.waiters,
            command_replies: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// An editor for the pump half of the round trip. Plugins stay off: the
    /// commands under test are built-ins, and a plugin runtime would only add
    /// startup cost.
    fn pump_test_editor() -> (Editor, TempDir) {
        let temp = TempDir::new().unwrap();
        let dir_context = crate::config_io::DirectoryContext::for_testing(temp.path());
        let editor = Editor::new(
            crate::config::Config::default(),
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            std::sync::Arc::new(crate::model::filesystem::StdFileSystem),
        )
        .expect("editor");
        (editor, temp)
    }

    /// The full `fresh --cmd script run` round trip against a host that drains
    /// the channel: the request reaches the editor thread, `pump` authorizes it,
    /// and the client reads a `ScriptResult` back.
    ///
    /// The token deliberately carries *no* script grant, so the answer is a
    /// refusal decided on the editor thread. That keeps the test about the one
    /// thing it guards — that a queued request gets answered — and keeps it
    /// deterministic: a granted token would hand the source to the plugin
    /// runtime, whose completion is drained by the real editor loop and not by
    /// `pump_shared`, so the reply would never arrive here and the test would
    /// hang (as it did on macOS, where the runtime is live, while passing on
    /// runners where it wasn't).
    ///
    /// Regression: only the TUI loop and the standalone web bridge pumped. The
    /// GUI tick and the session daemon (what `fresh --web` and every attached
    /// terminal run) did not, so the request sat in the queue, the handler
    /// thread parked on a reply that was never produced, and the CLI hung
    /// forever in the agent's terminal.
    #[test]
    fn script_is_answered_when_the_host_pumps() {
        let dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("script-pump-test", dir.path());
        let bound = bind_and_spawn(paths.clone()).expect("bind");
        let shutdown = bound.shutdown.clone();
        let shared = shared_for(bound);

        let token = command_access::mint(command_access::Grant::new(None, false));

        // The client runs on its own thread with a blocking read, exactly like
        // the real CLI; this thread plays the host loop below.
        let (done_tx, done_rx) = mpsc::channel::<(bool, Option<String>)>();
        let client_paths = paths.clone();
        let client_token = token.clone();
        let client = std::thread::spawn(move || {
            let conn = connect_client_with_token(&client_paths, &client_token);
            conn.write_control(
                &serde_json::to_string(&ClientControl::RunScript {
                    source: "return editor.activeWindow();".to_string(),
                    as_name: None,
                })
                .unwrap(),
            )
            .unwrap();
            let line = conn
                .read_control()
                .expect("read reply")
                .expect("reply present");
            let got = match serde_json::from_str::<ServerControl>(&line).expect("parse reply") {
                ServerControl::ScriptResult { ok, error, .. } => (ok, error),
                other => panic!("expected ScriptResult, got {:?}", other),
            };
            #[allow(clippy::let_underscore_must_use)]
            let _ = done_tx.send(got);
        });

        // Host loop: pump until the client reports, or give up. The deadline is
        // the assertion — with the regression no reply is ever produced, and a
        // bounded wait reports that as a failure here instead of hanging until
        // the outer test runner kills the whole suite.
        let (mut editor, _temp) = pump_test_editor();
        let deadline = std::time::Instant::now() + Duration::from_secs(10);
        let mut answer = None;
        while std::time::Instant::now() < deadline {
            pump_shared(&shared, &mut editor);
            if let Ok(got) = done_rx.try_recv() {
                answer = Some(got);
                break;
            }
            std::thread::sleep(Duration::from_millis(5));
        }
        let (ok, error) = answer.expect("a pumping host must answer a queued RunScript");

        assert!(!ok, "a token without the script grant must be refused");
        assert!(
            error.unwrap_or_default().contains("not granted"),
            "the refusal must name the missing grant"
        );

        client.join().unwrap();
        command_access::revoke(&token);
        shutdown.store(true, Ordering::SeqCst);
    }

    /// A host that accepts the connection but never drains the channel (the
    /// regression above, and any older editor build an agent may be talking to)
    /// must not park the client forever: the CLI's deadline-bounded read gives
    /// up with `TimedOut` so the agent gets a diagnosable error instead of a
    /// hang. The short deadline here *is* the unit under test — no reply can
    /// ever arrive, so there is nothing to race with.
    #[test]
    fn client_read_times_out_when_the_host_never_pumps() {
        let dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("script-nopump-test", dir.path());
        // Note: no `pump` — `bound` holds the receiving end and never drains it.
        let bound = bind_and_spawn(paths.clone()).expect("bind");

        let conn = connect_client(&paths);
        conn.write_control(
            &serde_json::to_string(&ClientControl::RunScript {
                source: "return 1;".to_string(),
                as_name: None,
            })
            .unwrap(),
        )
        .unwrap();

        let err = conn
            .control_reader()
            .read_line_timeout(Duration::from_millis(100))
            .expect_err("a host that never answers must not block the client forever");
        assert_eq!(err.kind(), std::io::ErrorKind::TimedOut);

        bound.shutdown.store(true, Ordering::SeqCst);
    }

    /// A client that presents no token still forwards the request, but with
    /// `token: None`; the editor-thread authorization then refuses it. This
    /// asserts the no-token path is wired (the refusal itself is unit-tested
    /// against `command_access::run_script`).
    #[test]
    fn run_script_without_token_forwards_none() {
        let dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("run-script-notoken-test", dir.path());
        let bound = bind_and_spawn(paths.clone()).expect("bind");

        let conn = connect_client(&paths);
        let msg = ClientControl::RunScript {
            source: "return 1;".to_string(),
            as_name: None,
        };
        conn.write_control(&serde_json::to_string(&msg).unwrap())
            .unwrap();

        match bound.req_rx.recv().expect("request forwarded") {
            LocalControlRequest::RunScript { token, .. } => {
                assert!(token.is_none());
            }
            other => panic!("expected RunScript, got {}", req_kind(&other)),
        }

        bound.shutdown.store(true, Ordering::SeqCst);
    }
}
