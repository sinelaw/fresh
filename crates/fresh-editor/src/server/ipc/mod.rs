//! IPC infrastructure for client-server communication
//!
//! Uses the `interprocess` crate for cross-platform local sockets:
//! - Unix domain sockets on Linux/macOS
//! - Named pipes on Windows
//!
//! Each daemon has two sockets: data (byte stream) and control (JSON messages).

use std::io::{self, BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use interprocess::local_socket::{
    prelude::*, Listener, ListenerNonblockingMode, ListenerOptions, Stream,
};

use crate::server::protocol::{
    ClientControl, ClientHello, ServerControl, VersionMismatch, PROTOCOL_VERSION,
};
use crate::workspace::encode_path_for_filename;

// Platform-specific implementations
#[cfg(unix)]
mod platform_unix;
#[cfg(windows)]
mod platform_windows;

#[cfg(unix)]
use platform_unix as platform;
#[cfg(windows)]
use platform_windows as platform;

/// Socket paths for a daemon
#[derive(Debug, Clone)]
pub struct SocketPaths {
    /// Data socket path (raw byte stream)
    pub data: PathBuf,
    /// Control socket path (JSON messages)
    pub control: PathBuf,
    /// PID file path (for detecting stale daemons)
    pub pid: PathBuf,
    /// Lock file path: whoever holds the lock on it owns the session (see
    /// [`ServerListener::bind`]). Never removed, unlike the files above.
    pub lock: PathBuf,
}

/// Whether a session's server is running, as far as *this* process can tell.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ServerLiveness {
    /// The server answered, or its process is visibly running.
    Alive,
    /// Positively not running: no pid and nothing listening.
    Dead,
    /// The socket is present but this process may not reach it — typically a
    /// sandbox denying `connect(2)`. Says nothing about the server itself.
    Unreachable,
}

impl SocketPaths {
    /// Get the socket directory
    pub fn socket_directory() -> io::Result<PathBuf> {
        platform::get_socket_dir()
    }

    /// Get socket paths for a working directory
    pub fn for_working_dir(working_dir: &Path) -> io::Result<Self> {
        let socket_dir = platform::get_socket_dir()?;
        let encoded = encode_path_for_filename(working_dir);
        Ok(Self::for_session_name_in_dir(&encoded, &socket_dir))
    }

    /// Get socket paths for a named daemon
    pub fn for_session_name(name: &str) -> io::Result<Self> {
        let socket_dir = platform::get_socket_dir()?;
        Ok(Self::for_session_name_in_dir(name, &socket_dir))
    }

    /// Get socket paths for a named daemon in a specific directory
    /// (primarily for testing with isolated temp directories)
    pub fn for_session_name_in_dir(name: &str, socket_dir: &Path) -> Self {
        let stem = socket_stem(name, socket_dir, platform::max_socket_path_len());
        Self {
            data: socket_dir.join(format!("{}.data.sock", stem)),
            control: socket_dir.join(format!("{}.ctrl.sock", stem)),
            pid: socket_dir.join(format!("{}.pid", stem)),
            lock: socket_dir.join(format!("{}.lock", stem)),
        }
    }

    /// Check if the sockets exist (server might be running)
    pub fn exists(&self) -> bool {
        self.data.exists() && self.control.exists()
    }

    /// Write the server PID to the PID file
    pub fn write_pid(&self, pid: u32) -> io::Result<()> {
        std::fs::write(&self.pid, pid.to_string())
    }

    /// Read the server PID from the PID file
    pub fn read_pid(&self) -> io::Result<Option<u32>> {
        if !self.pid.exists() {
            return Ok(None);
        }
        let content = std::fs::read_to_string(&self.pid)?;
        Ok(content.trim().parse().ok())
    }

    /// Check if the server process is still alive
    pub fn is_server_alive(&self) -> bool {
        matches!(self.probe_server(), ServerLiveness::Alive)
    }

    /// Liveness of the session's server, keeping "cannot tell" separate from
    /// "not running".
    ///
    /// Both signals can fail for reasons that have nothing to do with the
    /// editor: a caller in a PID namespace cannot see the editor's pid, and a
    /// sandboxed caller may be denied `connect(2)` on a socket that lives
    /// outside its writable roots. Reporting either as "dead" is what makes a
    /// sandboxed agent chase a phantom stale session — and, worse, invites
    /// `cleanup_if_stale` to unlink the *live* editor's sockets.
    pub fn probe_server(&self) -> ServerLiveness {
        use crate::server::daemon::is_process_running;

        // Check PID file - this is the reliable method
        if let Ok(Some(pid)) = self.read_pid() {
            if is_process_running(pid) {
                return ServerLiveness::Alive;
            }
        }

        // Platform-specific fallback check
        if self.exists() {
            return match platform::probe_server_by_connect(&self.control) {
                platform::ConnectProbe::Alive => ServerLiveness::Alive,
                platform::ConnectProbe::Blocked => ServerLiveness::Unreachable,
                platform::ConnectProbe::Refused => ServerLiveness::Dead,
            };
        }

        ServerLiveness::Dead
    }

    /// Clean up stale daemon files if the daemon is not running
    /// Returns true if files were cleaned up
    ///
    /// Only a positive `Dead` verdict may delete anything: an `Unreachable`
    /// probe means we were denied the answer, not given a negative one.
    pub fn cleanup_if_stale(&self) -> bool {
        if self.exists() && self.probe_server() == ServerLiveness::Dead {
            // Best-effort cleanup of stale socket files
            #[allow(clippy::let_underscore_must_use)]
            let _ = self.cleanup();
            true
        } else {
            false
        }
    }

    /// Remove socket and PID files (cleanup)
    ///
    /// Not the lock file: removing it while a server holds its lock would
    /// let the next one lock a new file at the same path and take the
    /// session from under it.
    pub fn cleanup(&self) -> io::Result<()> {
        if self.data.exists() {
            std::fs::remove_file(&self.data)?;
        }
        if self.control.exists() {
            std::fs::remove_file(&self.control)?;
        }
        if self.pid.exists() {
            std::fs::remove_file(&self.pid)?;
        }
        Ok(())
    }
}

/// The longest suffix a session's files carry; every other one is shorter.
const LONGEST_SUFFIX: &str = ".data.sock";

/// The file-name stem for a session's sockets in `socket_dir`.
///
/// A Unix socket's whole path has to fit `sun_path` (`max_path_len`), and a
/// working-directory session's name is the entire encoded directory, so a
/// deep project left `bind(2)` nothing to work with. A name that fits keeps
/// exactly the stem it always had — a daemon an older Fresh started is still
/// found — and only one that would not fit is folded to `<head>~<hash>`
/// (see [`fold_to_len`]): deterministic, so every `fresh -a` in a directory
/// meets the same daemon, and unique, since the hash covers the full name.
///
/// A folded stem is a fixed point: it already fits, so passing it back in as
/// a session name — as `daemon list` does with each `*.ctrl.sock` it finds —
/// names the same files.
///
/// [`fold_to_len`]: fresh_editor_core::path_encode::fold_to_len
fn socket_stem(name: &str, socket_dir: &Path, max_path_len: Option<usize>) -> String {
    let Some(max_path_len) = max_path_len else {
        return name.to_string();
    };
    // `<socket_dir>/<stem>.data.sock`
    let overhead = socket_dir.as_os_str().len() + 1 + LONGEST_SUFFIX.len();
    fresh_editor_core::path_encode::fold_to_len(name, max_path_len.saturating_sub(overhead))
}

/// Type alias for interprocess local socket stream
type LocalStream = Stream;
type LocalListener = Listener;

/// Server listener for accepting client connections
pub struct ServerListener {
    data_listener: LocalListener,
    control_listener: LocalListener,
    paths: SocketPaths,
    /// The session's lock, held for as long as this listener lives; `None`
    /// where the filesystem cannot lock (see [`lock_session`]).
    _session_lock: Option<std::fs::File>,
    /// Control streams accepted but not yet matched to a data stream
    pending_controls: Vec<PendingControl>,
    /// Data streams accepted but not yet matched to a control stream
    pending_data: Vec<PendingData>,
}

/// What a client sends first on its data stream: this, then its pair token.
const PAIR_MAGIC: &[u8] = b"fresh-pair:";
/// A pair token: 32 hex digits (see [`new_pair_token`]).
const PAIR_TOKEN_LEN: usize = 32;
const PAIR_PREAMBLE_LEN: usize = PAIR_MAGIC.len() + PAIR_TOKEN_LEN;
/// The longest Hello line a pending control stream may send.
const MAX_HELLO_LEN: usize = 64 * 1024;
/// How many unmatched streams of each kind are kept; past that the oldest is
/// dropped. Each is a client that connected one stream and has not finished
/// connecting or sending its first bytes: a handful at most, normally none.
const MAX_PENDING: usize = 32;

/// A token unique among this host's live connections: the process id, the
/// time, and a per-process count. Nothing secret rides on it — only a
/// process that can already open the sockets ever sees one.
fn new_pair_token() -> String {
    use std::sync::atomic::{AtomicU32, Ordering};
    static COUNTER: AtomicU32 = AtomicU32::new(0);
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0);
    format!(
        "{:08x}{:016x}{:08x}",
        std::process::id(),
        nanos,
        COUNTER.fetch_add(1, Ordering::Relaxed)
    )
}

/// What reading a pending stream came to.
enum Progress {
    /// More bytes are needed
    Waiting,
    /// The stream has sent its first message (or preamble) in full
    Ready,
    /// Closed, broken, or sending too much: drop it
    Drop,
}

/// Read what a pending stream has sent so far into `buf`, up to `limit`
/// bytes in all and never past `stop` (a byte that ends the message), one
/// byte at a time so nothing after the message is consumed: those bytes are
/// the connection's, for whoever reads it next.
fn read_pending(
    stream: &StreamWrapper,
    buf: &mut Vec<u8>,
    limit: usize,
    stop: Option<u8>,
) -> Progress {
    let mut byte = [0u8; 1];
    while buf.len() < limit {
        match stream.try_read(&mut byte) {
            Ok(0) => return Progress::Drop,
            Ok(_) => {
                buf.push(byte[0]);
                if stop == Some(byte[0]) {
                    return Progress::Ready;
                }
            }
            Err(e)
                if e.kind() == io::ErrorKind::WouldBlock
                    || e.kind() == io::ErrorKind::Interrupted =>
            {
                return Progress::Waiting
            }
            Err(_) => return Progress::Drop,
        }
    }
    if stop.is_some() {
        // A line longer than any Hello
        Progress::Drop
    } else {
        Progress::Ready
    }
}

/// A control stream waiting for its client's Hello, and then for its data
/// stream.
struct PendingControl {
    stream: StreamWrapper,
    line: Vec<u8>,
    /// The client's Hello and its pair token, once read
    hello: Option<(ClientHello, String)>,
}

impl PendingControl {
    /// Read on toward the Hello; `false` to drop this stream.
    fn advance(&mut self) -> bool {
        if self.hello.is_some() {
            return true;
        }
        match read_pending(&self.stream, &mut self.line, MAX_HELLO_LEN, Some(b'\n')) {
            Progress::Waiting => true,
            Progress::Drop => false,
            Progress::Ready => {
                self.hello = self.take_hello();
                self.hello.is_some()
            }
        }
    }

    /// The Hello in the line just read and its pair token, or `None` for a
    /// client this server cannot pair. One speaking another protocol version
    /// is told so (it cannot read anything else), as is one that sent no
    /// pair token.
    fn take_hello(&self) -> Option<(ClientHello, String)> {
        let hello = match serde_json::from_slice::<ClientControl>(&self.line) {
            Ok(ClientControl::Hello(hello)) => hello,
            Ok(_) | Err(_) => {
                tracing::debug!("Dropping a connection that did not open with a Hello");
                return None;
            }
        };
        let reply = if hello.protocol_version != PROTOCOL_VERSION {
            ServerControl::VersionMismatch(VersionMismatch::for_client(&hello))
        } else {
            match hello.pair_token.clone() {
                Some(token) => return Some((hello, token)),
                None => ServerControl::Error {
                    message: "Hello carries no pair token".to_string(),
                },
            }
        };
        tracing::debug!("Refusing a client: {reply:?}");
        if let Ok(json) = serde_json::to_string(&reply) {
            // Best-effort: the stream is dropped either way
            #[allow(clippy::let_underscore_must_use)]
            let _ = write_line(&self.stream, &json);
        }
        None
    }

    fn token(&self) -> Option<&str> {
        self.hello.as_ref().map(|(_, token)| token.as_str())
    }
}

/// A data stream waiting for its pair token, and then for its control stream.
struct PendingData {
    stream: StreamWrapper,
    preamble: Vec<u8>,
    token: Option<String>,
}

impl PendingData {
    /// Read on toward the pair token; `false` to drop this stream.
    fn advance(&mut self) -> bool {
        if self.token.is_some() {
            return true;
        }
        match read_pending(&self.stream, &mut self.preamble, PAIR_PREAMBLE_LEN, None) {
            Progress::Waiting => true,
            Progress::Drop => false,
            Progress::Ready => {
                let Some(token) = self.preamble.strip_prefix(PAIR_MAGIC) else {
                    tracing::debug!("Dropping a data stream that did not open with a pair token");
                    return false;
                };
                if !token.iter().all(u8::is_ascii_hexdigit) {
                    return false;
                }
                self.token = Some(String::from_utf8_lossy(token).into_owned());
                true
            }
        }
    }
}

/// Write one control line, all of it, whatever mode the stream is in.
fn write_line(stream: &StreamWrapper, msg: &str) -> io::Result<()> {
    #[cfg(not(windows))]
    stream.set_nonblocking(false)?;
    stream.write_all(msg.as_bytes())?;
    stream.write_all(b"\n")?;
    stream.flush()
}

/// Binding a session that another server owns. Carried inside the
/// `io::Error` (kind `AddrInUse`) that [`ServerListener::bind`] returns.
#[derive(Debug)]
pub struct SessionTaken {
    /// The owner's pid, as its pid file gives it
    pub pid: Option<u32>,
}

impl std::fmt::Display for SessionTaken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.pid {
            Some(pid) => write!(
                f,
                "a daemon for this session is already running (pid {pid})"
            ),
            None => write!(f, "a daemon for this session is already running"),
        }
    }
}

impl std::error::Error for SessionTaken {}

fn session_taken(pid: Option<u32>) -> io::Error {
    io::Error::new(io::ErrorKind::AddrInUse, SessionTaken { pid })
}

/// Take ownership of the session at `paths`: an exclusive lock on its lock
/// file, which the OS releases when the owner exits, however it exits. The
/// lock is what makes "is a server running?" and "then this one is" a single
/// step: two servers starting at once can both see none running, but only
/// one gets the lock.
///
/// When another server holds it, wait until that one has published its pid
/// (it does so right after binding), then refuse with [`SessionTaken`]: a
/// client that started this server and sees it exit then finds the owner
/// running, and attaches to it. If the owner exits before publishing, the
/// lock comes free and this server takes the session after all.
///
/// Where the filesystem cannot lock (some network filesystems), fall back to
/// the pid file and a connect probe, and refuse unless they say no server
/// runs: they cannot rule out two servers starting at the same moment, but
/// they never take a session from a running one.
fn lock_session(paths: &SocketPaths) -> io::Result<Option<std::fs::File>> {
    use crate::server::daemon::is_process_running;
    use std::fs::TryLockError;

    let mut options = std::fs::OpenOptions::new();
    options.read(true).write(true).create(true).truncate(false);
    #[cfg(unix)]
    std::os::unix::fs::OpenOptionsExt::mode(&mut options, 0o600);
    let file = options.open(&paths.lock)?;

    match file.try_lock() {
        Ok(()) => return Ok(Some(file)),
        Err(TryLockError::WouldBlock) => {}
        Err(TryLockError::Error(e)) if e.kind() == io::ErrorKind::Unsupported => {
            tracing::warn!(
                "Cannot lock {} ({e}); telling whether the session is taken from its pid file",
                paths.lock.display()
            );
            return match paths.probe_server() {
                ServerLiveness::Dead => Ok(None),
                ServerLiveness::Alive | ServerLiveness::Unreachable => {
                    Err(session_taken(paths.read_pid().ok().flatten()))
                }
            };
        }
        Err(TryLockError::Error(e)) => return Err(e),
    }

    tracing::debug!("{} is locked; waiting for its owner", paths.lock.display());
    loop {
        if let Ok(Some(pid)) = paths.read_pid() {
            if is_process_running(pid) {
                return Err(session_taken(Some(pid)));
            }
        }
        match file.try_lock() {
            Ok(()) => return Ok(Some(file)),
            Err(TryLockError::WouldBlock) => {}
            Err(TryLockError::Error(e)) => return Err(e),
        }
        std::thread::sleep(Duration::from_millis(5));
    }
}

impl ServerListener {
    /// Create a new server listener for the given socket paths, owning the
    /// session for as long as it lives, and publish this process's pid in
    /// its pid file.
    ///
    /// Refused with [`SessionTaken`] (kind `AddrInUse`) when another server
    /// owns the session; see [`lock_session`].
    pub fn bind(paths: SocketPaths) -> io::Result<Self> {
        tracing::debug!("ServerListener::bind starting for {:?}", paths.data);

        // Ensure socket directory exists
        if let Some(parent) = paths.data.parent() {
            tracing::debug!("Creating socket directory: {:?}", parent);
            std::fs::create_dir_all(parent)?;
        }

        // Own the session before touching its files: whatever is left of
        // them now is a dead server's
        let session_lock = lock_session(&paths)?;

        // Clean up any stale sockets
        paths.cleanup()?;

        let data_name = platform::socket_name_for_path(&paths.data)?;
        let control_name = platform::socket_name_for_path(&paths.control)?;

        tracing::debug!("Creating data listener...");
        let data_listener = ListenerOptions::new()
            .name(data_name)
            .create_sync()
            .map_err(|e| {
                tracing::error!("Failed to create data listener: {}", e);
                io::Error::new(io::ErrorKind::AddrInUse, e.to_string())
            })?;

        tracing::debug!("Creating control listener...");
        let control_listener = ListenerOptions::new()
            .name(control_name)
            .create_sync()
            .map_err(|e| {
                tracing::error!("Failed to create control listener: {}", e);
                io::Error::new(io::ErrorKind::AddrInUse, e.to_string())
            })?;

        // Write marker files so exists() check works on Windows
        // (Unix domain sockets already create socket files on the filesystem)
        #[cfg(windows)]
        {
            tracing::debug!("Writing marker files...");
            std::fs::write(&paths.data, "socket")?;
            std::fs::write(&paths.control, "socket")?;
        }

        // Neither accept may block: the two streams of a connection are
        // accepted separately (see `accept`)
        data_listener.set_nonblocking(ListenerNonblockingMode::Accept)?;
        control_listener.set_nonblocking(ListenerNonblockingMode::Accept)?;

        let listener = Self {
            data_listener,
            control_listener,
            paths,
            _session_lock: session_lock,
            pending_controls: Vec::new(),
            pending_data: Vec::new(),
        };

        // Publish the pid: clients take it for a running server, and a server
        // waiting on the lock (see `lock_session`) for its cue to stand down.
        // Without it that one would wait for good, so this fails the bind
        // (dropping the listener removes the sockets again).
        listener.paths.write_pid(std::process::id())?;

        tracing::info!("Server listening on {:?}", listener.paths.data);

        Ok(listener)
    }

    /// Accept a client connection whose control and data streams have both
    /// arrived and whose Hello has been read, or `None` if there is none yet.
    /// Never blocks.
    ///
    /// The two streams are accepted separately, as they come, and matched by
    /// the pair token the client sends on both: in its Hello on the control
    /// stream, and before anything else on the data stream. Nothing about one
    /// connection holds up another — a client that connects one stream and
    /// stops (a liveness probe does, on purpose) is dropped when it closes,
    /// and two clients connecting at once each get their own streams back.
    /// Pairing them by arrival order instead let a stream from one go with
    /// a stream from the other, and waiting for a data stream after each
    /// control stream let a probe stall the server for good.
    ///
    /// A client speaking another protocol version is answered with a
    /// [`VersionMismatch`] here and never returned.
    pub fn accept(&mut self) -> io::Result<Option<ServerConnection>> {
        while let Some(stream) = accept_ready(&self.control_listener)? {
            push_bounded(
                &mut self.pending_controls,
                PendingControl {
                    stream: StreamWrapper::new(stream),
                    line: Vec::new(),
                    hello: None,
                },
            );
        }
        while let Some(stream) = accept_ready(&self.data_listener)? {
            push_bounded(
                &mut self.pending_data,
                PendingData {
                    stream: StreamWrapper::new(stream),
                    preamble: Vec::new(),
                    token: None,
                },
            );
        }

        self.pending_controls.retain_mut(PendingControl::advance);
        self.pending_data.retain_mut(PendingData::advance);

        let Some((c, d)) = self
            .pending_controls
            .iter()
            .enumerate()
            .find_map(|(c, control)| {
                let token = control.token()?;
                let d = self
                    .pending_data
                    .iter()
                    .position(|data| data.token.as_deref() == Some(token))?;
                Some((c, d))
            })
        else {
            return Ok(None);
        };
        let control = self.pending_controls.remove(c);
        let data = self.pending_data.remove(d);
        let (hello, _) = control.hello.expect("matched by its token");

        // On Windows, DON'T set nonblocking here - the try_read() function handles it
        // Setting nonblocking early can cause issues with named pipes where read()
        // returns Ok(0) when no data is available (interpreted as EOF).
        #[cfg(not(windows))]
        {
            // Set data stream to nonblocking for polling (Unix only)
            #[allow(clippy::let_underscore_must_use)]
            let _ = data.stream.set_nonblocking(true);
            control.stream.set_nonblocking(true)?;
        }

        Ok(Some(ServerConnection {
            data: data.stream,
            control: control.stream,
            hello,
        }))
    }

    /// Get the socket paths
    pub fn paths(&self) -> &SocketPaths {
        &self.paths
    }
}

/// The next stream waiting on `listener`, if any.
fn accept_ready(listener: &LocalListener) -> io::Result<Option<LocalStream>> {
    match listener.accept() {
        Ok(stream) => Ok(Some(stream)),
        Err(e) if e.kind() == io::ErrorKind::WouldBlock => Ok(None),
        #[cfg(windows)]
        Err(e) if platform_windows::is_transient_pipe_error(&e) => Ok(None),
        Err(e) => Err(e),
    }
}

/// Add `item`, dropping the oldest past [`MAX_PENDING`].
fn push_bounded<T>(pending: &mut Vec<T>, item: T) {
    if pending.len() >= MAX_PENDING {
        tracing::debug!("Too many half-made connections; dropping the oldest");
        pending.remove(0);
    }
    pending.push(item);
}

impl Drop for ServerListener {
    fn drop(&mut self) {
        // Best-effort cleanup of socket files on shutdown
        #[allow(clippy::let_underscore_must_use)]
        let _ = self.paths.cleanup();
    }
}

/// Wrapper for LocalSocketStream that provides thread-safe sharing
/// Uses Arc<Mutex<>> internally to allow cloning and use across threads
#[derive(Clone)]
pub struct StreamWrapper(Arc<Mutex<LocalStream>>);

impl StreamWrapper {
    /// Create a new StreamWrapper from a LocalStream
    fn new(stream: LocalStream) -> Self {
        Self(Arc::new(Mutex::new(stream)))
    }

    /// Set non-blocking mode
    pub fn set_nonblocking(&self, nonblocking: bool) -> io::Result<()> {
        self.0
            .lock()
            .map_err(|_| io::Error::other("mutex poisoned"))?
            .set_nonblocking(nonblocking)
    }

    /// Write all bytes (takes &self for thread sharing)
    pub fn write_all(&self, buf: &[u8]) -> io::Result<()> {
        let mut guard = self
            .0
            .lock()
            .map_err(|_| io::Error::other("mutex poisoned"))?;
        Write::write_all(&mut *guard, buf)
    }

    /// Flush the stream
    pub fn flush(&self) -> io::Result<()> {
        let mut guard = self
            .0
            .lock()
            .map_err(|_| io::Error::other("mutex poisoned"))?;
        Write::flush(&mut *guard)
    }

    /// Try to read without blocking (returns WouldBlock if no data or if mutex is contended)
    pub fn try_read(&self, buf: &mut [u8]) -> io::Result<usize> {
        let mut guard = match self.0.try_lock() {
            Ok(g) => g,
            Err(std::sync::TryLockError::WouldBlock) => {
                return Err(io::Error::new(
                    io::ErrorKind::WouldBlock,
                    "stream busy (mutex contended)",
                ));
            }
            Err(std::sync::TryLockError::Poisoned(_)) => {
                return Err(io::Error::other("mutex poisoned"));
            }
        };

        platform::try_read_nonblocking(&mut guard, buf)
    }
}

/// A deadline-bounded, buffered line reader over a control stream.
///
/// [`ClientConnection::read_control`] blocks until a newline arrives, which is
/// right for a client that has attached to an editor and follows it for the
/// rest of its life. It is wrong for the one-shot `fresh --cmd cmd …` clients
/// an agent runs: if the editor never answers — an old build, or one whose
/// event loop doesn't drain the control socket — the agent hangs forever with
/// nothing to report. Reads here fail with [`io::ErrorKind::TimedOut`] instead,
/// so a stuck editor surfaces as a diagnosable error.
///
/// Bytes read past the first newline stay in this reader's own buffer, so
/// consecutive reads on one connection never lose a message that arrived in the
/// same chunk as its predecessor — the hazard of building a fresh [`BufReader`]
/// per call.
pub struct ControlReader {
    stream: StreamWrapper,
    buf: Vec<u8>,
}

impl ControlReader {
    /// How long to sleep between polls while waiting for the next chunk. Small
    /// enough to be invisible on a live editor (replies come back in one frame),
    /// large enough not to spin a core while waiting.
    const POLL_INTERVAL: Duration = Duration::from_millis(2);

    /// Wrap a control stream. Cheap: shares the underlying stream (no I/O until
    /// the first read), so the reader can outlive the borrow it was made from
    /// and carry its buffered bytes across a whole exchange.
    pub fn new(stream: &StreamWrapper) -> Self {
        Self {
            stream: stream.clone(),
            buf: Vec::new(),
        }
    }

    /// Read one newline-terminated control message, waiting at most `timeout`.
    ///
    /// Returns `Ok(Some(line))` for a complete message (newline included, as
    /// [`ClientConnection::read_control`] does), `Ok(None)` when the peer closed
    /// the connection with nothing buffered, and `Err` with kind `TimedOut` when
    /// the deadline passes first.
    pub fn read_line_timeout(&mut self, timeout: Duration) -> io::Result<Option<String>> {
        let deadline = Instant::now() + timeout;
        let mut chunk = [0u8; 4096];

        loop {
            if let Some(line) = self.take_line() {
                return Ok(Some(line));
            }

            match self.stream.try_read(&mut chunk) {
                // EOF. A newline-less tail is handed back rather than dropped so
                // a truncated message fails to parse loudly instead of looking
                // like a clean close.
                Ok(0) => return Ok(self.take_rest()),
                Ok(n) => {
                    self.buf.extend_from_slice(&chunk[..n]);
                    continue;
                }
                Err(e)
                    if e.kind() == io::ErrorKind::WouldBlock
                        || e.kind() == io::ErrorKind::Interrupted => {}
                Err(e) => return Err(e),
            }

            let now = Instant::now();
            if now >= deadline {
                return Err(io::Error::new(
                    io::ErrorKind::TimedOut,
                    "timed out waiting for a control message",
                ));
            }
            std::thread::sleep(Self::POLL_INTERVAL.min(deadline - now));
        }
    }

    /// Split off the first complete line in the buffer, if there is one.
    fn take_line(&mut self) -> Option<String> {
        let end = self.buf.iter().position(|b| *b == b'\n')? + 1;
        let rest = self.buf.split_off(end);
        let line = std::mem::replace(&mut self.buf, rest);
        Some(String::from_utf8_lossy(&line).into_owned())
    }

    /// Take whatever is buffered, `None` when empty.
    fn take_rest(&mut self) -> Option<String> {
        if self.buf.is_empty() {
            return None;
        }
        let line = std::mem::take(&mut self.buf);
        Some(String::from_utf8_lossy(&line).into_owned())
    }
}

/// Helper to map Windows pipe errors to WouldBlock
#[inline]
fn map_windows_pipe_error(result: io::Result<usize>) -> io::Result<usize> {
    match result {
        #[cfg(windows)]
        Err(e) if platform_windows::is_transient_pipe_error(&e) => {
            Err(io::Error::new(io::ErrorKind::WouldBlock, e))
        }
        other => other,
    }
}

impl Read for StreamWrapper {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let result = self
            .0
            .lock()
            .map_err(|_| io::Error::other("mutex poisoned"))?
            .read(buf);
        map_windows_pipe_error(result)
    }
}

impl Read for &StreamWrapper {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let result = self
            .0
            .lock()
            .map_err(|_| io::Error::other("mutex poisoned"))?
            .read(buf);
        map_windows_pipe_error(result)
    }
}

/// A client connection (from the server's perspective)
pub struct ServerConnection {
    /// Data stream for raw byte stream
    pub data: StreamWrapper,
    /// Control stream for JSON messages
    pub control: StreamWrapper,
    /// The Hello the client opened with, already read off the control stream
    /// (and of this server's protocol version)
    pub hello: ClientHello,
}

impl ServerConnection {
    /// Read available data from the data socket (non-blocking)
    pub fn read_data(&self, buf: &mut [u8]) -> io::Result<usize> {
        self.data.try_read(buf)
    }

    /// Write data to the data socket
    pub fn write_data(&self, buf: &[u8]) -> io::Result<()> {
        self.data.write_all(buf)?;
        self.data.flush()
    }

    /// Read a control message (blocking)
    pub fn read_control(&self) -> io::Result<Option<String>> {
        // On Windows, don't toggle blocking mode - named pipes don't support mode
        // switching after connection. The pipe should already be in blocking mode.
        #[cfg(not(windows))]
        self.control.set_nonblocking(false)?;
        let mut reader = BufReader::new(&self.control);
        let mut line = String::new();
        match reader.read_line(&mut line) {
            Ok(0) => Ok(None), // EOF
            Ok(_) => Ok(Some(line)),
            Err(e) => Err(e),
        }
    }

    /// Write a control message
    ///
    /// Control messages MUST be delivered atomically: the client frames them
    /// by reading up to a trailing newline, so a partial write (message body
    /// with no terminating `\n`) wedges the client's blocking `read_control()`
    /// forever, waiting for a newline that never arrives.
    ///
    /// The server keeps the control socket in non-blocking mode for its
    /// polling reads in `process_clients()`. On a non-blocking socket a large
    /// message (e.g. a `SetClipboard` carrying a big selection) overflows the
    /// kernel send buffer: `write_all` writes part of it, hits `WouldBlock`,
    /// and returns an error — leaving a truncated, newline-less message on the
    /// wire. To guarantee the whole message (and its newline) is delivered we
    /// force blocking mode for the duration of the write, then restore
    /// non-blocking mode for the next polling read.
    pub fn write_control(&self, msg: &str) -> io::Result<()> {
        // Force blocking so write_all cannot leave a partial message on the
        // wire. Only the server's main thread touches the control stream, so
        // toggling the mode here is race-free.
        #[cfg(not(windows))]
        let restore_nonblocking = self.control.set_nonblocking(false).is_ok();

        let result = (|| {
            self.control.write_all(msg.as_bytes())?;
            if !msg.ends_with('\n') {
                self.control.write_all(b"\n")?;
            }
            self.control.flush()
        })();

        // Restore non-blocking mode for the polling read in process_clients().
        #[cfg(not(windows))]
        if restore_nonblocking {
            #[allow(clippy::let_underscore_must_use)]
            let _ = self.control.set_nonblocking(true);
        }

        result
    }
}

/// Client connection to server
pub struct ClientConnection {
    /// Data stream for raw byte stream
    pub data: StreamWrapper,
    /// Control stream for JSON messages
    pub control: StreamWrapper,
    /// Sent at the start of the data stream and in the Hello, so the server
    /// can tell the two streams belong together
    pair_token: String,
}

/// Normalize a connect failure, keeping a permission denial distinguishable.
///
/// Everything else collapses to `ConnectionRefused`, but `PermissionDenied`
/// has to survive: it means "not allowed to reach it", which callers must not
/// conflate with "nothing is listening" — a sandbox that denies `connect(2)`
/// on the socket produces it while the editor is running perfectly well.
fn connect_error(e: impl std::fmt::Display + Into<io::Error>) -> io::Error {
    let err: io::Error = e.into();
    let kind = match err.kind() {
        io::ErrorKind::PermissionDenied => io::ErrorKind::PermissionDenied,
        _ => io::ErrorKind::ConnectionRefused,
    };
    io::Error::new(kind, err.to_string())
}

impl ClientConnection {
    /// Connect to a server at the given socket paths
    pub fn connect(paths: &SocketPaths) -> io::Result<Self> {
        let control_name = platform::socket_name_for_path(&paths.control)?;
        let data_name = platform::socket_name_for_path(&paths.data)?;

        let control = Stream::connect(control_name).map_err(connect_error)?;
        let data = StreamWrapper::new(Stream::connect(data_name).map_err(connect_error)?);

        // Mark the data stream as this connection's: the Hello sent on the
        // control stream (`send_hello`) carries the same token
        let pair_token = new_pair_token();
        data.write_all(PAIR_MAGIC)?;
        data.write_all(pair_token.as_bytes())?;
        data.flush()?;

        Ok(Self {
            data,
            control: StreamWrapper::new(control),
            pair_token,
        })
    }

    /// Open the handshake: send `hello`, carrying this connection's pair
    /// token. The server matches the control and data streams by it, and
    /// answers nothing until it has both.
    pub fn send_hello(&self, mut hello: ClientHello) -> io::Result<()> {
        hello.pair_token = Some(self.pair_token.clone());
        let json = serde_json::to_string(&ClientControl::Hello(hello)).map_err(io::Error::other)?;
        self.write_control(&json)
    }

    /// Set data socket to non-blocking mode
    pub fn set_data_nonblocking(&self, nonblocking: bool) -> io::Result<()> {
        self.data.set_nonblocking(nonblocking)
    }

    /// Read from data socket
    pub fn read_data(&self, buf: &mut [u8]) -> io::Result<usize> {
        self.data.try_read(buf)
    }

    /// Write to data socket
    pub fn write_data(&self, buf: &[u8]) -> io::Result<()> {
        self.data.write_all(buf)?;
        self.data.flush()
    }

    /// Read a control message
    pub fn read_control(&self) -> io::Result<Option<String>> {
        let mut reader = BufReader::new(&self.control);
        let mut line = String::new();
        match reader.read_line(&mut line) {
            Ok(0) => Ok(None),
            Ok(_) => Ok(Some(line)),
            Err(e) => Err(e),
        }
    }

    /// A deadline-bounded reader over this connection's control stream, for
    /// clients that must not hang when the editor never answers (see
    /// [`ControlReader`]). Hold on to one reader for the whole exchange: it
    /// carries the bytes buffered between reads.
    pub fn control_reader(&self) -> ControlReader {
        ControlReader::new(&self.control)
    }

    /// Write a control message
    pub fn write_control(&self, msg: &str) -> io::Result<()> {
        self.control.write_all(msg.as_bytes())?;
        if !msg.ends_with('\n') {
            self.control.write_all(b"\n")?;
        }
        self.control.flush()
    }

    /// Get the raw file descriptors for use with poll/select (Unix only)
    #[cfg(unix)]
    pub fn as_raw_fds(&self) -> (std::os::unix::io::RawFd, std::os::unix::io::RawFd) {
        use std::os::unix::io::{AsFd, AsRawFd};
        let data_guard = self.data.0.lock().unwrap();
        let ctrl_guard = self.control.0.lock().unwrap();
        let data_fd = match &*data_guard {
            Stream::UdSocket(s) => s.as_fd().as_raw_fd(),
        };
        let ctrl_fd = match &*ctrl_guard {
            Stream::UdSocket(s) => s.as_fd().as_raw_fd(),
        };
        (data_fd, ctrl_fd)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::server::protocol::TermSize;
    use tempfile::TempDir;

    #[test]
    fn test_socket_paths_encode_working_dir() {
        let paths = SocketPaths::for_working_dir(Path::new("/home/user/project")).unwrap();
        // Should encode path separators
        assert!(paths.data.to_string_lossy().contains("home_user_project"));
        assert!(paths.data.to_string_lossy().ends_with(".data.sock"));
        assert!(paths.control.to_string_lossy().ends_with(".ctrl.sock"));
    }

    #[test]
    fn test_named_session_uses_name_directly() {
        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("my-session", temp_dir.path());
        assert!(paths
            .data
            .to_string_lossy()
            .contains("my-session.data.sock"));
        assert!(paths
            .control
            .to_string_lossy()
            .contains("my-session.ctrl.sock"));
    }

    /// A working directory deep enough that its socket path overflows
    /// `sun_path` gets a folded stem that fits, while a short one keeps the
    /// name every earlier release gave it.
    #[test]
    fn socket_paths_for_a_long_working_dir_fit_sun_path() {
        let socket_dir = Path::new("/run/user/1000/fresh");
        let limit = Some(107);
        let deep = format!("/tmp/{}/proj", "x".repeat(100));
        let sibling = format!("/tmp/{}/proj2", "x".repeat(100));
        let stem =
            |dir: &str| socket_stem(&encode_path_for_filename(Path::new(dir)), socket_dir, limit);

        let a = stem(&deep);
        let full = socket_dir.join(format!("{a}{LONGEST_SUFFIX}"));
        assert!(full.as_os_str().len() <= 107, "{}", full.display());
        assert!(a.starts_with("tmp_xxx"), "keeps a readable head: {a}");
        assert_eq!(a, stem(&deep), "stable across calls");
        assert_ne!(a, stem(&sibling), "distinct directories stay distinct");
        // A folded stem fits, so it names the same files when handed back as
        // a session name (as `daemon list` does).
        assert_eq!(socket_stem(&a, socket_dir, limit), a);

        assert_eq!(stem("/home/user/project"), "home_user_project");
        // Named pipes have no such limit: nothing folds on Windows.
        let long = encode_path_for_filename(Path::new(&deep));
        assert_eq!(socket_stem(&long, socket_dir, None), long);
    }

    /// The same, through the real socket directory and platform limit, for
    /// every file a session has.
    #[cfg(unix)]
    #[test]
    fn every_session_file_for_a_long_working_dir_fits() {
        let max = platform::max_socket_path_len().unwrap();
        let deep = std::env::temp_dir().join("x".repeat(200)).join("proj");
        let paths = SocketPaths::for_working_dir(&deep).unwrap();
        for p in [&paths.data, &paths.control, &paths.pid, &paths.lock] {
            assert!(p.as_os_str().len() <= max, "{}", p.display());
        }
        let again = SocketPaths::for_working_dir(&deep).unwrap();
        assert_eq!(paths.data, again.data);

        let short = SocketPaths::for_working_dir(Path::new("/home/user/project")).unwrap();
        assert_eq!(
            short.data.file_name().unwrap(),
            "home_user_project.data.sock"
        );
    }

    /// A named daemon with an over-long name folds the same way.
    #[cfg(unix)]
    #[test]
    fn a_long_session_name_fits() {
        let max = platform::max_socket_path_len().unwrap();
        let temp_dir = TempDir::new().unwrap();
        let name = "n".repeat(300);
        let paths = SocketPaths::for_session_name_in_dir(&name, temp_dir.path());
        assert!(paths.data.as_os_str().len() <= max);
        let stem = paths.data.file_name().unwrap().to_str().unwrap();
        let stem = stem.strip_suffix(".data.sock").unwrap();
        let listed = SocketPaths::for_session_name_in_dir(stem, temp_dir.path());
        assert_eq!(listed.control, paths.control);
    }

    #[test]
    fn test_exists_returns_false_for_missing_sockets() {
        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("test-session", temp_dir.path());
        assert!(!paths.exists());
    }

    #[test]
    fn test_cleanup_succeeds_on_missing_files() {
        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("test-session", temp_dir.path());
        // Should not error when files don't exist
        assert!(paths.cleanup().is_ok());
    }

    #[test]
    fn test_socket_directory_creates_dir() {
        let dir = SocketPaths::socket_directory().unwrap();
        assert!(dir.exists());
        assert!(dir.is_dir());
    }

    #[test]
    fn test_different_working_dirs_get_different_paths() {
        let paths1 = SocketPaths::for_working_dir(Path::new("/home/user/project1")).unwrap();
        let paths2 = SocketPaths::for_working_dir(Path::new("/home/user/project2")).unwrap();
        assert_ne!(paths1.data, paths2.data);
        assert_ne!(paths1.control, paths2.control);
    }

    #[test]
    fn test_same_working_dir_gets_same_paths() {
        let paths1 = SocketPaths::for_working_dir(Path::new("/home/user/project")).unwrap();
        let paths2 = SocketPaths::for_working_dir(Path::new("/home/user/project")).unwrap();
        assert_eq!(paths1.data, paths2.data);
        assert_eq!(paths1.control, paths2.control);
    }

    #[test]
    fn test_pid_file_path_included() {
        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("pid-test-session", temp_dir.path());
        assert!(paths.pid.to_string_lossy().contains("pid-test-session.pid"));
    }

    #[test]
    fn test_write_and_read_pid() {
        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("test-session", temp_dir.path());

        // Write PID
        paths.write_pid(12345).unwrap();
        assert!(paths.pid.exists());

        // Read PID
        let pid = paths.read_pid().unwrap();
        assert_eq!(pid, Some(12345));

        // Clean up
        paths.cleanup().unwrap();
        assert!(!paths.pid.exists());
    }

    #[test]
    fn test_read_pid_returns_none_for_missing_file() {
        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("test-session", temp_dir.path());
        assert_eq!(paths.read_pid().unwrap(), None);
    }

    /// A second listener for a session that is already served is refused,
    /// and the first one keeps serving it. Binding used to clear the
    /// session's files whoever owned them, so the second took the session
    /// over and left the first running where no client could reach it.
    #[test]
    fn a_second_bind_of_a_served_session_is_refused() {
        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("served", temp_dir.path());
        let mut first = ServerListener::bind(paths.clone()).unwrap();
        paths.write_pid(std::process::id()).unwrap();

        let err = ServerListener::bind(paths.clone())
            .err()
            .expect("a second bind of a served session must be refused");
        assert_eq!(err.kind(), io::ErrorKind::AddrInUse, "{err}");
        assert_eq!(paths.read_pid().unwrap(), Some(std::process::id()));

        // A client still reaches the first listener.
        let client = ClientConnection::connect(&paths).expect("connect to the first listener");
        client.send_hello(hello(TermSize::new(80, 24))).unwrap();
        drop(accept_one(&mut first));
    }

    /// A server that finds the session locked by one that never published
    /// its pid (it died starting up, or is still binding) waits, and takes
    /// the session once the lock comes free.
    #[test]
    fn a_bind_waiting_on_the_lock_takes_the_session_when_it_comes_free() {
        use std::sync::atomic::{AtomicBool, Ordering};

        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("orphaned", temp_dir.path());
        let owner = std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(&paths.lock)
            .unwrap();
        owner.try_lock().unwrap();

        let released = Arc::new(AtomicBool::new(false));
        let waiter = {
            let paths = paths.clone();
            let released = Arc::clone(&released);
            std::thread::spawn(move || {
                let bound = ServerListener::bind(paths);
                (bound.map(drop), released.load(Ordering::SeqCst))
            })
        };
        released.store(true, Ordering::SeqCst);
        drop(owner);

        let (bound, after_release) = waiter.join().unwrap();
        bound.expect("the waiting bind takes the session");
        assert!(after_release, "bind returned while the lock was held");
    }

    #[test]
    fn test_cleanup_if_stale_with_no_sockets() {
        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("test-session", temp_dir.path());

        // No sockets exist, should return false (nothing to clean)
        assert!(!paths.cleanup_if_stale());
    }

    /// A liveness probe connects to the control socket and hangs up at once.
    /// The next real client must still get its own connection: accepting
    /// used to take the probe's control stream and then wait for a data
    /// stream to go with it, pairing it with the next client's, so that
    /// client's Hello went unanswered.
    #[cfg(unix)]
    #[test]
    fn a_probe_does_not_take_the_next_clients_connection() {
        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("probed", temp_dir.path());
        let mut listener = ServerListener::bind(paths.clone()).unwrap();

        assert_eq!(
            platform::probe_server_by_connect(&paths.control),
            platform::ConnectProbe::Alive
        );

        let client = ClientConnection::connect(&paths).unwrap();
        client.send_hello(hello(TermSize::new(81, 25))).unwrap();

        let server_conn = accept_one(&mut listener);
        assert_eq!(server_conn.hello.term_size, TermSize::new(81, 25));
        server_conn.write_control("{\"reply\":1}").unwrap();
        server_conn.write_data(b"data").unwrap();
        assert_eq!(
            client.read_control().unwrap().as_deref().map(str::trim),
            Some("{\"reply\":1}")
        );
        assert_eq!(read_exactly(&client.data, 4), b"data");
    }

    /// Two clients whose streams arrive interleaved each get their own back.
    #[cfg(unix)]
    #[test]
    fn streams_arriving_out_of_order_are_matched_by_their_token() {
        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("interleaved", temp_dir.path());
        let mut listener = ServerListener::bind(paths.clone()).unwrap();
        let connect = |path: &Path| {
            StreamWrapper::new(
                Stream::connect(platform::socket_name_for_path(path).unwrap()).unwrap(),
            )
        };

        // A's control, B's control, B's data, A's data
        let (token_a, token_b) = (new_pair_token(), new_pair_token());
        let control_a = connect(&paths.control);
        let control_b = connect(&paths.control);
        let data_b = connect(&paths.data);
        let data_a = connect(&paths.data);
        for (data, token) in [(&data_b, &token_b), (&data_a, &token_a)] {
            data.write_all(PAIR_MAGIC).unwrap();
            data.write_all(token.as_bytes()).unwrap();
        }
        for (control, token, cols) in [(&control_a, &token_a, 10), (&control_b, &token_b, 20)] {
            let mut hello = hello(TermSize::new(cols, 5));
            hello.pair_token = Some(token.clone());
            write_line(
                control,
                &serde_json::to_string(&ClientControl::Hello(hello)).unwrap(),
            )
            .unwrap();
        }

        for _ in 0..2 {
            let conn = accept_one(&mut listener);
            conn.write_data(&[conn.hello.term_size.cols as u8]).unwrap();
        }
        assert_eq!(read_exactly(&data_a, 1), [10]);
        assert_eq!(read_exactly(&data_b, 1), [20]);
    }

    /// A client of another protocol version cannot send a pair token the
    /// way this one expects, so it is told of the mismatch on its control
    /// stream alone, as before.
    #[test]
    fn a_client_of_another_protocol_version_is_told_so() {
        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("old-client", temp_dir.path());
        let mut listener = ServerListener::bind(paths.clone()).unwrap();

        let control = StreamWrapper::new(
            Stream::connect(platform::socket_name_for_path(&paths.control).unwrap()).unwrap(),
        );
        let mut old = hello(TermSize::new(80, 24));
        old.protocol_version = PROTOCOL_VERSION - 1;
        write_line(
            &control,
            &serde_json::to_string(&ClientControl::Hello(old)).unwrap(),
        )
        .unwrap();

        let reply = std::thread::spawn(move || {
            let mut line = String::new();
            BufReader::new(&control).read_line(&mut line).unwrap();
            line
        });
        while !reply.is_finished() {
            assert!(listener.accept().unwrap().is_none());
        }
        let reply: ServerControl = serde_json::from_str(&reply.join().unwrap()).unwrap();
        assert!(
            matches!(reply, ServerControl::VersionMismatch(_)),
            "{reply:?}"
        );
    }

    fn hello(term_size: TermSize) -> ClientHello {
        ClientHello::new(term_size)
    }

    /// Accept until a whole connection comes through.
    fn accept_one(listener: &mut ServerListener) -> ServerConnection {
        loop {
            if let Some(conn) = listener.accept().unwrap() {
                return conn;
            }
        }
    }

    /// Read exactly `n` bytes off `stream`, waiting for them.
    #[cfg(unix)]
    fn read_exactly(stream: &StreamWrapper, n: usize) -> Vec<u8> {
        stream.set_nonblocking(false).unwrap();
        let mut buf = vec![0u8; n];
        Read::read_exact(&mut &*stream, &mut buf).unwrap();
        buf
    }

    /// Connect a client to a freshly bound listener and return both ends.
    #[cfg(unix)]
    fn connected_pair(paths: &SocketPaths) -> (ServerConnection, ClientConnection) {
        use std::thread;

        let mut listener = ServerListener::bind(paths.clone()).unwrap();
        let client_paths = paths.clone();
        let client = thread::spawn(move || {
            let conn = ClientConnection::connect(&client_paths).unwrap();
            conn.send_hello(hello(TermSize::new(80, 24))).unwrap();
            conn
        });
        let server_conn = accept_one(&mut listener);
        // The listener owns the socket files; keep it alive for the caller's
        // lifetime by leaking it (the temp dir cleans up).
        std::mem::forget(listener);
        (server_conn, client.join().unwrap())
    }

    /// Two messages written back-to-back arrive in one chunk. A reader that
    /// rebuilt its buffer per call (what `read_control` does with a fresh
    /// `BufReader`) would drop the second; `ControlReader` keeps it.
    #[cfg(unix)]
    #[test]
    fn control_reader_keeps_a_second_message_from_the_same_chunk() {
        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("ctrl-reader-chunk", temp_dir.path());
        let (server_conn, client_conn) = connected_pair(&paths);

        server_conn.write_control("{\"first\":1}").unwrap();
        server_conn.write_control("{\"second\":2}").unwrap();

        let mut reader = client_conn.control_reader();
        let first = reader
            .read_line_timeout(Duration::from_secs(30))
            .unwrap()
            .expect("first message");
        let second = reader
            .read_line_timeout(Duration::from_secs(30))
            .unwrap()
            .expect("second message");
        assert_eq!(first.trim(), "{\"first\":1}");
        assert_eq!(second.trim(), "{\"second\":2}");
    }

    /// A peer that accepts the connection and then goes quiet must not park the
    /// reader forever — this is the `fresh --cmd cmd list` hang, bounded. The
    /// deadline is the unit under test: nothing is ever written, so there is no
    /// race between "reply arrives" and "deadline passes".
    #[cfg(unix)]
    #[test]
    fn control_reader_times_out_when_nothing_is_written() {
        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("ctrl-reader-timeout", temp_dir.path());
        let (_server_conn, client_conn) = connected_pair(&paths);

        let err = client_conn
            .control_reader()
            .read_line_timeout(Duration::from_millis(50))
            .expect_err("a silent peer must not block the reader forever");
        assert_eq!(err.kind(), io::ErrorKind::TimedOut);
    }

    /// A closed connection reports EOF rather than waiting out the deadline.
    #[cfg(unix)]
    #[test]
    fn control_reader_reports_eof_when_the_peer_closes() {
        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("ctrl-reader-eof", temp_dir.path());
        let (server_conn, client_conn) = connected_pair(&paths);
        drop(server_conn);

        let msg = client_conn
            .control_reader()
            .read_line_timeout(Duration::from_secs(30))
            .expect("EOF is not an error");
        assert!(msg.is_none(), "expected EOF, got {:?}", msg);
    }

    /// Regression test for the embedded-terminal copy hang.
    ///
    /// When the server's control socket is left non-blocking (as it is between
    /// polling reads in `process_clients()`), a large control message — e.g. a
    /// `SetClipboard` carrying a big selection copied from a large terminal
    /// scrollback — used to overflow the kernel send buffer: `write_all` wrote
    /// part of it, hit `WouldBlock`, and dropped the rest. The truncated,
    /// newline-less message then wedged the client's blocking `read_control()`
    /// forever, freezing the whole client.
    ///
    /// `write_control()` must deliver the entire message (including its
    /// trailing newline) regardless of the socket's non-blocking mode. Here a
    /// strict reader (the same blocking `read_control()` framing the client
    /// relied on) delays briefly before draining, forcing the send buffer to
    /// fill mid-write; the full message must still arrive.
    ///
    /// Without the fix, the non-blocking `write_all` drops the tail of the
    /// message and the reader blocks forever waiting for a newline — the test
    /// then hangs and is killed by the external runner (no in-test timeout, per
    /// the project's testing guidelines). With the fix it completes promptly.
    #[cfg(unix)]
    #[test]
    fn test_write_control_delivers_large_message_when_nonblocking() {
        use std::sync::mpsc;
        use std::thread;
        use std::time::Duration;

        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("ctrl-large-write", temp_dir.path());
        let mut listener = ServerListener::bind(paths.clone()).unwrap();

        // 4 MiB easily exceeds any local-socket send/receive buffer, so a
        // non-blocking write_all is guaranteed to hit WouldBlock part-way.
        let big_text = "X".repeat(4 * 1024 * 1024);
        let msg = serde_json::to_string(&crate::server::protocol::ServerControl::SetClipboard {
            text: big_text.clone(),
            use_osc52: true,
            use_system_clipboard: true,
        })
        .unwrap();

        let (connected_tx, connected_rx) = mpsc::channel::<()>();
        let (result_tx, result_rx) = mpsc::channel::<io::Result<Option<String>>>();
        let paths_client = paths.clone();
        let reader = thread::spawn(move || {
            let conn = ClientConnection::connect(&paths_client).unwrap();
            conn.send_hello(hello(TermSize::new(80, 24))).unwrap();
            // Signal connected, then delay before reading so the server writes
            // while nothing is draining the socket — recreating the buffer
            // pressure that triggered the partial-write bug. The delay only
            // affects how reliably the *bug* reproduces; the fixed code passes
            // regardless of its duration.
            connected_tx.send(()).unwrap();
            thread::sleep(Duration::from_millis(300));
            let received = conn.read_control();
            #[allow(clippy::let_underscore_must_use)]
            let _ = result_tx.send(received);
        });

        // Accept the client's connection.
        let server_conn = loop {
            if let Some(c) = listener.accept().unwrap() {
                break c;
            }
            thread::sleep(Duration::from_millis(5));
        };

        // Mimic the server's steady state: control socket left non-blocking.
        server_conn.control.set_nonblocking(true).unwrap();

        // Write while the client is still sleeping (not draining). The buggy
        // implementation returns Err(WouldBlock) after a partial write (which
        // the real server ignores too); the fixed one blocks until the client
        // drains the whole message. We assert on what the client *receives*, so
        // the write result itself is intentionally ignored.
        connected_rx.recv().unwrap();
        #[allow(clippy::let_underscore_must_use)]
        let _ = server_conn.write_control(&msg);

        // The client must receive a complete, parseable message. Wait
        // indefinitely: with the bug this never arrives and the external runner
        // times the test out.
        let received = result_rx
            .recv()
            .expect("reader thread dropped the channel")
            .expect("control read failed")
            .expect("control stream closed unexpectedly");

        match serde_json::from_str::<crate::server::protocol::ServerControl>(received.trim())
            .expect("received control message should be valid JSON")
        {
            crate::server::protocol::ServerControl::SetClipboard { text, .. } => {
                assert_eq!(
                    text.len(),
                    big_text.len(),
                    "the full clipboard payload must be delivered intact"
                );
            }
            other => panic!("unexpected control message: {:?}", other),
        }

        reader.join().unwrap();
    }
}
