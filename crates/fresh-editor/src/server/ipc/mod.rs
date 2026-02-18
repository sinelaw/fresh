//! IPC infrastructure for client-server communication
//!
//! Uses the `interprocess` crate for cross-platform local sockets:
//! - Unix domain sockets on Linux/macOS
//! - Named pipes on Windows
//!
//! Each session has two sockets: data (byte stream) and control (JSON messages).

use std::io::{self, BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use interprocess::local_socket::{
    prelude::*, Listener, ListenerNonblockingMode, ListenerOptions, Stream,
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

/// Socket paths for a session
#[derive(Debug, Clone)]
pub struct SocketPaths {
    /// Data socket path (raw byte stream)
    pub data: PathBuf,
    /// Control socket path (JSON messages)
    pub control: PathBuf,
    /// PID file path (for detecting stale sessions)
    pub pid: PathBuf,
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

        Ok(Self {
            data: socket_dir.join(format!("{}.data.sock", encoded)),
            control: socket_dir.join(format!("{}.ctrl.sock", encoded)),
            pid: socket_dir.join(format!("{}.pid", encoded)),
        })
    }

    /// Get socket paths for a named session
    pub fn for_session_name(name: &str) -> io::Result<Self> {
        let socket_dir = platform::get_socket_dir()?;
        Ok(Self::for_session_name_in_dir(name, &socket_dir))
    }

    /// Get socket paths for a named session in a specific directory
    /// (primarily for testing with isolated temp directories)
    pub fn for_session_name_in_dir(name: &str, socket_dir: &Path) -> Self {
        Self {
            data: socket_dir.join(format!("{}.data.sock", name)),
            control: socket_dir.join(format!("{}.ctrl.sock", name)),
            pid: socket_dir.join(format!("{}.pid", name)),
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
        use crate::server::daemon::is_process_running;

        // Check PID file - this is the reliable method
        if let Ok(Some(pid)) = self.read_pid() {
            if is_process_running(pid) {
                return true;
            }
        }

        // Platform-specific fallback check
        if self.exists() {
            return platform::check_server_by_connect(&self.control);
        }

        false
    }

    /// Clean up stale session files if server is not running
    /// Returns true if files were cleaned up
    pub fn cleanup_if_stale(&self) -> bool {
        if self.exists() && !self.is_server_alive() {
            // Best-effort cleanup of stale socket files
            #[allow(clippy::let_underscore_must_use)]
            let _ = self.cleanup();
            true
        } else {
            false
        }
    }

    /// Remove socket and PID files (cleanup)
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

/// Type alias for interprocess local socket stream
type LocalStream = Stream;
type LocalListener = Listener;

/// Server listener for accepting client connections
pub struct ServerListener {
    data_listener: LocalListener,
    control_listener: LocalListener,
    paths: SocketPaths,
}

impl ServerListener {
    /// Create a new server listener for the given socket paths
    pub fn bind(paths: SocketPaths) -> io::Result<Self> {
        tracing::debug!("ServerListener::bind starting for {:?}", paths.data);

        // Clean up any stale sockets
        paths.cleanup()?;

        // Ensure socket directory exists
        if let Some(parent) = paths.data.parent() {
            tracing::debug!("Creating socket directory: {:?}", parent);
            std::fs::create_dir_all(parent)?;
        }

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

        tracing::info!("Server listening on {:?}", paths.data);

        Ok(Self {
            data_listener,
            control_listener,
            paths,
        })
    }

    /// Accept a new client connection (both data and control sockets)
    /// Returns None if no connection is pending
    pub fn accept(&mut self) -> io::Result<Option<ServerConnection>> {
        // Try to accept on control socket first (client connects here first)
        // Use set_nonblocking for non-blocking accept
        if let Err(e) = self
            .control_listener
            .set_nonblocking(ListenerNonblockingMode::Accept)
        {
            // On Windows, set_nonblocking might fail if the pipe is in a certain state
            #[cfg(windows)]
            if platform_windows::is_transient_pipe_error(&e) {
                return Ok(None);
            }
            return Err(e);
        }

        let control_stream = match self.control_listener.accept() {
            Ok(stream) => stream,
            Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                return Ok(None);
            }
            #[cfg(windows)]
            Err(e) if platform_windows::is_transient_pipe_error(&e) => {
                return Ok(None);
            }
            Err(e) => return Err(e),
        };

        // Now wait for data socket connection (blocking)
        self.data_listener
            .set_nonblocking(ListenerNonblockingMode::Neither)?;
        let data_stream = self.data_listener.accept()?;

        // On Windows, DON'T set nonblocking here - the try_read() function handles it
        // Setting nonblocking early can cause issues with named pipes where read()
        // returns Ok(0) when no data is available (interpreted as EOF).
        #[cfg(not(windows))]
        {
            // Set data stream to nonblocking for polling (Unix only)
            #[allow(clippy::let_underscore_must_use)]
            let _ = data_stream.set_nonblocking(true);
            control_stream.set_nonblocking(true)?;
        }

        Ok(Some(ServerConnection {
            data: StreamWrapper::new(data_stream),
            control: StreamWrapper::new(control_stream),
        }))
    }

    /// Get the socket paths
    pub fn paths(&self) -> &SocketPaths {
        &self.paths
    }
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
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "mutex poisoned"))?
            .set_nonblocking(nonblocking)
    }

    /// Write all bytes (takes &self for thread sharing)
    pub fn write_all(&self, buf: &[u8]) -> io::Result<()> {
        let mut guard = self
            .0
            .lock()
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "mutex poisoned"))?;
        Write::write_all(&mut *guard, buf)
    }

    /// Flush the stream
    pub fn flush(&self) -> io::Result<()> {
        let mut guard = self
            .0
            .lock()
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "mutex poisoned"))?;
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
                return Err(io::Error::new(io::ErrorKind::Other, "mutex poisoned"));
            }
        };

        platform::try_read_nonblocking(&mut *guard, buf)
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
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "mutex poisoned"))?
            .read(buf);
        map_windows_pipe_error(result)
    }
}

impl Read for &StreamWrapper {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let result = self
            .0
            .lock()
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "mutex poisoned"))?
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
    pub fn write_control(&self, msg: &str) -> io::Result<()> {
        self.control.write_all(msg.as_bytes())?;
        if !msg.ends_with('\n') {
            self.control.write_all(b"\n")?;
        }
        self.control.flush()
    }
}

/// Client connection to server
pub struct ClientConnection {
    /// Data stream for raw byte stream
    pub data: StreamWrapper,
    /// Control stream for JSON messages
    pub control: StreamWrapper,
}

impl ClientConnection {
    /// Connect to a server at the given socket paths
    pub fn connect(paths: &SocketPaths) -> io::Result<Self> {
        let control_name = platform::socket_name_for_path(&paths.control)?;
        let data_name = platform::socket_name_for_path(&paths.data)?;

        // Connect control socket first, then data (matching server's accept order)
        let control = Stream::connect(control_name)
            .map_err(|e| io::Error::new(io::ErrorKind::ConnectionRefused, e.to_string()))?;

        let data = Stream::connect(data_name)
            .map_err(|e| io::Error::new(io::ErrorKind::ConnectionRefused, e.to_string()))?;

        Ok(Self {
            data: StreamWrapper::new(data),
            control: StreamWrapper::new(control),
        })
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

    #[test]
    fn test_cleanup_if_stale_with_no_sockets() {
        let temp_dir = TempDir::new().unwrap();
        let paths = SocketPaths::for_session_name_in_dir("test-session", temp_dir.path());

        // No sockets exist, should return false (nothing to clean)
        assert!(!paths.cleanup_if_stale());
    }
}
