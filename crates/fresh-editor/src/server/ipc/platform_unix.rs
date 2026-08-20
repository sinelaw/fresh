//! Unix-specific IPC implementation

use std::io::{self, Read};
use std::path::{Path, PathBuf};

use interprocess::local_socket::traits::Stream;
use interprocess::local_socket::{GenericFilePath, Stream as LocalStream, ToFsName};

/// Get the directory for socket files on Unix
pub fn get_socket_dir() -> io::Result<PathBuf> {
    // Try XDG_RUNTIME_DIR first (preferred, usually /run/user/$UID)
    if let Ok(runtime_dir) = std::env::var("XDG_RUNTIME_DIR") {
        let socket_dir = PathBuf::from(runtime_dir).join("fresh");
        std::fs::create_dir_all(&socket_dir)?;
        return Ok(socket_dir);
    }

    // Fallback to /tmp/fresh-$UID
    let uid = unsafe { libc::getuid() };
    let socket_dir = PathBuf::from(format!("/tmp/fresh-{}", uid));
    std::fs::create_dir_all(&socket_dir)?;

    // Ensure directory has correct permissions (owner-only)
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(&socket_dir, std::fs::Permissions::from_mode(0o700))?;

    Ok(socket_dir)
}

/// Convert a socket path to an interprocess socket name on Unix
pub fn socket_name_for_path(path: &Path) -> io::Result<interprocess::local_socket::Name<'static>> {
    // Convert to owned PathBuf to get 'static lifetime
    path.to_path_buf()
        .to_fs_name::<GenericFilePath>()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e.to_string()))
}

/// Try to read without blocking on Unix
///
/// Sets nonblocking mode temporarily to attempt a read.
pub fn try_read_nonblocking(stream: &mut LocalStream, buf: &mut [u8]) -> io::Result<usize> {
    stream.set_nonblocking(true)?;
    let result = stream.read(buf);
    // Best-effort restore of blocking mode
    #[allow(clippy::let_underscore_must_use)]
    let _ = stream.set_nonblocking(false);
    result
}

/// Outcome of a connect probe against a control socket.
///
/// `Blocked` is the case a plain boolean cannot express: the socket is there
/// and may well be live, but this process is not permitted to reach it. A
/// sandbox that denies `connect(2)` on a socket outside its writable roots
/// produces exactly this, and must never be read as "the editor is gone".
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConnectProbe {
    Alive,
    Refused,
    Blocked,
}

/// Probe a control socket, distinguishing "nobody home" from "not allowed".
pub fn probe_server_by_connect(control_path: &Path) -> ConnectProbe {
    let Ok(name) = socket_name_for_path(control_path) else {
        return ConnectProbe::Refused;
    };
    match interprocess::local_socket::Stream::connect(name) {
        Ok(_) => ConnectProbe::Alive,
        Err(e) if e.kind() == io::ErrorKind::PermissionDenied => ConnectProbe::Blocked,
        Err(_) => ConnectProbe::Refused,
    }
}
