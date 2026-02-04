//! Windows-specific IPC implementation

use std::io::{self, Read};
use std::os::windows::io::{AsHandle, AsRawHandle};
use std::path::{Path, PathBuf};

use interprocess::local_socket::{GenericNamespaced, Stream as LocalStream, ToNsName};
use windows_sys::Win32::System::Pipes::PeekNamedPipe;

/// Get the directory for socket files on Windows
///
/// On Windows, we still need a place for marker files and PID files.
/// The actual named pipes use a different namespace.
pub fn get_socket_dir() -> io::Result<PathBuf> {
    let local_app_data = std::env::var("LOCALAPPDATA")
        .unwrap_or_else(|_| std::env::temp_dir().display().to_string());
    let socket_dir = PathBuf::from(local_app_data).join("fresh").join("sockets");
    std::fs::create_dir_all(&socket_dir)?;
    Ok(socket_dir)
}

/// Convert a socket path to an interprocess socket name on Windows
///
/// On Windows, convert the path to a namespaced name for named pipes.
pub fn socket_name_for_path(path: &Path) -> io::Result<interprocess::local_socket::Name<'static>> {
    let name = path
        .file_name()
        .and_then(|n| n.to_str())
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Invalid socket path"))?;
    // Use format: @fresh-{name} for namespaced socket
    let ns_name = format!("fresh-{}", name.replace(".sock", ""));
    ns_name
        .to_ns_name::<GenericNamespaced>()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e.to_string()))
}

/// Try to read without blocking on Windows
///
/// Uses PeekNamedPipe to check if data is available before reading.
/// This is more reliable than toggling nonblocking mode on Windows named pipes.
pub fn try_read_nonblocking(stream: &mut LocalStream, buf: &mut [u8]) -> io::Result<usize> {
    // Get the raw handle from the NamedPipe variant
    let handle = match stream {
        LocalStream::NamedPipe(pipe) => pipe.as_handle().as_raw_handle() as *mut std::ffi::c_void,
    };

    // Use PeekNamedPipe to check if data is available without blocking
    let mut available: u32 = 0;
    let result = unsafe {
        PeekNamedPipe(
            handle,
            std::ptr::null_mut(),
            0,
            std::ptr::null_mut(),
            &mut available,
            std::ptr::null_mut(),
        )
    };

    if result == 0 {
        let err = io::Error::last_os_error();
        let raw = err.raw_os_error();
        // ERROR_BROKEN_PIPE (109) or ERROR_PIPE_NOT_CONNECTED (233) means pipe is closed
        if raw == Some(109) || raw == Some(233) {
            return Ok(0); // EOF
        }
        return Err(err);
    }

    if available == 0 {
        return Err(io::Error::new(
            io::ErrorKind::WouldBlock,
            "no data available",
        ));
    }

    // Data is available, do a regular read
    stream.read(buf)
}

/// Check if server is alive by trying to connect (not used on Windows)
///
/// On Windows, we don't try to connect to verify - it can leave pipes in busy state.
/// Just rely on PID file.
pub fn check_server_by_connect(_control_path: &Path) -> bool {
    false
}

/// Check if a Windows pipe error should be treated as WouldBlock
#[inline]
pub fn is_transient_pipe_error(error: &io::Error) -> bool {
    let raw_error = error.raw_os_error();
    // ERROR_NO_DATA (232) - The pipe is being closed
    // ERROR_PIPE_NOT_CONNECTED (233) - No process on other end (can happen transiently)
    raw_error == Some(232) || raw_error == Some(233)
}
