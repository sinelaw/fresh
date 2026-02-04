//! Daemonization support for running the server in the background
//!
//! On Unix: Uses double-fork with setsid() to fully detach from terminal
//! On Windows: Uses CreateProcess with DETACHED_PROCESS flag

use std::io;
use std::path::PathBuf;

#[cfg(unix)]
mod unix;
#[cfg(windows)]
mod windows;

#[cfg(unix)]
pub use unix::*;
#[cfg(windows)]
pub use windows::*;

/// Write the server PID to a file for tracking
pub fn write_pid_file(socket_dir: &std::path::Path, session_id: &str) -> io::Result<PathBuf> {
    let pid_file = socket_dir.join(format!("{}.pid", session_id));
    std::fs::write(&pid_file, std::process::id().to_string())?;
    Ok(pid_file)
}

/// Read the server PID from a file
pub fn read_pid_file(socket_dir: &std::path::Path, session_id: &str) -> io::Result<Option<u32>> {
    let pid_file = socket_dir.join(format!("{}.pid", session_id));
    if !pid_file.exists() {
        return Ok(None);
    }

    let content = std::fs::read_to_string(&pid_file)?;
    content
        .trim()
        .parse()
        .map(Some)
        .map_err(|e| io::Error::other(format!("Invalid PID file: {}", e)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_process_running() {
        // Current process should be running
        let pid = std::process::id();
        assert!(is_process_running(pid));

        // PID 999999999 is unlikely to exist
        assert!(!is_process_running(999999999));
    }
}
