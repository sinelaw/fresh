//! Windows-specific daemonization support

use std::io;
use std::os::windows::process::CommandExt;
use std::path::PathBuf;

use windows_sys::Win32::Foundation::{CloseHandle, STILL_ACTIVE};
use windows_sys::Win32::System::Threading::{
    GetExitCodeProcess, OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION,
};

const DETACHED_PROCESS: u32 = 0x00000008;
const CREATE_NEW_PROCESS_GROUP: u32 = 0x00000200;

/// Daemonize the current process (not supported on Windows)
///
/// On Windows, we don't daemonize the current process.
/// Instead, use `spawn_server_detached()` to start a new detached process.
pub fn daemonize() -> io::Result<()> {
    Err(io::Error::new(
        io::ErrorKind::Unsupported,
        "Use spawn_server_detached() on Windows",
    ))
}

/// Spawn the server as a detached background process
///
/// This is used when the client starts and no server is running.
/// The server inherits the current working directory.
/// Returns the PID of the spawned server.
pub fn spawn_server_detached(session_name: Option<&str>) -> io::Result<u32> {
    let exe = std::env::current_exe()?;

    let mut cmd = std::process::Command::new(&exe);
    cmd.arg("--server");

    if let Some(name) = session_name {
        cmd.arg("--session-name").arg(name);
    }

    cmd.creation_flags(DETACHED_PROCESS | CREATE_NEW_PROCESS_GROUP);
    cmd.stdin(std::process::Stdio::null());
    cmd.stdout(std::process::Stdio::null());

    // Redirect stderr to a log file for debugging
    let log_dir = std::env::var("LOCALAPPDATA")
        .map(PathBuf::from)
        .unwrap_or_else(|_| std::env::temp_dir())
        .join("fresh")
        .join("logs");
    std::fs::create_dir_all(&log_dir)?;

    let log_file = log_dir.join(format!("server-{}.log", session_name.unwrap_or("default")));
    let stderr_file = std::fs::File::create(&log_file)?;
    cmd.stderr(std::process::Stdio::from(stderr_file));

    tracing::debug!("Server log file: {:?}", log_file);

    let child = cmd.spawn()?;
    Ok(child.id())
}

/// Check if a process with the given PID is still running
pub fn is_process_running(pid: u32) -> bool {
    unsafe {
        let handle = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, 0, pid);
        if handle.is_null() {
            return false;
        }

        let mut exit_code: u32 = 0;
        let result = GetExitCodeProcess(handle, &mut exit_code);
        CloseHandle(handle);

        result != 0 && exit_code == STILL_ACTIVE as u32
    }
}
