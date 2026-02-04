//! Unix-specific daemonization support

use std::io;
use std::os::unix::io::AsRawFd;

/// Daemonize the current process
///
/// This function:
/// 1. Forks the process (first fork)
/// 2. Creates a new session with setsid()
/// 3. Forks again (second fork) to prevent acquiring a controlling terminal
/// 4. Redirects stdin/stdout/stderr to /dev/null
/// 5. Changes working directory to /
///
/// Returns Ok(()) in the daemon process, or an error if daemonization fails.
/// The parent process exits immediately.
pub fn daemonize() -> io::Result<()> {
    // First fork
    match unsafe { libc::fork() } {
        -1 => return Err(io::Error::last_os_error()),
        0 => {}                     // Child continues
        _ => std::process::exit(0), // Parent exits
    }

    // Create new session, become session leader
    if unsafe { libc::setsid() } == -1 {
        return Err(io::Error::last_os_error());
    }

    // Second fork to prevent acquiring controlling terminal
    match unsafe { libc::fork() } {
        -1 => return Err(io::Error::last_os_error()),
        0 => {}                     // Child continues
        _ => std::process::exit(0), // Parent exits
    }

    // Redirect stdin/stdout/stderr to /dev/null
    let devnull = std::fs::File::open("/dev/null")?;
    let devnull_fd = devnull.as_raw_fd();

    unsafe {
        libc::dup2(devnull_fd, 0); // stdin
        libc::dup2(devnull_fd, 1); // stdout
        libc::dup2(devnull_fd, 2); // stderr
    }

    // Change to root directory to avoid holding mount points
    std::env::set_current_dir("/")?;

    // Clear umask
    unsafe {
        libc::umask(0);
    }

    Ok(())
}

/// Spawn the server as a detached background process
///
/// This is used when the client starts and no server is running.
/// The server inherits the current working directory.
/// Returns the PID of the spawned server (intermediate, not final daemon PID).
pub fn spawn_server_detached(session_name: Option<&str>) -> io::Result<u32> {
    let exe = std::env::current_exe()?;

    let mut args = vec!["--server".to_string()];

    if let Some(name) = session_name {
        args.push("--session-name".to_string());
        args.push(name.to_string());
    }

    // Use Command to spawn, which properly handles the process
    let child = std::process::Command::new(&exe)
        .args(&args)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn()?;

    Ok(child.id())
}

/// Check if a process with the given PID is still running
pub fn is_process_running(pid: u32) -> bool {
    // Send signal 0 to check if process exists
    unsafe { libc::kill(pid as i32, 0) == 0 }
}
