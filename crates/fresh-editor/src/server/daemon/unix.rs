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
/// `ssh_url`, when set, is forwarded as `--ssh-url <URL>` so the
/// spawned daemon boots into an SSH authority instead of the default
/// `Authority::local()` (see `EditorServerConfig.startup_authority`).
/// Returns the PID of the spawned server (intermediate, not final daemon PID).
///
/// The child calls `setsid()` before exec so the daemon leads its own session
/// with no controlling terminal — the Unix counterpart of the Windows
/// `DETACHED_PROCESS | CREATE_NEW_PROCESS_GROUP`. Without it the daemon stayed
/// in the launching terminal's session and process group: closing that terminal
/// SIGHUP'd the daemon even though the user had already detached, and killing
/// the terminal's process group took the daemon down with it — along with any
/// client attached from another terminal.
///
/// `setsid()` rather than [`daemonize`] because the daemon must keep the
/// inherited working directory (it serves that project); `daemonize` chdirs
/// to `/`.
pub fn spawn_server_detached(session_name: Option<&str>, ssh_url: Option<&str>) -> io::Result<u32> {
    let exe = std::env::current_exe()?;

    let mut args = vec!["--server".to_string()];

    if let Some(name) = session_name {
        args.push("--session-name".to_string());
        args.push(name.to_string());
    }

    if let Some(url) = ssh_url {
        args.push("--ssh-url".to_string());
        args.push(url.to_string());
    }

    // Use Command to spawn, which properly handles the process
    let mut cmd = std::process::Command::new(&exe);
    cmd.args(&args);
    detach_from_terminal(&mut cmd);

    let child = cmd.spawn()?;

    Ok(child.id())
}

/// Configure `cmd` so the spawned child outlives this process's terminal:
/// stdio at `/dev/null`, and its own session with no controlling terminal.
///
/// The session is the part that matters. A child spawned normally stays in the
/// caller's session and process group, so closing the launching terminal
/// SIGHUPs it and a signal aimed at that process group reaches it — which is
/// how a detached daemon died along with the terminal it was started from.
fn detach_from_terminal(cmd: &mut std::process::Command) {
    use std::os::unix::process::CommandExt;

    cmd.stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null());

    // SAFETY: runs in the forked child between fork and exec, where only
    // async-signal-safe calls are allowed. `setsid` is one such call, and it
    // touches no allocator or lock this process holds. It fails with EPERM only
    // when the caller is already a session leader — harmless here, since that
    // means the child is already free of a controlling terminal, so the error is
    // deliberately not propagated (returning would abort an otherwise usable
    // daemon).
    unsafe {
        cmd.pre_exec(|| {
            libc::setsid();
            Ok(())
        });
    }
}

/// Check if a process with the given PID is still running
pub fn is_process_running(pid: u32) -> bool {
    // Send signal 0 to check if process exists
    unsafe { libc::kill(pid as i32, 0) == 0 }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn session_of(pid: u32) -> i32 {
        let sid = unsafe { libc::getsid(pid as i32) };
        assert_ne!(sid, -1, "could not read session of pid {pid}");
        sid
    }

    fn group_of(pid: u32) -> i32 {
        let pgid = unsafe { libc::getpgid(pid as i32) };
        assert_ne!(pgid, -1, "could not read process group of pid {pgid}");
        pgid
    }

    /// A child left in our session and process group is reachable by the
    /// terminal teardown that closing a terminal performs; `detach_from_terminal`
    /// is what moves the daemon out of range.
    ///
    /// The plain spawn is the control: it proves the assertion below can fail,
    /// so a change that silently stopped detaching would be caught rather than
    /// passing vacuously. `/bin/sh` stands in for the daemon — the property
    /// under test belongs to the spawn, not to the editor, so nothing here has
    /// to boot one.
    #[test]
    fn detach_from_terminal_puts_the_child_in_its_own_session() {
        let ours = session_of(std::process::id());

        // Control: a normally spawned child inherits our session and group.
        let mut attached = std::process::Command::new("/bin/sh")
            .args(["-c", "read _ 2>/dev/null || sleep 300"])
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .spawn()
            .expect("spawn control child");
        assert_eq!(
            session_of(attached.id()),
            ours,
            "control child should have inherited our session"
        );

        // Detached: its own session, and therefore its own process group, so
        // neither a terminal hangup nor a signal to our group can reach it.
        let mut cmd = std::process::Command::new("/bin/sh");
        cmd.args(["-c", "sleep 300"]);
        detach_from_terminal(&mut cmd);
        let mut detached = cmd.spawn().expect("spawn detached child");

        let detached_sid = session_of(detached.id());
        assert_ne!(
            detached_sid, ours,
            "detached child must not share our session"
        );
        assert_eq!(
            detached_sid,
            detached.id() as i32,
            "detached child must lead its own session"
        );
        assert_ne!(
            group_of(detached.id()),
            group_of(std::process::id()),
            "detached child must not share our process group"
        );

        // Reap both stubs rather than ignoring the results: a `sleep` that
        // outlives the test would linger for five minutes, and the detached one
        // is — by construction — in a session this process no longer controls.
        attached.kill().expect("kill the control child");
        attached.wait().expect("reap the control child");
        detached.kill().expect("kill the detached child");
        detached.wait().expect("reap the detached child");
    }
}
