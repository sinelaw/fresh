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
/// `ssh_url`, when set, is forwarded as `--ssh-url <URL>` so the
/// spawned daemon boots into an SSH authority instead of the default
/// `Authority::local()` (see `EditorServerConfig.startup_authority`).
/// `locale`, when set, is forwarded as `--locale <L>` so the client's
/// `--locale` reaches the daemon that renders the UI — the daemon has no
/// other way to see a flag that was typed on the client's command line
/// (#3149).
/// Returns the PID of the spawned server.
pub fn spawn_server_detached(
    session_name: Option<&str>,
    ssh_url: Option<&str>,
    locale: Option<&str>,
) -> io::Result<u32> {
    let exe = std::env::current_exe()?;

    let mut cmd = std::process::Command::new(&exe);
    cmd.args(server_args(session_name, ssh_url, locale));

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

/// The argv the detached daemon is started with.
///
/// Kept as its own function, mirroring the Unix side, so the forwarding is
/// testable rather than asserted: everything the daemon cannot rediscover
/// for itself has to be listed here. It re-reads the config file and the
/// environment on its own, but a flag the user typed on the *client's*
/// command line exists nowhere the daemon can see it — `--locale` is one of
/// those (#3149).
fn server_args(
    session_name: Option<&str>,
    ssh_url: Option<&str>,
    locale: Option<&str>,
) -> Vec<String> {
    let mut args = vec!["--server".to_string()];

    if let Some(name) = session_name {
        args.push("--session-name".to_string());
        args.push(name.to_string());
    }

    if let Some(url) = ssh_url {
        args.push("--ssh-url".to_string());
        args.push(url.to_string());
    }

    if let Some(locale) = locale {
        args.push("--locale".to_string());
        args.push(locale.to_string());
    }

    args
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

#[cfg(test)]
mod tests {
    use super::*;

    /// The client's `--locale` has to ride along in the daemon's argv: the
    /// daemon renders the UI, and a flag typed on the client's command line
    /// reaches it by no other route (#3149).
    #[test]
    fn server_args_forwards_the_clients_locale() {
        assert_eq!(
            server_args(Some("mysession"), None, Some("ja")),
            vec!["--server", "--session-name", "mysession", "--locale", "ja"]
        );
    }

    /// No `--locale` on the client means "let the daemon decide" — it reads
    /// the config file and the environment itself, and an empty `--locale`
    /// would override both.
    #[test]
    fn server_args_omits_locale_when_the_client_had_none() {
        let args = server_args(Some("mysession"), None, None);
        assert!(
            !args.iter().any(|a| a == "--locale"),
            "unexpected --locale in {args:?}"
        );
    }

    #[test]
    fn server_args_carries_the_ssh_url_alongside_the_locale() {
        assert_eq!(
            server_args(None, Some("ssh://host/srv"), Some("fr")),
            vec!["--server", "--ssh-url", "ssh://host/srv", "--locale", "fr"]
        );
    }
}
