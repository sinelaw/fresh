//! Daemonization support for running the server in the background
//!
//! On Unix: Uses double-fork with setsid() to fully detach from terminal
//! On Windows: Uses CreateProcess with DETACHED_PROCESS flag

use std::io;
use std::path::{Path, PathBuf};

#[cfg(unix)]
mod unix;
#[cfg(windows)]
mod windows;

#[cfg(unix)]
pub use unix::*;
#[cfg(windows)]
pub use windows::*;

/// Everything a detached daemon has to be *told*, because it cannot
/// rediscover it for itself.
///
/// The daemon finds its config file and reads the environment on its own, but
/// a flag the user typed on the *client's* command line exists nowhere the
/// daemon can see it — the client and the daemon are different processes with
/// different command lines (#3149). So every field here is one the client
/// knows and the daemon otherwise could not.
///
/// A struct rather than five positional parameters: the two call sites read as
/// what they mean, and adding a field does not silently reorder anything.
#[derive(Debug, Clone, Copy, Default)]
pub struct DaemonSpawn<'a> {
    /// `--session-name NAME`. `None` keys the daemon on its working directory.
    pub session_name: Option<&'a str>,
    /// `--ssh-url URL`. Boots the daemon into an SSH authority instead of the
    /// default `Authority::local()` (see `EditorServerConfig.startup_authority`).
    pub ssh_url: Option<&'a str>,
    /// `--locale L`, as typed on the client's command line.
    pub locale: Option<&'a str>,
    /// `--config PATH`, as typed on the client's command line. Passed verbatim
    /// because the daemon inherits this process's working directory, so a
    /// relative path resolves the same on both sides.
    pub config: Option<&'a Path>,
    /// `--orchestrator-mode`: boot into Orchestrator mode — bring back the
    /// workspace that was last focused rather than the one matching the launch
    /// directory. Set by the bare-`fresh` launch (see `config.orchestrator_mode`);
    /// never inferred by the daemon, which has no way to know how it was started.
    pub orchestrator_mode: bool,
}

/// The argv a detached daemon is exec'd with, for the platform spawners.
///
/// Shared rather than per-platform: nothing in it is platform-specific, and
/// the two copies it replaces had drifted only in their tests' path spellings.
pub(crate) fn server_args(spawn: &DaemonSpawn<'_>) -> Vec<String> {
    let mut args = vec!["--server".to_string()];

    if let Some(name) = spawn.session_name {
        args.push("--session-name".to_string());
        args.push(name.to_string());
    }

    if let Some(url) = spawn.ssh_url {
        args.push("--ssh-url".to_string());
        args.push(url.to_string());
    }

    if let Some(locale) = spawn.locale {
        args.push("--locale".to_string());
        args.push(locale.to_string());
    }

    if let Some(config) = spawn.config {
        args.push("--config".to_string());
        args.push(config.to_string_lossy().into_owned());
    }

    if spawn.orchestrator_mode {
        args.push("--orchestrator-mode".to_string());
    }

    args
}

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

    /// The client's `--locale` has to ride along in the daemon's argv: the
    /// daemon renders the UI, and a flag typed on the client's command line
    /// reaches it by no other route (#3149).
    #[test]
    fn server_args_forwards_the_clients_locale() {
        assert_eq!(
            server_args(&DaemonSpawn {
                session_name: Some("mysession"),
                locale: Some("ja"),
                ..Default::default()
            }),
            vec!["--server", "--session-name", "mysession", "--locale", "ja"]
        );
    }

    /// No `--locale` on the client means "let the daemon decide" — it reads
    /// the config file and the environment itself, and an empty `--locale`
    /// would override both.
    #[test]
    fn server_args_omits_locale_when_the_client_had_none() {
        let args = server_args(&DaemonSpawn {
            session_name: Some("mysession"),
            ..Default::default()
        });
        assert!(
            !args.iter().any(|a| a == "--locale"),
            "unexpected --locale in {args:?}"
        );
        assert!(
            !args.iter().any(|a| a == "--config"),
            "unexpected --config in {args:?}"
        );
    }

    /// A `--config` the user typed starts a daemon that reads that file:
    /// the daemon finds a config for itself otherwise, and would silently
    /// read the wrong one.
    #[test]
    fn server_args_forwards_the_clients_config_path() {
        assert_eq!(
            server_args(&DaemonSpawn {
                session_name: Some("mysession"),
                config: Some(Path::new("/tmp/alt/config.json")),
                ..Default::default()
            }),
            vec![
                "--server",
                "--session-name",
                "mysession",
                "--config",
                "/tmp/alt/config.json"
            ]
        );
    }

    #[test]
    fn server_args_carries_the_ssh_url_alongside_the_locale() {
        assert_eq!(
            server_args(&DaemonSpawn {
                ssh_url: Some("ssh://host/srv"),
                locale: Some("fr"),
                ..Default::default()
            }),
            vec!["--server", "--ssh-url", "ssh://host/srv", "--locale", "fr"]
        );
    }

    /// Orchestrator mode is a property of *how the client was launched* — a
    /// bare `fresh` — and the daemon is a separate process that cannot see
    /// that. If the flag does not ride along, the daemon boots the
    /// launch-directory workspace and "reopen what I was last in" silently
    /// degrades to the old behaviour.
    #[test]
    fn server_args_forwards_orchestrator_mode() {
        let args = server_args(&DaemonSpawn {
            session_name: Some("orchestrator"),
            orchestrator_mode: true,
            ..Default::default()
        });
        assert_eq!(
            args,
            vec![
                "--server",
                "--session-name",
                "orchestrator",
                "--orchestrator-mode"
            ]
        );
    }

    /// The other direction: an ordinary `fresh -a` daemon must not be told to
    /// jump to a workspace in some other directory.
    #[test]
    fn server_args_omits_orchestrator_mode_by_default() {
        let args = server_args(&DaemonSpawn {
            session_name: Some("mysession"),
            ..Default::default()
        });
        assert!(
            !args.iter().any(|a| a == "--orchestrator-mode"),
            "unexpected --orchestrator-mode in {args:?}"
        );
    }

    #[test]
    fn test_is_process_running() {
        // Current process should be running
        let pid = std::process::id();
        assert!(is_process_running(pid));

        // PID 999999999 is unlikely to exist
        assert!(!is_process_running(999999999));
    }
}
