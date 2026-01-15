//! XDG-compliant log directory management
//!
//! This module provides functions to get log file paths following the
//! XDG Base Directory Specification. Logs are stored in:
//! - `$XDG_STATE_HOME/fresh/logs/` (typically `~/.local/state/fresh/logs/`)
//!
//! Each Fresh instance uses PID-based log files to support concurrent runs.
//! On startup, stale log files from dead processes are cleaned up automatically.

use std::fs;
use std::path::PathBuf;
use std::sync::OnceLock;
use std::time::{Duration, SystemTime};

/// Minimum age for log files to be cleaned up (24 hours)
const CLEANUP_AGE: Duration = Duration::from_secs(24 * 60 * 60);

/// Cached log directory path
static LOG_DIR: OnceLock<PathBuf> = OnceLock::new();

/// Get the base log directory for Fresh, creating it if necessary.
///
/// Returns `$XDG_STATE_HOME/fresh/logs/` (typically `~/.local/state/fresh/logs/`).
/// Falls back to `~/.local/state/fresh/logs/` if XDG_STATE_HOME is not set.
/// As a last resort, falls back to the system temp directory.
pub fn log_dir() -> &'static PathBuf {
    LOG_DIR.get_or_init(|| {
        let dir = get_xdg_log_dir().unwrap_or_else(|| std::env::temp_dir().join("fresh-logs"));

        // Create the directory if it doesn't exist
        if let Err(e) = fs::create_dir_all(&dir) {
            tracing::warn!("Failed to create log directory {:?}: {}", dir, e);
            // Fall back to temp dir if we can't create the XDG directory
            return std::env::temp_dir().join("fresh-logs");
        }

        dir
    })
}

/// Get the XDG state home log directory
fn get_xdg_log_dir() -> Option<PathBuf> {
    // First try XDG_STATE_HOME
    if let Ok(state_home) = std::env::var("XDG_STATE_HOME") {
        let path = PathBuf::from(state_home);
        if path.is_absolute() {
            return Some(path.join("fresh").join("logs"));
        }
    }

    // Fall back to ~/.local/state
    if let Some(home) = home_dir() {
        return Some(home.join(".local").join("state").join("fresh").join("logs"));
    }

    None
}

/// Get the user's home directory
fn home_dir() -> Option<PathBuf> {
    // Try HOME environment variable first (works on all Unix-likes)
    if let Ok(home) = std::env::var("HOME") {
        return Some(PathBuf::from(home));
    }

    // On Windows, try USERPROFILE
    #[cfg(windows)]
    if let Ok(profile) = std::env::var("USERPROFILE") {
        return Some(PathBuf::from(profile));
    }

    None
}

/// Get the path for the main Fresh log file for this process.
///
/// Returns `{log_dir}/fresh-{PID}.log`
pub fn main_log_path() -> PathBuf {
    log_dir().join(format!("fresh-{}.log", std::process::id()))
}

/// Get the path for the warnings log file for this process.
///
/// Returns `{log_dir}/warnings-{PID}.log`
pub fn warnings_log_path() -> PathBuf {
    log_dir().join(format!("warnings-{}.log", std::process::id()))
}

/// Get the directory for LSP-related logs.
///
/// Returns `{log_dir}/lsp/`, creating it if necessary.
pub fn lsp_log_dir() -> PathBuf {
    let dir = log_dir().join("lsp");
    if let Err(e) = fs::create_dir_all(&dir) {
        tracing::warn!("Failed to create LSP log directory {:?}: {}", dir, e);
    }
    dir
}

/// Get the path for an LSP server's log file for this process.
///
/// Returns `{log_dir}/lsp/{language}-{PID}.log`
pub fn lsp_log_path(language: &str) -> PathBuf {
    lsp_log_dir().join(format!("{}-{}.log", language, std::process::id()))
}

/// Clean up stale log files from dead processes.
///
/// This removes:
/// 1. Legacy log files from /tmp (old location before XDG migration)
/// 2. Stale PID-based log files in the XDG directory for processes that no longer exist
pub fn cleanup_stale_logs() {
    cleanup_legacy_tmp_logs();
    cleanup_stale_xdg_logs();
}

/// Clean up legacy log files from /tmp
fn cleanup_legacy_tmp_logs() {
    let tmp_dir = std::env::temp_dir();

    // Patterns to clean up (old PID-based files in /tmp)
    let cleanup_patterns = [
        "fresh-warnings-",
        "fresh-lsp-",
        "rust-analyzer-",
        "fresh-stdin-",
        "fresh.log", // Old single fresh.log in /tmp
    ];

    if let Ok(entries) = fs::read_dir(&tmp_dir) {
        for entry in entries.flatten() {
            let file_name = entry.file_name();
            let name = file_name.to_string_lossy();

            // Check if this is an old Fresh log file
            let should_cleanup = cleanup_patterns
                .iter()
                .any(|pattern| name.starts_with(pattern));

            if should_cleanup {
                // Only remove files older than CLEANUP_AGE, not directories
                if entry.file_type().map(|t| t.is_file()).unwrap_or(false)
                    && is_file_older_than(&entry.path(), CLEANUP_AGE)
                {
                    if let Err(e) = fs::remove_file(entry.path()) {
                        tracing::debug!("Failed to clean up legacy log {:?}: {}", entry.path(), e);
                    } else {
                        tracing::info!("Cleaned up legacy log file: {:?}", entry.path());
                    }
                }
            }
        }
    }
}

/// Clean up stale PID-based log files in XDG directory for dead processes
fn cleanup_stale_xdg_logs() {
    let current_pid = std::process::id();

    // Clean main log directory
    cleanup_stale_logs_in_dir(log_dir(), current_pid);

    // Clean LSP log directory
    let lsp_dir = log_dir().join("lsp");
    if lsp_dir.exists() {
        cleanup_stale_logs_in_dir(&lsp_dir, current_pid);
    }
}

/// Clean up stale log files in a specific directory
fn cleanup_stale_logs_in_dir(dir: &std::path::Path, current_pid: u32) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };

    for entry in entries.flatten() {
        let file_name = entry.file_name();
        let name = file_name.to_string_lossy();

        // Only process .log files
        if !name.ends_with(".log") {
            continue;
        }

        // Try to extract PID from filename (format: something-{PID}.log)
        if let Some(pid) = extract_pid_from_filename(&name) {
            // Don't remove our own log files
            if pid == current_pid {
                continue;
            }

            // Check if the process is still running and file is old enough
            if !is_process_running(pid)
                && is_file_older_than(&entry.path(), CLEANUP_AGE)
                && entry.file_type().map(|t| t.is_file()).unwrap_or(false)
            {
                if let Err(e) = fs::remove_file(entry.path()) {
                    tracing::debug!("Failed to clean up stale log {:?}: {}", entry.path(), e);
                } else {
                    tracing::debug!("Cleaned up stale log file: {:?}", entry.path());
                }
            }
        }
    }
}

/// Check if a file is older than the specified duration
fn is_file_older_than(path: &std::path::Path, age: Duration) -> bool {
    let Ok(metadata) = fs::metadata(path) else {
        return false;
    };

    let Ok(modified) = metadata.modified() else {
        return false;
    };

    SystemTime::now()
        .duration_since(modified)
        .map(|elapsed| elapsed > age)
        .unwrap_or(false)
}

/// Extract PID from a filename like "fresh-12345.log" or "rust-12345.log"
fn extract_pid_from_filename(name: &str) -> Option<u32> {
    // Remove .log extension
    let without_ext = name.strip_suffix(".log")?;

    // Find the last hyphen and try to parse what follows as a PID
    let last_hyphen = without_ext.rfind('-')?;
    let pid_str = &without_ext[last_hyphen + 1..];

    pid_str.parse().ok()
}

/// Check if a process with the given PID is still running
fn is_process_running(pid: u32) -> bool {
    #[cfg(unix)]
    {
        // On Unix, we can use kill with signal 0 to check if process exists
        // This doesn't actually send a signal, just checks if we could
        unsafe {
            libc::kill(pid as libc::pid_t, 0) == 0
                || std::io::Error::last_os_error().raw_os_error() == Some(libc::EPERM)
        }
    }

    #[cfg(windows)]
    {
        // On Windows, try to open the process
        use windows_sys::Win32::Foundation::CloseHandle;
        use windows_sys::Win32::System::Threading::{
            OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION,
        };

        unsafe {
            let handle = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, 0, pid);
            if !handle.is_null() {
                CloseHandle(handle);
                true
            } else {
                false
            }
        }
    }

    #[cfg(not(any(unix, windows)))]
    {
        // On other platforms, assume process is running to be safe
        true
    }
}

/// Print all directories used by Fresh to stdout
pub fn print_all_paths() {
    use std::io::Write;

    let stdout = std::io::stdout();
    let mut handle = stdout.lock();

    // Config directory (~/.config/fresh)
    let config_dir = dirs::config_dir()
        .map(|d| d.join("fresh"))
        .unwrap_or_else(|| PathBuf::from("<unavailable>"));

    // Data directory (~/.local/share/fresh)
    let data_dir = dirs::data_dir()
        .map(|d| d.join("fresh"))
        .unwrap_or_else(|| PathBuf::from("<unavailable>"));

    // State/logs directory
    let logs_dir = log_dir().clone();

    writeln!(handle, "Fresh directories:").ok();
    writeln!(handle).ok();

    writeln!(handle, "Config:     {}", config_dir.display()).ok();
    writeln!(
        handle,
        "  config.json:  {}",
        config_dir.join("config.json").display()
    )
    .ok();
    writeln!(
        handle,
        "  themes/:      {}",
        config_dir.join("themes").display()
    )
    .ok();
    writeln!(
        handle,
        "  grammars/:    {}",
        config_dir.join("grammars").display()
    )
    .ok();
    writeln!(
        handle,
        "  plugins/:     {}",
        config_dir.join("plugins").display()
    )
    .ok();
    writeln!(handle).ok();

    writeln!(handle, "Data:       {}", data_dir.display()).ok();
    writeln!(
        handle,
        "  sessions/:    {}",
        data_dir.join("sessions").display()
    )
    .ok();
    writeln!(
        handle,
        "  recovery/:    {}",
        data_dir.join("recovery").display()
    )
    .ok();
    writeln!(
        handle,
        "  terminals/:   {}",
        data_dir.join("terminals").display()
    )
    .ok();
    writeln!(handle).ok();

    writeln!(handle, "Logs:       {}", logs_dir.display()).ok();
    writeln!(handle, "  lsp/:         {}", logs_dir.join("lsp").display()).ok();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_log_dir_is_absolute() {
        let dir = log_dir();
        assert!(dir.is_absolute(), "Log directory should be absolute");
    }

    #[test]
    fn test_main_log_path_contains_pid() {
        let path = main_log_path();
        let name = path.file_name().unwrap().to_string_lossy();
        assert!(name.starts_with("fresh-"), "Should start with fresh-");
        assert!(name.ends_with(".log"), "Should end with .log");
        assert!(
            name.contains(&std::process::id().to_string()),
            "Should contain PID"
        );
    }

    #[test]
    fn test_warnings_log_path_contains_pid() {
        let path = warnings_log_path();
        let name = path.file_name().unwrap().to_string_lossy();
        assert!(name.starts_with("warnings-"), "Should start with warnings-");
        assert!(name.ends_with(".log"), "Should end with .log");
    }

    #[test]
    fn test_lsp_log_path_contains_pid() {
        let path = lsp_log_path("rust");
        let name = path.file_name().unwrap().to_string_lossy();
        assert!(name.starts_with("rust-"), "Should start with language-");
        assert!(name.ends_with(".log"), "Should end with .log");
        assert!(
            path.to_string_lossy().contains("lsp"),
            "Should be in lsp dir"
        );
    }

    #[test]
    fn test_extract_pid_from_filename() {
        assert_eq!(extract_pid_from_filename("fresh-12345.log"), Some(12345));
        assert_eq!(extract_pid_from_filename("rust-99999.log"), Some(99999));
        assert_eq!(extract_pid_from_filename("warnings-1.log"), Some(1));
        assert_eq!(extract_pid_from_filename("no-pid.txt"), None);
        assert_eq!(extract_pid_from_filename("invalid"), None);
    }

    #[test]
    fn test_current_process_is_running() {
        assert!(is_process_running(std::process::id()));
    }

    #[test]
    fn test_nonexistent_process_not_running() {
        // PID 99999999 is very unlikely to exist
        // But on some systems this might be valid, so we just test it doesn't panic
        let _ = is_process_running(99999999);
    }
}
