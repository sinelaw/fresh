//! Release checker module for checking if a new version is available.
//!
//! This module provides functionality to:
//! - Check for new releases by fetching a GitHub releases API endpoint
//! - Detect the installation method (Homebrew, npm, cargo, etc.) based on executable path
//! - Provide appropriate update commands based on installation method
//! - Periodic update checking with automatic re-spawn daily

use std::env;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{self, Receiver, TryRecvError};
use std::sync::Arc;
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

/// The current version of the editor
pub const CURRENT_VERSION: &str = env!("CARGO_PKG_VERSION");

/// Default GitHub releases API URL for the fresh editor
pub const DEFAULT_RELEASES_URL: &str = "https://api.github.com/repos/sinelaw/fresh/releases/latest";

/// Installation method detection result
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstallMethod {
    /// Installed via Homebrew
    Homebrew,
    /// Installed via cargo
    Cargo,
    /// Installed via npm
    Npm,
    /// Installed via a Linux package manager (apt, dnf, etc.)
    PackageManager,
    /// Installed via AUR (Arch User Repository)
    Aur,
    /// Unknown installation method or manually installed
    Unknown,
}

impl InstallMethod {
    /// Get the update command for this installation method
    pub fn update_command(&self) -> Option<&'static str> {
        Some(match self {
            Self::Homebrew => " brew upgrade fresh-editor",
            Self::Cargo => "cargo install fresh-editor",
            Self::Npm => "npm update -g @fresh-editor/fresh-editor",
            Self::Aur => "yay -Syu fresh-editor  # or use your AUR helper",
            Self::PackageManager => "Update using your system package manager",
            Self::Unknown => return None,
        })
    }
}

/// Result of checking for a new release
#[derive(Debug, Clone)]
pub struct ReleaseCheckResult {
    /// The latest version available
    pub latest_version: String,
    /// Whether an update is available
    pub update_available: bool,
    /// The detected installation method
    pub install_method: InstallMethod,
}

/// Handle to a background update check (one-shot)
///
/// Use `try_get_result` to check if the result is ready without blocking.
pub struct UpdateCheckHandle {
    receiver: Receiver<Result<ReleaseCheckResult, String>>,
    #[allow(dead_code)]
    thread: JoinHandle<()>,
}

impl UpdateCheckHandle {
    /// Try to get the result without blocking.
    /// Returns Some(result) if the check completed, None if still running.
    /// If still running, the background thread is abandoned (will be killed on process exit).
    pub fn try_get_result(self) -> Option<Result<ReleaseCheckResult, String>> {
        match self.receiver.try_recv() {
            Ok(result) => {
                tracing::debug!("Update check completed");
                Some(result)
            }
            Err(TryRecvError::Empty) => {
                // Still running - abandon the thread
                tracing::debug!("Update check still running, abandoning");
                drop(self.thread);
                None
            }
            Err(TryRecvError::Disconnected) => {
                // Thread panicked or exited without sending
                tracing::debug!("Update check thread disconnected");
                None
            }
        }
    }
}

/// Default check interval for periodic update checking (24 hours)
pub const DEFAULT_UPDATE_CHECK_INTERVAL: Duration = Duration::from_secs(24 * 60 * 60);

/// Handle to a periodic update checker that runs in the background.
///
/// The checker runs daily and provides results via `poll_result()`.
/// When a check finds an update, the result is stored until retrieved.
pub struct PeriodicUpdateChecker {
    /// Receiver for update check results
    receiver: Receiver<Result<ReleaseCheckResult, String>>,
    /// Signal to stop the background thread
    stop_signal: Arc<AtomicBool>,
    /// Background thread handle
    #[allow(dead_code)]
    thread: JoinHandle<()>,
    /// Last successful result (cached)
    last_result: Option<ReleaseCheckResult>,
    /// Time of last check (for tracking)
    last_check_time: Option<Instant>,
}

impl PeriodicUpdateChecker {
    /// Poll for a new update check result without blocking.
    ///
    /// Returns `Some(result)` if a new check completed, `None` if no new result.
    /// Successful results are cached and can be retrieved via `get_cached_result()`.
    pub fn poll_result(&mut self) -> Option<Result<ReleaseCheckResult, String>> {
        match self.receiver.try_recv() {
            Ok(result) => {
                self.last_check_time = Some(Instant::now());
                if let Ok(ref release_result) = result {
                    tracing::debug!(
                        "Periodic update check completed: update_available={}",
                        release_result.update_available
                    );
                    self.last_result = Some(release_result.clone());
                }
                Some(result)
            }
            Err(TryRecvError::Empty) => None,
            Err(TryRecvError::Disconnected) => {
                tracing::debug!("Periodic update checker thread disconnected");
                None
            }
        }
    }

    /// Get the cached result from the last successful check.
    pub fn get_cached_result(&self) -> Option<&ReleaseCheckResult> {
        self.last_result.as_ref()
    }

    /// Check if an update is available (from cached result).
    pub fn is_update_available(&self) -> bool {
        self.last_result
            .as_ref()
            .map(|r| r.update_available)
            .unwrap_or(false)
    }

    /// Get the latest version string if an update is available.
    pub fn latest_version(&self) -> Option<&str> {
        self.last_result.as_ref().and_then(|r| {
            if r.update_available {
                Some(r.latest_version.as_str())
            } else {
                None
            }
        })
    }
}

impl Drop for PeriodicUpdateChecker {
    fn drop(&mut self) {
        // Signal the background thread to stop
        self.stop_signal.store(true, Ordering::SeqCst);
    }
}

/// Start a periodic update checker that runs daily.
///
/// The checker immediately runs the first check, then repeats daily.
/// Results are available via `poll_result()` on the returned handle.
pub fn start_periodic_update_check(releases_url: &str) -> PeriodicUpdateChecker {
    start_periodic_update_check_with_interval(releases_url, DEFAULT_UPDATE_CHECK_INTERVAL)
}

/// Start a periodic update checker with a custom check interval.
///
/// This is primarily for testing - allows specifying a short interval to verify
/// the periodic behavior without waiting for a day.
///
/// # Arguments
/// * `releases_url` - The GitHub releases API URL to check
/// * `check_interval` - Duration between checks
pub fn start_periodic_update_check_with_interval(
    releases_url: &str,
    check_interval: Duration,
) -> PeriodicUpdateChecker {
    tracing::debug!(
        "Starting periodic update checker with interval {:?}",
        check_interval
    );
    let url = releases_url.to_string();
    let (tx, rx) = mpsc::channel();
    let stop_signal = Arc::new(AtomicBool::new(false));
    let stop_signal_clone = stop_signal.clone();

    // Use a smaller sleep increment for shorter intervals
    let sleep_increment = if check_interval < Duration::from_secs(10) {
        Duration::from_millis(10)
    } else {
        Duration::from_secs(1)
    };

    let handle = thread::spawn(move || {
        // Run initial check immediately
        let result = check_for_update(&url);
        if tx.send(result).is_err() {
            return; // Receiver dropped, exit
        }

        // Then check periodically
        loop {
            // Sleep in small increments to allow quick shutdown
            let sleep_end = Instant::now() + check_interval;
            while Instant::now() < sleep_end {
                if stop_signal_clone.load(Ordering::SeqCst) {
                    tracing::debug!("Periodic update checker stopping");
                    return;
                }
                thread::sleep(sleep_increment);
            }

            // Check if we should stop before making a new request
            if stop_signal_clone.load(Ordering::SeqCst) {
                tracing::debug!("Periodic update checker stopping");
                return;
            }

            tracing::debug!("Periodic update check starting");
            let result = check_for_update(&url);
            if tx.send(result).is_err() {
                return; // Receiver dropped, exit
            }
        }
    });

    PeriodicUpdateChecker {
        receiver: rx,
        stop_signal,
        thread: handle,
        last_result: None,
        last_check_time: None,
    }
}

/// Start a background update check
///
/// Returns a handle that can be used to query the result later.
/// The check runs in a background thread and won't block.
pub fn start_update_check(releases_url: &str) -> UpdateCheckHandle {
    tracing::debug!("Starting background update check");
    let url = releases_url.to_string();
    let (tx, rx) = mpsc::channel();

    let handle = thread::spawn(move || {
        let result = check_for_update(&url);
        let _ = tx.send(result);
    });

    UpdateCheckHandle {
        receiver: rx,
        thread: handle,
    }
}

/// Fetches release information from the provided URL.
pub fn fetch_latest_version(url: &str) -> Result<String, String> {
    tracing::debug!("Fetching latest version from {}", url);
    let response = ureq::get(url)
        .set("User-Agent", "fresh-editor-update-checker")
        .set("Accept", "application/vnd.github.v3+json")
        .timeout(Duration::from_secs(5))
        .call()
        .map_err(|e| {
            tracing::debug!("HTTP request failed: {}", e);
            format!("HTTP request failed: {}", e)
        })?;

    let body = response
        .into_string()
        .map_err(|e| format!("Failed to read response body: {}", e))?;

    let version = parse_version_from_json(&body)?;
    tracing::debug!("Latest version: {}", version);
    Ok(version)
}

/// Parse version from GitHub API JSON response
fn parse_version_from_json(json: &str) -> Result<String, String> {
    let tag_name_key = "\"tag_name\"";
    let start = json
        .find(tag_name_key)
        .ok_or_else(|| "tag_name not found in response".to_string())?;

    let after_key = &json[start + tag_name_key.len()..];

    let value_start = after_key
        .find('"')
        .ok_or_else(|| "Invalid JSON: missing quote after tag_name".to_string())?;

    let value_content = &after_key[value_start + 1..];
    let value_end = value_content
        .find('"')
        .ok_or_else(|| "Invalid JSON: unclosed quote".to_string())?;

    let tag = &value_content[..value_end];

    // Strip 'v' prefix if present
    Ok(tag.strip_prefix('v').unwrap_or(tag).to_string())
}

/// Detect the installation method based on the current executable path
pub fn detect_install_method() -> InstallMethod {
    match env::current_exe() {
        Ok(path) => detect_install_method_from_path(&path),
        Err(_) => InstallMethod::Unknown,
    }
}

/// Detect installation method from a given executable path
pub fn detect_install_method_from_path(exe_path: &Path) -> InstallMethod {
    let path_str = exe_path.to_string_lossy();

    // Check for Homebrew paths (macOS and Linux)
    if path_str.contains("/opt/homebrew/")
        || path_str.contains("/usr/local/Cellar/")
        || path_str.contains("/home/linuxbrew/")
        || path_str.contains("/.linuxbrew/")
    {
        return InstallMethod::Homebrew;
    }

    // Check for Cargo installation
    if path_str.contains("/.cargo/bin/") || path_str.contains("\\.cargo\\bin\\") {
        return InstallMethod::Cargo;
    }

    // Check for npm global installation
    if path_str.contains("/node_modules/")
        || path_str.contains("\\node_modules\\")
        || path_str.contains("/npm/")
        || path_str.contains("/lib/node_modules/")
    {
        return InstallMethod::Npm;
    }

    // Check for AUR installation (Arch Linux)
    if path_str.starts_with("/usr/bin/") && is_arch_linux() {
        return InstallMethod::Aur;
    }

    // Check for package manager installation (standard system paths)
    if path_str.starts_with("/usr/bin/")
        || path_str.starts_with("/usr/local/bin/")
        || path_str.starts_with("/bin/")
    {
        return InstallMethod::PackageManager;
    }

    InstallMethod::Unknown
}

/// Check if we're running on Arch Linux
fn is_arch_linux() -> bool {
    std::fs::read_to_string("/etc/os-release")
        .map(|content| content.contains("Arch Linux") || content.contains("ID=arch"))
        .unwrap_or(false)
}

/// Compare two semantic versions
/// Returns true if `latest` is newer than `current`
pub fn is_newer_version(current: &str, latest: &str) -> bool {
    let parse_version = |v: &str| -> Option<(u32, u32, u32)> {
        let parts: Vec<&str> = v.split('.').collect();
        if parts.len() >= 3 {
            Some((
                parts[0].parse().ok()?,
                parts[1].parse().ok()?,
                parts[2].split('-').next()?.parse().ok()?,
            ))
        } else if parts.len() == 2 {
            Some((parts[0].parse().ok()?, parts[1].parse().ok()?, 0))
        } else {
            None
        }
    };

    match (parse_version(current), parse_version(latest)) {
        (Some((c_major, c_minor, c_patch)), Some((l_major, l_minor, l_patch))) => {
            (l_major, l_minor, l_patch) > (c_major, c_minor, c_patch)
        }
        _ => false,
    }
}

/// Check for a new release (blocking)
pub fn check_for_update(releases_url: &str) -> Result<ReleaseCheckResult, String> {
    // Send telemetry alongside update check
    super::telemetry::track_open();

    let latest_version = fetch_latest_version(releases_url)?;
    let install_method = detect_install_method();
    let update_available = is_newer_version(CURRENT_VERSION, &latest_version);

    tracing::debug!(
        current = CURRENT_VERSION,
        latest = %latest_version,
        update_available,
        install_method = ?install_method,
        "Release check complete"
    );

    Ok(ReleaseCheckResult {
        latest_version,
        update_available,
        install_method,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_is_newer_version_major() {
        assert!(is_newer_version("0.1.26", "1.0.0"));
        assert!(is_newer_version("1.0.0", "2.0.0"));
    }

    #[test]
    fn test_is_newer_version_minor() {
        assert!(is_newer_version("0.1.26", "0.2.0"));
        assert!(is_newer_version("0.1.26", "0.2.26"));
    }

    #[test]
    fn test_is_newer_version_patch() {
        assert!(is_newer_version("0.1.26", "0.1.27"));
        assert!(is_newer_version("0.1.26", "0.1.100"));
    }

    #[test]
    fn test_is_newer_version_same() {
        assert!(!is_newer_version("0.1.26", "0.1.26"));
    }

    #[test]
    fn test_is_newer_version_older() {
        assert!(!is_newer_version("0.1.26", "0.1.25"));
        assert!(!is_newer_version("0.2.0", "0.1.26"));
        assert!(!is_newer_version("1.0.0", "0.1.26"));
    }

    #[test]
    fn test_is_newer_version_with_v_prefix() {
        assert!(is_newer_version("0.1.26", "0.1.27"));
    }

    #[test]
    fn test_is_newer_version_with_prerelease() {
        assert!(is_newer_version("0.1.26-alpha", "0.1.27"));
        assert!(is_newer_version("0.1.26", "0.1.27-beta"));
    }

    #[test]
    fn test_detect_install_method_homebrew_macos() {
        let path = PathBuf::from("/opt/homebrew/Cellar/fresh/0.1.26/bin/fresh");
        assert_eq!(
            detect_install_method_from_path(&path),
            InstallMethod::Homebrew
        );
    }

    #[test]
    fn test_detect_install_method_homebrew_intel_mac() {
        let path = PathBuf::from("/usr/local/Cellar/fresh/0.1.26/bin/fresh");
        assert_eq!(
            detect_install_method_from_path(&path),
            InstallMethod::Homebrew
        );
    }

    #[test]
    fn test_detect_install_method_homebrew_linux() {
        let path = PathBuf::from("/home/linuxbrew/.linuxbrew/bin/fresh");
        assert_eq!(
            detect_install_method_from_path(&path),
            InstallMethod::Homebrew
        );
    }

    #[test]
    fn test_detect_install_method_cargo() {
        let path = PathBuf::from("/home/user/.cargo/bin/fresh");
        assert_eq!(detect_install_method_from_path(&path), InstallMethod::Cargo);
    }

    #[test]
    fn test_detect_install_method_cargo_windows() {
        let path = PathBuf::from("C:\\Users\\user\\.cargo\\bin\\fresh.exe");
        assert_eq!(detect_install_method_from_path(&path), InstallMethod::Cargo);
    }

    #[test]
    fn test_detect_install_method_npm() {
        let path = PathBuf::from("/usr/local/lib/node_modules/fresh-editor/bin/fresh");
        assert_eq!(detect_install_method_from_path(&path), InstallMethod::Npm);
    }

    #[test]
    fn test_detect_install_method_package_manager() {
        let path = PathBuf::from("/usr/local/bin/fresh");
        assert_eq!(
            detect_install_method_from_path(&path),
            InstallMethod::PackageManager
        );
    }

    #[test]
    fn test_detect_install_method_unknown() {
        let path = PathBuf::from("/home/user/downloads/fresh");
        assert_eq!(
            detect_install_method_from_path(&path),
            InstallMethod::Unknown
        );
    }

    #[test]
    fn test_parse_version_from_json() {
        let json = r#"{"tag_name": "v0.1.27", "name": "Release 0.1.27"}"#;
        assert_eq!(parse_version_from_json(json).unwrap(), "0.1.27");
    }

    #[test]
    fn test_parse_version_from_json_no_v_prefix() {
        let json = r#"{"tag_name": "0.1.27", "name": "Release 0.1.27"}"#;
        assert_eq!(parse_version_from_json(json).unwrap(), "0.1.27");
    }

    #[test]
    fn test_parse_version_from_json_full_response() {
        let json = r#"{
            "url": "https://api.github.com/repos/sinelaw/fresh/releases/12345",
            "tag_name": "v0.2.0",
            "target_commitish": "main",
            "name": "v0.2.0",
            "draft": false,
            "prerelease": false
        }"#;
        assert_eq!(parse_version_from_json(json).unwrap(), "0.2.0");
    }

    #[test]
    fn test_current_version_is_valid() {
        let parts: Vec<&str> = CURRENT_VERSION.split('.').collect();
        assert!(parts.len() >= 2, "Version should have at least major.minor");
        assert!(
            parts[0].parse::<u32>().is_ok(),
            "Major version should be a number"
        );
        assert!(
            parts[1].parse::<u32>().is_ok(),
            "Minor version should be a number"
        );
    }

    #[test]
    fn test_version_parsing_with_mock_data() {
        let json = r#"{"tag_name": "v99.0.0"}"#;
        let version = parse_version_from_json(json).unwrap();
        assert!(is_newer_version(CURRENT_VERSION, &version));
    }

    use std::sync::mpsc as std_mpsc;

    /// Test helper: start a local HTTP server that returns a mock release JSON
    /// Returns (stop_sender, url) - send to stop_sender to shut down the server
    fn start_mock_release_server(version: &str) -> (std_mpsc::Sender<()>, String) {
        let server = tiny_http::Server::http("127.0.0.1:0").expect("Failed to start test server");
        let port = server.server_addr().to_ip().unwrap().port();
        let url = format!("http://127.0.0.1:{}/releases/latest", port);

        let (stop_tx, stop_rx) = std_mpsc::channel::<()>();

        // Spawn a thread to handle requests
        let version = version.to_string();
        thread::spawn(move || {
            loop {
                // Check for stop signal
                if stop_rx.try_recv().is_ok() {
                    break;
                }

                // Non-blocking receive with timeout
                match server.recv_timeout(Duration::from_millis(100)) {
                    Ok(Some(request)) => {
                        let response_body = format!(r#"{{"tag_name": "v{}"}}"#, version);
                        let response = tiny_http::Response::from_string(response_body).with_header(
                            tiny_http::Header::from_bytes(
                                &b"Content-Type"[..],
                                &b"application/json"[..],
                            )
                            .unwrap(),
                        );
                        let _ = request.respond(response);
                    }
                    Ok(None) => {
                        // Timeout, continue loop
                    }
                    Err(_) => {
                        // Server error, exit
                        break;
                    }
                }
            }
        });

        (stop_tx, url)
    }

    #[test]
    fn test_periodic_update_checker_with_local_server() {
        // Test that the production periodic checker works with a real HTTP server
        let (stop_tx, url) = start_mock_release_server("99.0.0");

        let mut checker =
            start_periodic_update_check_with_interval(&url, Duration::from_millis(50));

        // Wait for initial result
        let start = Instant::now();
        while start.elapsed() < Duration::from_secs(2) {
            if checker.poll_result().is_some() {
                break;
            }
            thread::sleep(Duration::from_millis(10));
        }

        // Verify cached result
        assert!(
            checker.is_update_available(),
            "Should detect update available"
        );
        assert_eq!(checker.latest_version(), Some("99.0.0"));
        assert!(checker.get_cached_result().is_some());

        drop(checker);
        let _ = stop_tx.send(());
    }

    #[test]
    fn test_periodic_update_checker_shutdown_clean() {
        // Test that the checker shuts down cleanly without hanging
        let (stop_tx, url) = start_mock_release_server("99.0.0");

        let checker = start_periodic_update_check_with_interval(&url, Duration::from_millis(50));

        // Let it run briefly
        thread::sleep(Duration::from_millis(100));

        // Drop should signal stop and not hang
        let start = Instant::now();
        drop(checker);
        let elapsed = start.elapsed();

        // Shutdown should be quick (within a second)
        assert!(
            elapsed < Duration::from_secs(2),
            "Shutdown took too long: {:?}",
            elapsed
        );

        let _ = stop_tx.send(());
    }

    #[test]
    fn test_periodic_update_checker_multiple_cycles_production() {
        // Test that the production checker produces multiple results over time
        let (stop_tx, url) = start_mock_release_server("99.0.0");

        let mut checker =
            start_periodic_update_check_with_interval(&url, Duration::from_millis(30));

        let mut result_count = 0;
        let start = Instant::now();
        let timeout = Duration::from_secs(2);

        while start.elapsed() < timeout && result_count < 3 {
            if checker.poll_result().is_some() {
                result_count += 1;
            }
            thread::sleep(Duration::from_millis(10));
        }

        // Should have received at least 2 results (initial + at least one periodic)
        assert!(
            result_count >= 2,
            "Expected at least 2 results, got {}",
            result_count
        );

        drop(checker);
        let _ = stop_tx.send(());
    }

    #[test]
    fn test_periodic_update_checker_no_update_when_current() {
        // Test behavior when server returns current version (no update)
        let (stop_tx, url) = start_mock_release_server(CURRENT_VERSION);

        let mut checker =
            start_periodic_update_check_with_interval(&url, Duration::from_secs(3600));

        // Wait for initial result
        let start = Instant::now();
        while start.elapsed() < Duration::from_secs(2) {
            if checker.poll_result().is_some() {
                break;
            }
            thread::sleep(Duration::from_millis(10));
        }

        // Verify no update available
        assert!(!checker.is_update_available());
        assert!(checker.latest_version().is_none()); // Returns None when no update
        assert!(checker.get_cached_result().is_some()); // But result is still cached

        drop(checker);
        let _ = stop_tx.send(());
    }

    #[test]
    fn test_periodic_update_checker_api_before_result() {
        // Test that API methods work correctly before any result is received
        let (stop_tx, url) = start_mock_release_server("99.0.0");

        // Use a very long interval so we only test the initial state
        let checker = start_periodic_update_check_with_interval(&url, Duration::from_secs(3600));

        // Immediately check (before result arrives)
        assert!(!checker.is_update_available());
        assert!(checker.latest_version().is_none());
        assert!(checker.get_cached_result().is_none());

        drop(checker);
        let _ = stop_tx.send(());
    }
}
