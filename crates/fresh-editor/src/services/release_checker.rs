//! Release checker: is a newer version available, and how should the user get
//! it?
//!
//! The provenance resolution ("how was this copy installed?") and the
//! update-command registry now live in the `fresh-update` crate; version
//! comparison and release-feed parsing moved there too. This module keeps only
//! the editor-side concerns: the HTTP fetch (`services::http`), the daily
//! debounce (`services::telemetry` stamp file), and the background thread that
//! surfaces the result to the UI.

use super::time_source::SharedTimeSource;
use std::path::PathBuf;
use std::sync::mpsc::{self, Receiver, TryRecvError};
use std::thread::{self, JoinHandle};
use std::time::Duration;

pub use fresh_update::{Provenance, UpdateKind, UpdatePlan};

/// The current version of the editor
pub const CURRENT_VERSION: &str = env!("CARGO_PKG_VERSION");

/// Default GitHub releases API URL for the fresh editor
pub const DEFAULT_RELEASES_URL: &str = "https://api.github.com/repos/sinelaw/fresh/releases/latest";

/// Result of checking for a new release
#[derive(Debug, Clone)]
pub struct ReleaseCheckResult {
    /// The latest version available
    pub latest_version: String,
    /// Whether an update is available
    pub update_available: bool,
    /// How this copy of `fresh` was installed (drives the update command).
    pub provenance: Provenance,
}

impl ReleaseCheckResult {
    /// The concrete update action for this install (command to run, or the
    /// self-contained/manual fallback). See `fresh_update::registry`.
    pub fn update_plan(&self) -> UpdatePlan {
        fresh_update::plan(&self.provenance)
    }
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

/// Handle to an update checker running in the background.
///
/// Runs a single check at startup (if not already done today).
/// Results are available via `poll_result()`.
pub struct UpdateChecker {
    /// Receiver for update check results
    receiver: Receiver<Result<ReleaseCheckResult, String>>,
    /// Background thread handle
    #[allow(dead_code)]
    thread: JoinHandle<()>,
    /// Last successful result (cached)
    last_result: Option<ReleaseCheckResult>,
}

/// Backwards compatibility alias
pub type PeriodicUpdateChecker = UpdateChecker;

impl UpdateChecker {
    /// Poll for a new update check result without blocking.
    ///
    /// Returns `Some(result)` if a new check completed, `None` if no new result.
    /// Successful results are cached and can be retrieved via `get_cached_result()`.
    pub fn poll_result(&mut self) -> Option<Result<ReleaseCheckResult, String>> {
        match self.receiver.try_recv() {
            Ok(result) => {
                if let Ok(ref release_result) = result {
                    tracing::debug!(
                        "Update check completed: update_available={}",
                        release_result.update_available
                    );
                    self.last_result = Some(release_result.clone());
                }
                Some(result)
            }
            Err(TryRecvError::Empty) => None,
            Err(TryRecvError::Disconnected) => None,
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

/// Start an update checker that runs once at startup.
///
/// The check respects daily debouncing via the stamp file - if already
/// checked today, no network request is made.
/// Results are available via `poll_result()` on the returned handle.
pub fn start_periodic_update_check(
    releases_url: &str,
    time_source: SharedTimeSource,
    data_dir: PathBuf,
) -> UpdateChecker {
    tracing::debug!("Starting update checker");
    let url = releases_url.to_string();
    let (tx, rx) = mpsc::channel();

    let handle = thread::spawn(move || {
        if let Some(unique_id) =
            super::telemetry::should_run_daily_check(time_source.as_ref(), &data_dir)
        {
            super::telemetry::track_open(&unique_id);
            let result = check_for_update(&url);
            // Receiver may be dropped if checker is dropped before result arrives.
            #[allow(clippy::let_underscore_must_use)]
            let _ = tx.send(result);
        }
    });

    UpdateChecker {
        receiver: rx,
        thread: handle,
        last_result: None,
    }
}

/// Start an update checker (for testing with custom parameters).
#[doc(hidden)]
pub fn start_periodic_update_check_with_interval(
    releases_url: &str,
    _check_interval: Duration,
    time_source: SharedTimeSource,
    data_dir: PathBuf,
) -> UpdateChecker {
    // check_interval is ignored - debouncing is handled by stamp file
    start_periodic_update_check(releases_url, time_source, data_dir)
}

/// Start a background update check
///
/// Returns a handle that can be used to query the result later.
/// The check runs in a background thread and won't block.
/// Respects daily debouncing - if already checked today, no result will be sent.
pub fn start_update_check(
    releases_url: &str,
    time_source: SharedTimeSource,
    data_dir: PathBuf,
) -> UpdateCheckHandle {
    tracing::debug!("Starting background update check");
    let url = releases_url.to_string();
    let (tx, rx) = mpsc::channel();

    let handle = thread::spawn(move || {
        if let Some(unique_id) =
            super::telemetry::should_run_daily_check(time_source.as_ref(), &data_dir)
        {
            super::telemetry::track_open(&unique_id);
            let result = check_for_update(&url);
            // Receiver may be dropped if handle is dropped before result arrives.
            #[allow(clippy::let_underscore_must_use)]
            let _ = tx.send(result);
        }
    });

    UpdateCheckHandle {
        receiver: rx,
        thread: handle,
    }
}

/// Fetches release information from the provided URL.
///
/// The HTTP/TLS transport lives in `services::http`; without the `http`
/// feature that call returns an error and we surface it here unchanged.
pub fn fetch_latest_version(url: &str) -> Result<String, String> {
    tracing::debug!("Fetching latest version from {}", url);
    let body = super::http::get_release_json(url)?;
    let version = parse_version_from_json(&body)?;
    tracing::debug!("Latest version: {}", version);
    Ok(version)
}

/// Parse the version from a GitHub releases API body.
///
/// Thin wrapper over `fresh_update::version::parse_tag_name` kept because
/// callers/tests use the `Result` shape.
fn parse_version_from_json(json: &str) -> Result<String, String> {
    fresh_update::version::parse_tag_name(json)
        .ok_or_else(|| "tag_name not found in response".to_string())
}

/// Compare two versions; `true` if `latest` is newer than `current`.
/// Delegates to `fresh_update::version`.
pub fn is_newer_version(current: &str, latest: &str) -> bool {
    fresh_update::version::is_newer(current, latest)
}

/// Detect how this copy of `fresh` was installed.
///
/// Delegates entirely to `fresh_update::resolve()` (receipt → embedded channel
/// → path heuristic). See `docs/internal/packaging-self-update.md`.
pub fn detect_provenance() -> Provenance {
    fresh_update::resolve()
}

/// The update plan for a given provenance (thin re-export of the registry).
pub fn plan_for(prov: &Provenance) -> UpdatePlan {
    fresh_update::plan(prov)
}

/// Check for a new release (blocking).
///
/// Fetches the release feed here (HTTP lives in `services::http`) and hands the
/// body to `fresh_update::check::evaluate`, which parses it, compares versions,
/// and resolves provenance.
pub fn check_for_update(releases_url: &str) -> Result<ReleaseCheckResult, String> {
    let body = super::http::get_release_json(releases_url)?;
    let check = fresh_update::check::evaluate(CURRENT_VERSION, &body)?;

    tracing::debug!(
        current = CURRENT_VERSION,
        latest = %check.latest_version,
        update_available = check.update_available,
        channel = %check.provenance.channel,
        confidence = ?check.provenance.confidence,
        "Release check complete"
    );

    Ok(ReleaseCheckResult {
        latest_version: check.latest_version,
        update_available: check.update_available,
        provenance: check.provenance,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_newer_version() {
        // (current, latest, expected_newer)
        let cases = [
            ("0.1.26", "1.0.0", true),        // major bump
            ("0.1.26", "0.2.0", true),        // minor bump
            ("0.1.26", "0.1.27", true),       // patch bump
            ("0.1.26", "0.1.26", false),      // same
            ("0.1.26", "0.1.25", false),      // older patch
            ("0.2.0", "0.1.26", false),       // older minor
            ("1.0.0", "0.1.26", false),       // older major
            ("0.1.26-alpha", "0.1.27", true), // prerelease current
            ("0.1.26", "0.1.27-beta", true),  // prerelease latest
        ];
        for (current, latest, expected) in cases {
            assert_eq!(
                is_newer_version(current, latest),
                expected,
                "is_newer_version({:?}, {:?})",
                current,
                latest
            );
        }
    }

    // Install-method detection now lives in `fresh_update::heuristic` and
    // `fresh_update::provenance` (see that crate's tests). release_checker only
    // delegates, so there is nothing path-specific to test here.

    #[test]
    fn test_parse_version_from_json() {
        // Various JSON formats should all parse correctly
        let cases = [
            (r#"{"tag_name": "v0.1.27"}"#, "0.1.27"),
            (r#"{"tag_name": "0.1.27"}"#, "0.1.27"),
            (
                r#"{"tag_name": "v0.2.0", "name": "v0.2.0", "draft": false}"#,
                "0.2.0",
            ),
        ];
        for (json, expected) in cases {
            assert_eq!(parse_version_from_json(json).unwrap(), expected);
        }

        // Verify mock version is detected as newer than current
        let version = parse_version_from_json(r#"{"tag_name": "v99.0.0"}"#).unwrap();
        assert!(is_newer_version(CURRENT_VERSION, &version));
    }

    #[test]
    fn test_current_version_is_valid() {
        let parts: Vec<&str> = CURRENT_VERSION.split('.').collect();
        assert!(parts.len() >= 2, "Version should have at least major.minor");
        assert!(parts[0].parse::<u32>().is_ok());
        assert!(parts[1].parse::<u32>().is_ok());
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
                        drop(request.respond(response));
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
    fn test_update_checker_detects_new_version() {
        let (stop_tx, url) = start_mock_release_server("99.0.0");
        let time_source = super::super::time_source::TestTimeSource::shared();
        let temp_dir = tempfile::tempdir().unwrap();

        let mut checker =
            start_periodic_update_check(&url, time_source, temp_dir.path().to_path_buf());

        // Wait for result
        let start = std::time::Instant::now();
        while start.elapsed() < Duration::from_secs(2) {
            if checker.poll_result().is_some() {
                break;
            }
            thread::sleep(Duration::from_millis(10));
        }

        assert!(checker.is_update_available());
        assert_eq!(checker.latest_version(), Some("99.0.0"));

        stop_tx.send(()).ok();
    }

    #[test]
    fn test_update_checker_no_update_when_current() {
        let (stop_tx, url) = start_mock_release_server(CURRENT_VERSION);
        let time_source = super::super::time_source::TestTimeSource::shared();
        let temp_dir = tempfile::tempdir().unwrap();

        let mut checker =
            start_periodic_update_check(&url, time_source, temp_dir.path().to_path_buf());

        // Wait for result
        let start = std::time::Instant::now();
        while start.elapsed() < Duration::from_secs(2) {
            if checker.poll_result().is_some() {
                break;
            }
            thread::sleep(Duration::from_millis(10));
        }

        assert!(!checker.is_update_available());
        assert!(checker.latest_version().is_none());
        assert!(checker.get_cached_result().is_some());

        stop_tx.send(()).ok();
    }

    #[test]
    fn test_update_checker_api_before_result() {
        let (stop_tx, url) = start_mock_release_server("99.0.0");
        let time_source = super::super::time_source::TestTimeSource::shared();
        let temp_dir = tempfile::tempdir().unwrap();

        let checker = start_periodic_update_check(&url, time_source, temp_dir.path().to_path_buf());

        // Immediately check (before result arrives)
        assert!(!checker.is_update_available());
        assert!(checker.latest_version().is_none());
        assert!(checker.get_cached_result().is_none());

        stop_tx.send(()).ok();
    }
}
