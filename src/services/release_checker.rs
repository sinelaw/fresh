//! Release checker module for checking if a new version is available.
//!
//! This module provides functionality to:
//! - Check for new releases by fetching a GitHub releases API endpoint
//! - Detect the installation method (Homebrew, npm, cargo, etc.) based on executable path
//! - Provide appropriate update commands based on installation method

use std::env;
use std::path::Path;
use std::sync::mpsc::{self, Receiver};
use std::thread::{self, JoinHandle};
use std::time::Duration;

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
        match self {
            InstallMethod::Homebrew => Some("brew upgrade fresh"),
            InstallMethod::Cargo => Some("cargo install fresh-editor"),
            InstallMethod::Npm => Some("npm update -g fresh-editor"),
            InstallMethod::Aur => Some("yay -Syu fresh-editor  # or use your AUR helper"),
            InstallMethod::PackageManager => Some("Update using your system package manager"),
            InstallMethod::Unknown => None,
        }
    }
}

/// Result of checking for a new release
#[derive(Debug)]
pub struct ReleaseCheckResult {
    /// The latest version available
    pub latest_version: String,
    /// Whether an update is available
    pub update_available: bool,
    /// The detected installation method
    pub install_method: InstallMethod,
}

/// Handle to a background update check
///
/// Use `try_get_result` to check if the result is ready without blocking.
pub struct UpdateCheckHandle {
    receiver: Receiver<Result<ReleaseCheckResult, String>>,
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
            Err(mpsc::TryRecvError::Empty) => {
                // Still running - abandon the thread
                tracing::debug!("Update check still running, abandoning");
                drop(self.thread);
                None
            }
            Err(mpsc::TryRecvError::Disconnected) => {
                // Thread panicked or exited without sending
                tracing::debug!("Update check thread disconnected");
                None
            }
        }
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
    fn test_update_commands() {
        assert_eq!(
            InstallMethod::Homebrew.update_command(),
            Some("brew upgrade fresh")
        );
        assert_eq!(
            InstallMethod::Cargo.update_command(),
            Some("cargo install fresh-editor")
        );
        assert_eq!(
            InstallMethod::Npm.update_command(),
            Some("npm update -g fresh-editor")
        );
        assert!(InstallMethod::Unknown.update_command().is_none());
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
}
