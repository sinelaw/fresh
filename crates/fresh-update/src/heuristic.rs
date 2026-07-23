//! The demoted, best-effort path heuristic (Layer D).
//!
//! This is the *last* resort — used only when no receipt and no embedded
//! channel are available. It mirrors the historical
//! `release_checker::detect_install_method_from_path` logic so behaviour for
//! existing installs is unchanged, but its results are marked
//! [`crate::Confidence::Heuristic`] and never trigger a self-swap.

use crate::channel::Channel;
use std::path::Path;

/// Guess the channel from an executable path. `is_arch_linux` disambiguates
/// the `/usr/bin` case (kept pure for testing).
pub fn detect_from_path(exe_path: &Path, is_arch_linux: bool) -> Channel {
    let path_str = exe_path.to_string_lossy();

    // Homebrew (macOS + linuxbrew).
    if path_str.contains("/opt/homebrew/")
        || path_str.contains("/usr/local/Cellar/")
        || path_str.contains("/home/linuxbrew/")
        || path_str.contains("/.linuxbrew/")
    {
        return Channel::Homebrew;
    }

    // Cargo.
    if path_str.contains("/.cargo/bin/") || path_str.contains("\\.cargo\\bin\\") {
        return Channel::Cargo;
    }

    // npm global.
    if path_str.contains("/node_modules/")
        || path_str.contains("\\node_modules\\")
        || path_str.contains("/npm/")
        || path_str.contains("/lib/node_modules/")
    {
        return Channel::Npm;
    }

    // A system path on Arch is most likely an AUR install (the historical
    // guess). On other distros we cannot tell apt from dnf from a manual drop,
    // so we decline to guess and return Unknown — a receipt is required to be
    // sure.
    if path_str.starts_with("/usr/bin/") && is_arch_linux {
        return Channel::AurBin;
    }

    Channel::Unknown
}

/// Read `/etc/os-release` to decide whether the host is Arch Linux. Best-effort
/// and only meaningful on Linux.
pub fn host_is_arch_linux() -> bool {
    std::fs::read_to_string("/etc/os-release")
        .map(|content| content.contains("Arch Linux") || content.contains("ID=arch"))
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn detects_known_paths() {
        let cases = [
            (
                "/opt/homebrew/Cellar/fresh/0.4.4/bin/fresh",
                Channel::Homebrew,
            ),
            ("/usr/local/Cellar/fresh/0.4.4/bin/fresh", Channel::Homebrew),
            ("/home/linuxbrew/.linuxbrew/bin/fresh", Channel::Homebrew),
            ("/home/u/.cargo/bin/fresh", Channel::Cargo),
            ("C:\\Users\\u\\.cargo\\bin\\fresh.exe", Channel::Cargo),
            (
                "/usr/local/lib/node_modules/fresh-editor/bin/fresh",
                Channel::Npm,
            ),
            ("/home/u/downloads/fresh", Channel::Unknown),
        ];
        for (path, expected) in cases {
            assert_eq!(
                detect_from_path(&PathBuf::from(path), false),
                expected,
                "detect_from_path({path:?})"
            );
        }
    }

    #[test]
    fn usr_bin_on_arch_is_aur() {
        assert_eq!(
            detect_from_path(&PathBuf::from("/usr/bin/fresh"), true),
            Channel::AurBin
        );
        // …but on a non-Arch host we decline to guess.
        assert_eq!(
            detect_from_path(&PathBuf::from("/usr/bin/fresh"), false),
            Channel::Unknown
        );
    }
}
