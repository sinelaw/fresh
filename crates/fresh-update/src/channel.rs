//! The canonical set of distribution channels `fresh` ships through.
//!
//! Every channel has a stable string id (used in the install receipt and the
//! `FRESH_BUILD_CHANNEL` / `FRESH_INSTALL_CHANNEL` env vars). The id is the
//! wire format; the enum is the in-memory form. Keep the two in sync.

use std::fmt;
use std::str::FromStr;

/// A distribution channel. See `docs/internal/packaging-self-update.md` §3 for
/// the full inventory and how each one records its provenance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Channel {
    /// Homebrew (tap `sinelaw/homebrew-fresh`).
    Homebrew,
    /// npm package `@fresh-editor/fresh-editor`.
    Npm,
    /// crates.io source build.
    Cargo,
    /// `cargo binstall` (prebuilt archive fetched by cargo-binstall).
    CargoBinstall,
    /// AUR source package `fresh-editor`.
    Aur,
    /// AUR binary package `fresh-editor-bin`.
    AurBin,
    /// Debian/Ubuntu `.deb` (apt/dpkg).
    Apt,
    /// Fedora/RHEL `.rpm` (dnf/rpm).
    Dnf,
    /// openSUSE `.rpm` (zypper).
    Zypper,
    /// Arch official repo (hypothetical) via pacman.
    Pacman,
    /// Flatpak (`io.github.sinelaw.fresh`).
    Flatpak,
    /// Snap.
    Snap,
    /// AppImage (single-file, self-updatable).
    Appimage,
    /// Windows winget (`sinelaw.fresh-editor`).
    Winget,
    /// Windows Scoop.
    Scoop,
    /// Windows Chocolatey.
    Chocolatey,
    /// Nix flake / profile.
    Nix,
    /// FreeBSD pkg / ports.
    FreebsdPkg,
    /// mise (`github:sinelaw/fresh`).
    Mise,
    /// A raw GitHub release archive extracted by hand (self-updatable).
    Tarball,
    /// Built from a local git checkout (developer build).
    Source,
    /// The shared prebuilt archive with no more specific provenance yet.
    /// Resolves like `Tarball` for update purposes but signals "a wrapper may
    /// not have written its receipt".
    Prebuilt,
    /// Provenance could not be determined.
    Unknown,
}

impl Channel {
    /// The stable string id used on the wire (receipt / env vars).
    pub const fn id(self) -> &'static str {
        match self {
            Channel::Homebrew => "homebrew",
            Channel::Npm => "npm",
            Channel::Cargo => "cargo",
            Channel::CargoBinstall => "cargo-binstall",
            Channel::Aur => "aur",
            Channel::AurBin => "aur-bin",
            Channel::Apt => "apt",
            Channel::Dnf => "dnf",
            Channel::Zypper => "zypper",
            Channel::Pacman => "pacman",
            Channel::Flatpak => "flatpak",
            Channel::Snap => "snap",
            Channel::Appimage => "appimage",
            Channel::Winget => "winget",
            Channel::Scoop => "scoop",
            Channel::Chocolatey => "chocolatey",
            Channel::Nix => "nix",
            Channel::FreebsdPkg => "freebsd-pkg",
            Channel::Mise => "mise",
            Channel::Tarball => "tarball",
            Channel::Source => "source",
            Channel::Prebuilt => "prebuilt",
            Channel::Unknown => "unknown",
        }
    }

    /// Parse a channel id, accepting a handful of friendly aliases. Returns
    /// `None` for an unrecognised string (callers map that to
    /// [`Channel::Unknown`] where appropriate).
    pub fn from_id(s: &str) -> Option<Self> {
        let normalized = s.trim().to_ascii_lowercase();
        Some(match normalized.as_str() {
            "homebrew" | "brew" => Channel::Homebrew,
            "npm" => Channel::Npm,
            "cargo" | "crates.io" | "cratesio" => Channel::Cargo,
            "cargo-binstall" | "binstall" => Channel::CargoBinstall,
            "aur" => Channel::Aur,
            "aur-bin" => Channel::AurBin,
            "apt" | "deb" | "dpkg" | "debian" => Channel::Apt,
            "dnf" | "rpm" | "yum" | "fedora" => Channel::Dnf,
            "zypper" | "opensuse" | "suse" => Channel::Zypper,
            "pacman" => Channel::Pacman,
            "flatpak" => Channel::Flatpak,
            "snap" | "snapcraft" => Channel::Snap,
            "appimage" => Channel::Appimage,
            "winget" => Channel::Winget,
            "scoop" => Channel::Scoop,
            "chocolatey" | "choco" => Channel::Chocolatey,
            "nix" | "nixos" => Channel::Nix,
            "freebsd-pkg" | "freebsd" | "pkg" => Channel::FreebsdPkg,
            "mise" | "asdf" => Channel::Mise,
            "tarball" | "archive" | "github" => Channel::Tarball,
            "source" | "git" => Channel::Source,
            "prebuilt" => Channel::Prebuilt,
            "unknown" | "" => Channel::Unknown,
            _ => return None,
        })
    }

    /// A short human-readable label for UI (e.g. status bar / `config paths`).
    pub const fn label(self) -> &'static str {
        match self {
            Channel::Homebrew => "Homebrew",
            Channel::Npm => "npm",
            Channel::Cargo => "cargo (crates.io)",
            Channel::CargoBinstall => "cargo-binstall",
            Channel::Aur => "AUR (source)",
            Channel::AurBin => "AUR (binary)",
            Channel::Apt => "APT (.deb)",
            Channel::Dnf => "DNF (.rpm)",
            Channel::Zypper => "zypper",
            Channel::Pacman => "pacman",
            Channel::Flatpak => "Flatpak",
            Channel::Snap => "Snap",
            Channel::Appimage => "AppImage",
            Channel::Winget => "winget",
            Channel::Scoop => "Scoop",
            Channel::Chocolatey => "Chocolatey",
            Channel::Nix => "Nix",
            Channel::FreebsdPkg => "FreeBSD pkg",
            Channel::Mise => "mise",
            Channel::Tarball => "release archive",
            Channel::Source => "source build",
            Channel::Prebuilt => "prebuilt archive",
            Channel::Unknown => "unknown",
        }
    }
}

impl fmt::Display for Channel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.id())
    }
}

/// Error returned when a channel id cannot be parsed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseChannelError(pub String);

impl fmt::Display for ParseChannelError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "unknown install channel id: {:?}", self.0)
    }
}

impl std::error::Error for ParseChannelError {}

impl FromStr for Channel {
    type Err = ParseChannelError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Channel::from_id(s).ok_or_else(|| ParseChannelError(s.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn id_roundtrips_through_from_id() {
        let all = [
            Channel::Homebrew,
            Channel::Npm,
            Channel::Cargo,
            Channel::CargoBinstall,
            Channel::Aur,
            Channel::AurBin,
            Channel::Apt,
            Channel::Dnf,
            Channel::Zypper,
            Channel::Pacman,
            Channel::Flatpak,
            Channel::Snap,
            Channel::Appimage,
            Channel::Winget,
            Channel::Scoop,
            Channel::Chocolatey,
            Channel::Nix,
            Channel::FreebsdPkg,
            Channel::Mise,
            Channel::Tarball,
            Channel::Source,
            Channel::Prebuilt,
            Channel::Unknown,
        ];
        for ch in all {
            assert_eq!(Channel::from_id(ch.id()), Some(ch), "roundtrip {}", ch.id());
        }
    }

    #[test]
    fn aliases_resolve() {
        assert_eq!(Channel::from_id("brew"), Some(Channel::Homebrew));
        assert_eq!(Channel::from_id("DEB"), Some(Channel::Apt));
        assert_eq!(Channel::from_id("choco"), Some(Channel::Chocolatey));
        assert_eq!(Channel::from_id("crates.io"), Some(Channel::Cargo));
        assert_eq!(Channel::from_id("  Flatpak  "), Some(Channel::Flatpak));
    }

    #[test]
    fn unknown_id_is_none() {
        assert_eq!(Channel::from_id("banana"), None);
        assert!("banana".parse::<Channel>().is_err());
    }
}
