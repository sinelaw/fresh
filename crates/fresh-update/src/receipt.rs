//! The install receipt: `install-receipt.toml`.
//!
//! Written at install time by whatever installed `fresh` (a packaged file for
//! OS/package-manager channels, a sidecar file for wrapper channels), and read
//! at runtime to resolve provenance authoritatively. See
//! `docs/internal/packaging-self-update.md` §5.

use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// The receipt filename, searched for in [`candidate_paths`].
pub const RECEIPT_FILE_NAME: &str = "install-receipt.toml";

/// The package-name directory OS packages ship their data under. The binary is
/// `fresh` but the package is `fresh-editor`, so `.deb`/`.rpm` receipts land in
/// `/usr/share/fresh-editor/` rather than `/usr/share/fresh/`.
pub const PACKAGE_DIR_NAME: &str = "fresh-editor";

/// The current receipt schema version this build understands.
pub const CURRENT_SCHEMA: u32 = 1;

/// Channel-specific hints used to build the exact update invocation. All
/// fields are optional; the registry supplies sensible defaults when a hint is
/// absent.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Hints {
    /// Homebrew tap (e.g. `sinelaw/homebrew-fresh`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tap: Option<String>,
    /// Homebrew formula name.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub formula: Option<String>,
    /// AUR package name (`fresh-editor` or `fresh-editor-bin`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub aur_pkg: Option<String>,
    /// Preferred AUR helper (`yay`, `paru`, …). **Retained for wire
    /// compatibility only — no longer consulted.** An AUR receipt means
    /// `pacman` + `makepkg`, and the update uses that one route on every
    /// machine rather than varying with what happens to be installed.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub aur_helper: Option<String>,
    /// winget package id.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub winget_id: Option<String>,
    /// Flatpak application ref.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub flatpak_ref: Option<String>,
    /// npm package name.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub npm_pkg: Option<String>,
    /// Target triple this artifact was built for (self-update asset selection).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub target: Option<String>,
    /// The architecture as the *packaging tool* spells it (`amd64` for a
    /// `.deb`, `x86_64` for a `.rpm`). Recorded by the packaging pipeline so
    /// the update path never has to re-derive it from the target triple — the
    /// installer knows this for certain, the running binary can only infer it.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub pkg_arch: Option<String>,
    /// Release asset filename recorded at install time. **Retained for wire
    /// compatibility only — no longer consulted.** It names the artifact of the
    /// version already installed, so it cannot identify the one to fetch next;
    /// the asset is resolved from the release feed (packages) or computed from
    /// the compiled-in target triple (archives).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub asset: Option<String>,
    /// Install root to swap for AppImage/tarball self-update.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub install_root: Option<String>,
}

/// A parsed install receipt.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct InstallReceipt {
    /// Receipt schema version.
    pub schema: u32,
    /// Canonical channel id (see [`crate::Channel::id`]).
    pub channel: String,
    /// Version this receipt was written for.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
    /// Package name (usually `fresh-editor`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub package_name: Option<String>,
    /// RFC3339 install timestamp.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub installed_at: Option<String>,
    /// `true` if an external manager owns updates (do not self-swap).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub managed: Option<bool>,
    /// `true` if `fresh update` may swap the binary in place.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub self_update: Option<bool>,
    /// Channel-specific hints. Serialized last so it renders as `[hints]`.
    #[serde(default)]
    pub hints: Hints,
}

impl InstallReceipt {
    /// Construct a minimal receipt for `channel` at the current schema.
    pub fn new(channel: &str) -> Self {
        InstallReceipt {
            schema: CURRENT_SCHEMA,
            channel: channel.to_string(),
            version: None,
            package_name: Some("fresh-editor".to_string()),
            installed_at: None,
            managed: None,
            self_update: None,
            hints: Hints::default(),
        }
    }

    /// Parse a receipt from TOML text.
    pub fn parse(text: &str) -> Result<Self, toml::de::Error> {
        toml::from_str(text)
    }

    /// Serialize the receipt to TOML text (for installers to write).
    pub fn to_toml(&self) -> Result<String, toml::ser::Error> {
        toml::to_string_pretty(self)
    }
}

/// The ordered list of paths where a receipt may live, given the running
/// executable and the per-user data directory. First existing + parseable
/// file wins. Kept pure (no filesystem access) so it is unit-testable.
pub fn candidate_paths(exe: &Path, data_dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    if let Some(dir) = exe.parent() {
        // 1. sidecar in the same directory as the binary
        out.push(dir.join(RECEIPT_FILE_NAME));
        if let Some(prefix) = dir.parent() {
            // 2. FHS-style, binary name: <prefix>/share/fresh/
            //    (Homebrew formula, Flatpak manifest, Nix derivation)
            out.push(prefix.join("share").join("fresh").join(RECEIPT_FILE_NAME));
            // 3. FHS-style, *package* name: <prefix>/share/fresh-editor/
            //    (.deb and .rpm — OS packages put their data under
            //    /usr/share/<package>, and the package is `fresh-editor`
            //    even though the binary is `fresh`)
            out.push(
                prefix
                    .join("share")
                    .join(PACKAGE_DIR_NAME)
                    .join(RECEIPT_FILE_NAME),
            );
            // 4. node/npm layout: <prefix>/lib/fresh/
            out.push(prefix.join("lib").join("fresh").join(RECEIPT_FILE_NAME));
        }
    }
    // 5. per-user data dir fallback
    out.push(data_dir.join("fresh").join(RECEIPT_FILE_NAME));
    out
}

/// Search [`candidate_paths`] and return the first parseable receipt together
/// with the path it came from.
pub fn find(exe: &Path, data_dir: &Path) -> Option<(PathBuf, InstallReceipt)> {
    for path in candidate_paths(exe, data_dir) {
        let Ok(text) = std::fs::read_to_string(&path) else {
            continue;
        };
        match InstallReceipt::parse(&text) {
            Ok(receipt) => {
                tracing::debug!(path = %path.display(), channel = %receipt.channel, "found install receipt");
                return Some((path, receipt));
            }
            Err(e) => {
                tracing::warn!(path = %path.display(), error = %e, "ignoring unparseable install receipt");
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_full_receipt() {
        let text = r#"
schema = 1
channel = "homebrew"
version = "0.4.4"
package_name = "fresh-editor"
installed_at = "2026-07-23T10:30:00Z"
managed = true
self_update = false

[hints]
tap = "sinelaw/homebrew-fresh"
formula = "fresh-editor"
"#;
        let r = InstallReceipt::parse(text).unwrap();
        assert_eq!(r.schema, 1);
        assert_eq!(r.channel, "homebrew");
        assert_eq!(r.managed, Some(true));
        assert_eq!(r.self_update, Some(false));
        assert_eq!(r.hints.tap.as_deref(), Some("sinelaw/homebrew-fresh"));
        assert_eq!(r.hints.formula.as_deref(), Some("fresh-editor"));
    }

    #[test]
    fn parse_minimal_receipt() {
        // Only the two required fields; everything else defaults.
        let r = InstallReceipt::parse("schema = 1\nchannel = \"tarball\"\n").unwrap();
        assert_eq!(r.channel, "tarball");
        assert_eq!(r.managed, None);
        assert_eq!(r.hints, Hints::default());
    }

    #[test]
    fn roundtrip_serialize_parse() {
        let mut r = InstallReceipt::new("winget");
        r.managed = Some(true);
        r.self_update = Some(false);
        r.hints.winget_id = Some("sinelaw.fresh-editor".to_string());
        let text = r.to_toml().unwrap();
        let back = InstallReceipt::parse(&text).unwrap();
        assert_eq!(r, back);
        // `[hints]` must render as a table, not inline before scalars.
        assert!(text.contains("[hints]"), "serialized:\n{text}");
    }

    #[test]
    fn candidate_paths_are_ordered() {
        let exe = Path::new("/opt/homebrew/bin/fresh");
        let data = Path::new("/home/u/.local/share");
        let paths = candidate_paths(exe, data);
        assert_eq!(
            paths[0],
            Path::new("/opt/homebrew/bin/install-receipt.toml")
        );
        assert_eq!(
            paths[1],
            Path::new("/opt/homebrew/share/fresh/install-receipt.toml")
        );
        assert_eq!(
            paths[2],
            Path::new("/opt/homebrew/share/fresh-editor/install-receipt.toml")
        );
        assert_eq!(
            paths[3],
            Path::new("/opt/homebrew/lib/fresh/install-receipt.toml")
        );
        assert_eq!(
            *paths.last().unwrap(),
            Path::new("/home/u/.local/share/fresh/install-receipt.toml")
        );
    }

    /// The layout an installed `.deb`/`.rpm` actually produces: binary at
    /// `/usr/bin/fresh`, receipt at `/usr/share/fresh-editor/`. Searching only
    /// `share/fresh/` missed it, so every apt/dnf install fell through to
    /// `Channel::Unknown` and was offered no update path at all.
    #[test]
    fn candidate_paths_cover_the_packaged_deb_rpm_location() {
        let paths = candidate_paths(
            Path::new("/usr/bin/fresh"),
            Path::new("/home/u/.local/share"),
        );
        assert!(
            paths.contains(&PathBuf::from(
                "/usr/share/fresh-editor/install-receipt.toml"
            )),
            "packaged .deb/.rpm receipt path not searched; got {paths:#?}"
        );
    }

    /// End-to-end over the real packaged layout: a receipt written where
    /// `debian/rules` and the `generate-rpm` asset list put it must be found.
    #[test]
    fn find_reads_packaged_receipt_from_share_package_name() {
        let dir = tempfile::tempdir().unwrap();
        let usr = dir.path().join("usr");
        std::fs::create_dir_all(usr.join("bin")).unwrap();
        let exe = usr.join("bin").join("fresh");
        std::fs::write(&exe, b"not really a binary").unwrap();

        // Exactly where debian/rules and [package.metadata.generate-rpm] ship it.
        let share = usr.join("share").join(PACKAGE_DIR_NAME);
        std::fs::create_dir_all(&share).unwrap();
        std::fs::write(
            share.join(RECEIPT_FILE_NAME),
            "schema = 1\nchannel = \"apt\"\nmanaged = true\nself_update = false\n",
        )
        .unwrap();

        let (found_path, receipt) = find(&exe, Path::new("/nonexistent"))
            .expect("packaged receipt at share/fresh-editor/ must resolve");
        assert_eq!(found_path, share.join(RECEIPT_FILE_NAME));
        assert_eq!(receipt.channel, "apt");
    }

    #[test]
    fn find_reads_first_existing() {
        let dir = tempfile::tempdir().unwrap();
        let bin = dir.path().join("bin");
        std::fs::create_dir_all(&bin).unwrap();
        let exe = bin.join("fresh");
        std::fs::write(&exe, b"not really a binary").unwrap();

        // Put a receipt in <prefix>/share/fresh/ (candidate #2).
        let share = dir.path().join("share").join("fresh");
        std::fs::create_dir_all(&share).unwrap();
        std::fs::write(
            share.join(RECEIPT_FILE_NAME),
            "schema = 1\nchannel = \"flatpak\"\n",
        )
        .unwrap();

        let (found_path, receipt) = find(&exe, Path::new("/nonexistent")).unwrap();
        assert_eq!(receipt.channel, "flatpak");
        assert_eq!(found_path, share.join(RECEIPT_FILE_NAME));
    }
}
