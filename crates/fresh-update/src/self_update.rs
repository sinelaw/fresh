//! The in-place self-update engine for [`UpdateKind::SelfContained`] channels
//! (raw tarball, AppImage).
//!
//! Scope: this crate owns the parts that must be correct and are pure enough to
//! test — **checksum verification** and the **atomic binary swap** (including
//! the Windows rename-the-running-exe dance). It deliberately does *not*
//! perform network I/O or archive extraction: the caller (the editor, which
//! already has an HTTP + TLS stack in `services::http`) fetches the bytes and,
//! for tar/zip archives, extracts the inner binary, then hands the verified
//! executable bytes to [`atomic_replace`]. AppImages need no extraction, so the
//! whole flow is usable directly for them.
//!
//! See `docs/internal/packaging-self-update.md` §8.

use crate::provenance::Provenance;
use sha2::{Digest, Sha256};
use std::fmt;
use std::path::{Path, PathBuf};

/// Errors from the self-update engine.
#[derive(Debug)]
pub enum UpdateError {
    /// An underlying filesystem error.
    Io(std::io::Error),
    /// The downloaded bytes did not match the expected checksum.
    ChecksumMismatch { expected: String, actual: String },
    /// The resolved provenance is not eligible for an in-place swap.
    NotSelfUpdatable,
}

impl fmt::Display for UpdateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UpdateError::Io(e) => write!(f, "io error: {e}"),
            UpdateError::ChecksumMismatch { expected, actual } => {
                write!(f, "checksum mismatch: expected {expected}, got {actual}")
            }
            UpdateError::NotSelfUpdatable => {
                write!(
                    f,
                    "this install cannot self-update; use the package manager"
                )
            }
        }
    }
}

impl std::error::Error for UpdateError {}

impl From<std::io::Error> for UpdateError {
    fn from(e: std::io::Error) -> Self {
        UpdateError::Io(e)
    }
}

/// Whether this provenance is eligible for an automatic in-place swap:
/// self-update-capable *and* resolved at a trustworthy confidence.
pub fn can_self_update(prov: &Provenance) -> bool {
    prov.self_update && prov.confidence.allows_self_swap()
}

/// Lower-case hex encoding, no external `hex` crate.
fn to_hex(bytes: &[u8]) -> String {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut s = String::with_capacity(bytes.len() * 2);
    for &b in bytes {
        s.push(HEX[(b >> 4) as usize] as char);
        s.push(HEX[(b & 0x0f) as usize] as char);
    }
    s
}

/// Compute the SHA-256 of `bytes` as lower-case hex.
pub fn sha256_hex(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    to_hex(&hasher.finalize())
}

/// Verify `bytes` against an expected SHA-256. The expected string may be a
/// bare hex digest or a `sha256sum`-style line (`<hex>␠␠<filename>`); only the
/// first whitespace-delimited token is compared, case-insensitively.
pub fn verify_sha256(bytes: &[u8], expected: &str) -> Result<(), UpdateError> {
    let expected_hex = expected
        .split_whitespace()
        .next()
        .unwrap_or("")
        .trim_start_matches("sha256:")
        .to_ascii_lowercase();
    let actual = sha256_hex(bytes);
    if actual == expected_hex {
        Ok(())
    } else {
        Err(UpdateError::ChecksumMismatch {
            expected: expected_hex,
            actual,
        })
    }
}

/// The temp path used while staging a new binary, alongside `target` so the
/// final rename stays on the same filesystem (and is therefore atomic).
fn staging_path(target: &Path) -> PathBuf {
    let name = target
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| "fresh".to_string());
    let dir = target.parent().unwrap_or_else(|| Path::new("."));
    dir.join(format!(".{name}.new-{}", std::process::id()))
}

/// The path the previous binary is moved aside to on Windows before the swap.
#[cfg_attr(not(windows), allow(dead_code))]
fn backup_path(target: &Path) -> PathBuf {
    let name = target
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| "fresh".to_string());
    let dir = target.parent().unwrap_or_else(|| Path::new("."));
    dir.join(format!(".{name}.old"))
}

/// Atomically replace the executable at `target` with `new_bytes`.
///
/// The new bytes are written to a sibling temp file (same directory, so the
/// rename is atomic), made executable, then moved into place:
///
/// * **Unix** — a single `rename()` over `target`.
/// * **Windows** — a running `.exe` cannot be deleted but *can* be renamed, so
///   we move the current binary aside to `<name>.old`, move the new one in, and
///   leave the stale `.old` for [`cleanup_previous`] to remove on next launch.
pub fn atomic_replace(target: &Path, new_bytes: &[u8]) -> Result<(), UpdateError> {
    let staging = staging_path(target);
    std::fs::write(&staging, new_bytes)?;
    set_executable(&staging)?;

    let result = do_swap(target, &staging);
    if result.is_err() {
        // Best-effort cleanup of the staged file on failure.
        let _ = std::fs::remove_file(&staging);
    }
    result
}

#[cfg(not(windows))]
fn do_swap(target: &Path, staging: &Path) -> Result<(), UpdateError> {
    std::fs::rename(staging, target)?;
    Ok(())
}

#[cfg(windows)]
fn do_swap(target: &Path, staging: &Path) -> Result<(), UpdateError> {
    let backup = backup_path(target);
    // Remove any leftover backup from a previous update first.
    let _ = std::fs::remove_file(&backup);
    if target.exists() {
        std::fs::rename(target, &backup)?;
    }
    match std::fs::rename(staging, target) {
        Ok(()) => Ok(()),
        Err(e) => {
            // Roll back: restore the original binary.
            let _ = std::fs::rename(&backup, target);
            Err(UpdateError::Io(e))
        }
    }
}

/// Remove the leftover `<name>.old` backup from a prior Windows self-update.
/// No-op on Unix. Call once at startup.
pub fn cleanup_previous(target: &Path) {
    let _ = target;
    #[cfg(windows)]
    {
        let _ = std::fs::remove_file(backup_path(target));
    }
}

#[cfg(unix)]
fn set_executable(path: &Path) -> std::io::Result<()> {
    use std::os::unix::fs::PermissionsExt;
    let mut perms = std::fs::metadata(path)?.permissions();
    perms.set_mode(0o755);
    std::fs::set_permissions(path, perms)
}

#[cfg(not(unix))]
fn set_executable(_path: &Path) -> std::io::Result<()> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::channel::Channel;
    use crate::confidence::Confidence;

    #[test]
    fn sha256_matches_known_vector() {
        // echo -n "" | sha256sum
        assert_eq!(
            sha256_hex(b""),
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        );
        // echo -n "abc" | sha256sum
        assert_eq!(
            sha256_hex(b"abc"),
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        );
    }

    #[test]
    fn verify_accepts_bare_and_sumfile_forms() {
        let bytes = b"hello fresh";
        let digest = sha256_hex(bytes);
        assert!(verify_sha256(bytes, &digest).is_ok());
        assert!(verify_sha256(bytes, &digest.to_uppercase()).is_ok());
        assert!(verify_sha256(bytes, &format!("{digest}  fresh.tar.xz")).is_ok());
        assert!(verify_sha256(bytes, &format!("sha256:{digest}")).is_ok());
    }

    #[test]
    fn verify_rejects_mismatch() {
        let err = verify_sha256(b"data", "deadbeef").unwrap_err();
        assert!(matches!(err, UpdateError::ChecksumMismatch { .. }));
    }

    #[test]
    fn atomic_replace_swaps_contents() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("fresh");
        std::fs::write(&target, b"old version").unwrap();

        atomic_replace(&target, b"new version").unwrap();
        assert_eq!(std::fs::read(&target).unwrap(), b"new version");

        // No staging file left behind.
        let leftovers: Vec<_> = std::fs::read_dir(dir.path())
            .unwrap()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_name().to_string_lossy().contains(".new-"))
            .collect();
        assert!(leftovers.is_empty(), "staging file not cleaned up");
    }

    #[cfg(unix)]
    #[test]
    fn atomic_replace_sets_executable_bit() {
        use std::os::unix::fs::PermissionsExt;
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("fresh");
        std::fs::write(&target, b"x").unwrap();
        atomic_replace(&target, b"binary").unwrap();
        let mode = std::fs::metadata(&target).unwrap().permissions().mode();
        assert_eq!(mode & 0o111, 0o111, "executable bits not set: {mode:o}");
    }

    #[test]
    fn can_self_update_requires_capability_and_confidence() {
        // Tarball at authoritative confidence: yes.
        let ok = Provenance::for_channel(Channel::Tarball, Confidence::Authoritative);
        assert!(can_self_update(&ok));

        // Tarball but only heuristic confidence: no.
        let low = Provenance::for_channel(Channel::Tarball, Confidence::Heuristic);
        assert!(!can_self_update(&low));

        // Homebrew (managed) at any confidence: no.
        let brew = Provenance::for_channel(Channel::Homebrew, Confidence::Authoritative);
        assert!(!can_self_update(&brew));
    }
}
