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

/// Create the temp file used while staging a new binary, alongside `target` so
/// the final rename stays on the same filesystem (and is therefore atomic).
///
/// Three properties, each closing something the previous `.{name}.new-{pid}`
/// plus `fs::write` did not:
///
/// * **Unpredictable name.** The pid is guessable and, on a machine where the
///   install directory is writable by more than its owner — `/usr/local/bin` is
///   group-writable on plenty of macOS setups, and any shared install
///   qualifies — an attacker could pre-create the path.
/// * **`O_NOFOLLOW` + `create_new`.** `fs::write` opens with `O_CREAT|O_TRUNC`
///   and *follows symlinks*, so a pre-created symlink meant our 0755 content
///   landed wherever it pointed. Refusing to open anything that already exists,
///   and refusing to traverse a final symlink, removes that entirely; if the
///   name is taken we try another and eventually fail closed, which is a denial
///   of service rather than a compromise.
/// * **Mode at creation.** 0600 while we write, widened on the descriptor once
///   the contents are complete — never a window where a partially written file
///   is executable.
fn staging_file(target: &Path) -> Result<(PathBuf, std::fs::File), UpdateError> {
    let name = target
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| "fresh".to_string());
    let dir = target.parent().unwrap_or_else(|| Path::new("."));
    for attempt in 0..16u32 {
        let nonce = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.subsec_nanos())
            .unwrap_or(0)
            ^ attempt.wrapping_mul(0x9E37_79B9);
        let path = dir.join(format!(".{name}.new-{nonce:08x}"));
        match create_exclusive(&path) {
            Ok(file) => return Ok((path, file)),
            Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(e) => return Err(UpdateError::Io(e)),
        }
    }
    Err(UpdateError::Io(std::io::Error::new(
        std::io::ErrorKind::AlreadyExists,
        "could not create a private staging file next to the target",
    )))
}

#[cfg(unix)]
fn create_exclusive(path: &Path) -> std::io::Result<std::fs::File> {
    use std::os::unix::fs::OpenOptionsExt;
    std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .custom_flags(libc::O_NOFOLLOW)
        .mode(0o600)
        .open(path)
}

#[cfg(not(unix))]
fn create_exclusive(path: &Path) -> std::io::Result<std::fs::File> {
    std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(path)
}

/// Flush the directory entry itself, so the rename survives a crash.
///
/// Without this, `rename` can be visible in the page cache but not yet on
/// disk. Losing power in that window leaves the name pointing at an inode
/// whose contents were never written — and since the old inode has already
/// been unlinked from the name, there is nothing to fall back to. A
/// zero-length 0755 `fresh` is a worse outcome than a failed update.
fn sync_dir(dir: &Path) {
    if let Ok(handle) = std::fs::File::open(dir) {
        let _ = handle.sync_all();
    }
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
    use std::io::Write;

    let (staging, mut file) = staging_file(target)?;
    let result = (|| -> Result<(), UpdateError> {
        file.write_all(new_bytes)?;
        // Widen the mode only now the contents are complete, and on the
        // descriptor rather than the path — a path-based chmod is a second
        // race, and one that can be steered onto a file we do not own.
        set_executable(&file)?;
        // Durable before it is reachable: the rename must not be able to
        // publish a name whose contents have not reached disk.
        file.sync_all()?;
        drop(file);
        do_swap(target, &staging)
    })();

    if result.is_err() {
        // Best-effort cleanup of the staged file on failure.
        let _ = std::fs::remove_file(&staging);
    } else if let Some(dir) = target.parent() {
        sync_dir(dir);
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
fn set_executable(file: &std::fs::File) -> std::io::Result<()> {
    use std::os::unix::fs::PermissionsExt;
    file.set_permissions(std::fs::Permissions::from_mode(0o755))
}

#[cfg(not(unix))]
fn set_executable(_file: &std::fs::File) -> std::io::Result<()> {
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
    /// The staging file must never be written *through* something that was
    /// already there. `fs::write` opened with `O_CREAT|O_TRUNC` and followed
    /// symlinks, so on a machine where the install directory is writable by
    /// more than its owner, pre-creating the staging name as a symlink made
    /// the updater write 0755 content to a target of someone else's choosing.
    #[cfg(unix)]
    #[test]
    fn staging_refuses_a_path_that_already_exists() {
        let dir = tempfile::tempdir().unwrap();

        // A dangling symlink is the dangerous case: it does not exist as a
        // file, so a plain create would happily follow it and write to the
        // other end.
        let victim = dir.path().join("victim");
        let link = dir.path().join("staged-via-symlink");
        std::os::unix::fs::symlink(&victim, &link).unwrap();
        let err = create_exclusive(&link).expect_err("must refuse an existing symlink");
        assert_eq!(err.kind(), std::io::ErrorKind::AlreadyExists);
        assert!(!victim.exists(), "wrote through the symlink to {victim:?}");

        // And a plain existing file, so a staged binary can never be silently
        // truncated and reused.
        let existing = dir.path().join("existing");
        std::fs::write(&existing, b"x").unwrap();
        assert_eq!(
            create_exclusive(&existing).unwrap_err().kind(),
            std::io::ErrorKind::AlreadyExists
        );
        assert_eq!(std::fs::read(&existing).unwrap(), b"x");
    }

    /// Two concurrent updates must not collide on the staging name — which
    /// they would have, since the old name was derived from the pid and a
    /// process only has one.
    #[test]
    fn staging_names_do_not_collide() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("fresh");
        let (first, _f1) = staging_file(&target).unwrap();
        let (second, _f2) = staging_file(&target).unwrap();
        assert_ne!(first, second);
        assert_eq!(first.parent(), target.parent());
    }

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
