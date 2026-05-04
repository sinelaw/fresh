//! Embedded plugins support
//!
//! When the `embed-plugins` feature is enabled, this module provides access to plugins
//! that are compiled directly into the binary. This is useful for cargo-binstall
//! distributions where the plugins directory would otherwise be missing.
//!
//! The plugins are extracted to a temporary directory at runtime and loaded from there.

use include_dir::{include_dir, Dir};
use std::path::PathBuf;
use std::sync::OnceLock;

/// The plugins directory embedded at compile time
static EMBEDDED_PLUGINS: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/plugins");

/// Cached path to the extracted plugins directory
static EXTRACTED_PLUGINS_DIR: OnceLock<PathBuf> = OnceLock::new();

/// Get the path to the embedded plugins directory.
///
/// On first call, this extracts the embedded plugins to a cache directory.
/// The cache is content-addressed, so unchanged plugins are reused across runs.
///
/// Returns `None` if extraction fails.
pub fn get_embedded_plugins_dir() -> Option<&'static PathBuf> {
    EXTRACTED_PLUGINS_DIR.get_or_init(|| match extract_plugins() {
        Ok(path) => path,
        Err(e) => {
            tracing::error!("Failed to extract embedded plugins: {}", e);
            PathBuf::new()
        }
    });

    let path = EXTRACTED_PLUGINS_DIR.get()?;
    if path.exists()
        && path
            .read_dir()
            .map(|mut d| d.next().is_some())
            .unwrap_or(false)
    {
        Some(path)
    } else {
        None
    }
}

/// Content hash of embedded plugins, computed at build time
const PLUGINS_CONTENT_HASH: &str = include_str!(concat!(env!("OUT_DIR"), "/plugins_hash.txt"));

/// Get the cache directory for extracted plugins
fn get_cache_dir() -> Option<PathBuf> {
    dirs::cache_dir().map(|p| p.join("fresh").join("embedded-plugins"))
}

/// Extract embedded plugins to the cache directory.
///
/// Concurrency contract: this function is called via a process-local
/// `OnceLock`, but multiple test processes (e.g. cargo-nextest) may
/// each call it concurrently against the same on-disk directory. We
/// publish atomically: extract into a sibling `.pending.<pid>.<nanos>`
/// directory, write a `.extracted` marker, then `rename` into place.
/// `rename` over an existing non-empty directory fails on POSIX, so
/// only one publisher wins; losers fall back to the winner's
/// directory. Readers gate on the marker file so they never observe a
/// half-extracted tree.
fn extract_plugins() -> Result<PathBuf, std::io::Error> {
    let cache_base = get_cache_dir().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "Could not determine cache directory",
        )
    })?;

    let content_hash = PLUGINS_CONTENT_HASH.trim();
    let cache_dir = cache_base.join(content_hash);
    let marker = cache_dir.join(".extracted");

    if marker.exists() {
        tracing::info!("Using cached embedded plugins from: {:?}", cache_dir);
        return Ok(cache_dir);
    }

    tracing::info!("Extracting embedded plugins to: {:?}", cache_dir);
    std::fs::create_dir_all(&cache_base)?;

    let pid = std::process::id();
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    // tmp_dir name includes the current nanosecond timestamp, so it
    // can't collide with any prior invocation — no pre-existing dir
    // to clean up here.
    let tmp_dir = cache_base.join(format!(".pending.{}.{}", pid, nanos));

    extract_dir_recursive(&EMBEDDED_PLUGINS, &tmp_dir)?;
    // Marker is written *inside* the temp dir so the rename publishes
    // the directory and its completeness signal in one atomic step.
    std::fs::write(tmp_dir.join(".extracted"), b"")?;

    let publish = |tmp_dir: &std::path::Path| std::fs::rename(tmp_dir, &cache_dir);

    let result = match publish(&tmp_dir) {
        Ok(()) => Ok(cache_dir.clone()),
        Err(_) if marker.exists() => {
            // A concurrent publisher won the race; drop our partial work.
            // tmp_dir has a unique name, so a leak here is unrecoverable
            // disk noise but never reachable by future readers.
            #[allow(clippy::let_underscore_must_use)]
            let _ = std::fs::remove_dir_all(&tmp_dir);
            Ok(cache_dir.clone())
        }
        Err(_) => {
            // `cache_dir` exists but has no marker — a leftover from
            // the pre-marker code path that could have written a
            // half-extracted tree. Sweep it aside and retry once.
            // Either rename succeeds (cache_dir gone, retry will
            // publish), fails because cache_dir vanished (another
            // process beat us — fine, retry will see the publish), or
            // fails for an OS-level reason (we fall through to the
            // marker check). All paths converge.
            let stale = cache_base.join(format!(".stale.{}.{}", pid, nanos));
            #[allow(clippy::let_underscore_must_use)]
            let _ = std::fs::rename(&cache_dir, &stale);
            // Best-effort: if the rename failed, `stale` may not exist
            // (NotFound) — that's the no-op case. If it succeeded but
            // remove fails, we leak disk space at a unique-named path
            // nobody else references.
            #[allow(clippy::let_underscore_must_use)]
            let _ = std::fs::remove_dir_all(&stale);
            match publish(&tmp_dir) {
                Ok(()) => Ok(cache_dir.clone()),
                Err(e) => {
                    // Same as the marker-exists arm above: tmp_dir has
                    // a unique name, leakage on cleanup failure is
                    // bounded and unobservable to readers.
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = std::fs::remove_dir_all(&tmp_dir);
                    if marker.exists() {
                        Ok(cache_dir.clone())
                    } else {
                        Err(e)
                    }
                }
            }
        }
    };

    if result.is_ok() {
        tracing::info!(
            "Successfully extracted {} embedded plugin files",
            count_files(&EMBEDDED_PLUGINS)
        );
        // Clean up old cache versions (other content hashes) and stale
        // pending/stale-rename leftovers from prior crashes. Only does
        // anything once we've successfully committed our extraction.
        if let Ok(entries) = std::fs::read_dir(&cache_base) {
            for entry in entries.flatten() {
                let name_os = entry.file_name();
                let name = name_os.to_string_lossy();
                if name == content_hash {
                    continue;
                }
                if name.starts_with(".pending.") || name.starts_with(".stale.") {
                    // Never trash a tmp dir that another live extractor
                    // might still own. We can't tell from here, so leave
                    // those alone — they're cheap and self-clean on the
                    // next successful publish from that process.
                    continue;
                }
                // Best-effort cleanup of old cache versions.
                #[allow(clippy::let_underscore_must_use)]
                let _ = trash::delete(entry.path());
            }
        }
    }

    result
}

/// Recursively extract a directory and its contents
fn extract_dir_recursive(dir: &Dir<'_>, target_path: &std::path::Path) -> std::io::Result<()> {
    std::fs::create_dir_all(target_path)?;

    // Extract files
    for file in dir.files() {
        let file_path = target_path.join(file.path().file_name().unwrap_or_default());
        std::fs::write(&file_path, file.contents())?;
        tracing::debug!("Extracted: {:?}", file_path);
    }

    // Recursively extract subdirectories
    for subdir in dir.dirs() {
        let subdir_name = subdir.path().file_name().unwrap_or_default();
        let subdir_path = target_path.join(subdir_name);
        extract_dir_recursive(subdir, &subdir_path)?;
    }

    Ok(())
}

/// Count total files in embedded directory (for logging)
fn count_files(dir: &Dir<'_>) -> usize {
    let mut count = dir.files().count();
    for subdir in dir.dirs() {
        count += count_files(subdir);
    }
    count
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_embedded_plugins_exist() {
        // Verify that plugins are embedded
        assert!(EMBEDDED_PLUGINS.files().count() > 0 || EMBEDDED_PLUGINS.dirs().count() > 0);
    }

    #[test]
    fn test_extract_plugins() {
        let path = get_embedded_plugins_dir();
        assert!(path.is_some());
        let path = path.unwrap();
        assert!(path.exists());
        assert!(path.is_dir());

        // Check that some plugin files exist
        let entries: Vec<_> = std::fs::read_dir(path).unwrap().collect();
        assert!(!entries.is_empty());
    }
}
