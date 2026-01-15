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

/// Extract embedded plugins to the cache directory
fn extract_plugins() -> Result<PathBuf, std::io::Error> {
    let cache_base = get_cache_dir().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "Could not determine cache directory",
        )
    })?;

    let content_hash = PLUGINS_CONTENT_HASH.trim();
    let cache_dir = cache_base.join(&content_hash);

    // Check if already extracted
    if cache_dir.exists() && cache_dir.read_dir()?.next().is_some() {
        tracing::info!("Using cached embedded plugins from: {:?}", cache_dir);
        return Ok(cache_dir);
    }

    tracing::info!("Extracting embedded plugins to: {:?}", cache_dir);

    // Clean up old cache versions (move to trash for safety)
    if cache_base.exists() {
        for entry in std::fs::read_dir(&cache_base)? {
            let entry = entry?;
            if entry.file_name() != content_hash {
                let _ = trash::delete(entry.path());
            }
        }
    }

    extract_dir_recursive(&EMBEDDED_PLUGINS, &cache_dir)?;

    tracing::info!(
        "Successfully extracted {} embedded plugin files",
        count_files(&EMBEDDED_PLUGINS)
    );

    Ok(cache_dir)
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
