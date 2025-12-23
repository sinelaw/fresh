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
/// On first call, this extracts the embedded plugins to a temporary directory.
/// The directory persists for the lifetime of the process.
///
/// Returns `None` if extraction fails.
pub fn get_embedded_plugins_dir() -> Option<&'static PathBuf> {
    EXTRACTED_PLUGINS_DIR.get_or_init(|| {
        match extract_plugins() {
            Ok(path) => path,
            Err(e) => {
                tracing::error!("Failed to extract embedded plugins: {}", e);
                // Return a non-existent path - the caller will handle missing dirs
                PathBuf::from("/nonexistent-embedded-plugins")
            }
        }
    });

    let path = EXTRACTED_PLUGINS_DIR.get()?;
    if path.exists() {
        Some(path)
    } else {
        None
    }
}

/// Extract embedded plugins to a temporary directory
fn extract_plugins() -> Result<PathBuf, std::io::Error> {
    // Create a persistent temp directory (won't be auto-deleted)
    // tempdir()? creates the dir, keep() prevents auto-deletion and returns PathBuf
    let temp_dir = tempfile::Builder::new()
        .prefix("fresh-plugins-")
        .tempdir()?
        .keep();

    tracing::info!("Extracting embedded plugins to: {:?}", temp_dir);

    extract_dir_recursive(&EMBEDDED_PLUGINS, &temp_dir)?;

    tracing::info!(
        "Successfully extracted {} embedded plugin files",
        count_files(&EMBEDDED_PLUGINS)
    );

    Ok(temp_dir)
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
