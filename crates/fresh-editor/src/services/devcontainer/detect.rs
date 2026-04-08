//! Devcontainer configuration detection
//!
//! Detects `.devcontainer/devcontainer.json` or `.devcontainer.json`
//! when a directory is opened.

use std::path::{Path, PathBuf};

use crate::model::filesystem::FileSystem;

/// Result of devcontainer detection
#[derive(Debug, Clone)]
pub struct DetectedConfig {
    /// Path to the devcontainer.json file
    pub config_path: PathBuf,
    /// The workspace directory containing the devcontainer config
    pub workspace_path: PathBuf,
}

/// Detect devcontainer configuration in the given directory.
///
/// Checks the following locations (in order):
/// 1. `<dir>/.devcontainer/devcontainer.json`
/// 2. `<dir>/.devcontainer.json`
///
/// Uses the FileSystem trait so this works for both local and remote paths.
pub fn detect_devcontainer(dir: &Path, fs: &dyn FileSystem) -> Option<DetectedConfig> {
    let candidates = [
        dir.join(".devcontainer").join("devcontainer.json"),
        dir.join(".devcontainer.json"),
    ];

    for candidate in &candidates {
        if fs.is_file(candidate).unwrap_or(false) {
            return Some(DetectedConfig {
                config_path: candidate.clone(),
                workspace_path: dir.to_path_buf(),
            });
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::filesystem::StdFileSystem;
    use std::fs;

    #[test]
    fn test_detect_devcontainer_in_subdirectory() {
        let tmp = tempfile::tempdir().unwrap();
        let devcontainer_dir = tmp.path().join(".devcontainer");
        fs::create_dir_all(&devcontainer_dir).unwrap();
        fs::write(devcontainer_dir.join("devcontainer.json"), "{}").unwrap();

        let result = detect_devcontainer(tmp.path(), &StdFileSystem);
        assert!(result.is_some());
        let detected = result.unwrap();
        assert_eq!(
            detected.config_path,
            devcontainer_dir.join("devcontainer.json")
        );
        assert_eq!(detected.workspace_path, tmp.path());
    }

    #[test]
    fn test_detect_devcontainer_root_json() {
        let tmp = tempfile::tempdir().unwrap();
        fs::write(tmp.path().join(".devcontainer.json"), "{}").unwrap();

        let result = detect_devcontainer(tmp.path(), &StdFileSystem);
        assert!(result.is_some());
        let detected = result.unwrap();
        assert_eq!(
            detected.config_path,
            tmp.path().join(".devcontainer.json")
        );
    }

    #[test]
    fn test_detect_devcontainer_not_found() {
        let tmp = tempfile::tempdir().unwrap();
        let result = detect_devcontainer(tmp.path(), &StdFileSystem);
        assert!(result.is_none());
    }

    #[test]
    fn test_detect_devcontainer_prefers_subdirectory() {
        let tmp = tempfile::tempdir().unwrap();
        // Create both variants
        let devcontainer_dir = tmp.path().join(".devcontainer");
        fs::create_dir_all(&devcontainer_dir).unwrap();
        fs::write(devcontainer_dir.join("devcontainer.json"), "{}").unwrap();
        fs::write(tmp.path().join(".devcontainer.json"), "{}").unwrap();

        let result = detect_devcontainer(tmp.path(), &StdFileSystem);
        assert!(result.is_some());
        // Should prefer .devcontainer/devcontainer.json
        let detected = result.unwrap();
        assert!(detected.config_path.ends_with(".devcontainer/devcontainer.json"));
    }
}
