//! Path utilities for path expansion and normalization.

use std::path::PathBuf;

/// Expand tilde (~) in a path to the user's home directory.
///
/// # Examples
/// - `~/Documents` -> `/home/user/Documents`
/// - `~` -> `/home/user`
/// - `/absolute/path` -> `/absolute/path` (unchanged)
/// - `relative/path` -> `relative/path` (unchanged)
///
/// If the home directory cannot be determined, the path is returned unchanged.
pub fn expand_tilde(path: &str) -> PathBuf {
    if let Some(suffix) = path.strip_prefix('~') {
        if let Some(home) = dirs::home_dir() {
            home.join(suffix.trim_start_matches('/'))
        } else {
            PathBuf::from(path)
        }
    } else {
        PathBuf::from(path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expand_tilde_with_path() {
        let result = expand_tilde("~/Documents/test.txt");
        if let Some(home) = dirs::home_dir() {
            assert_eq!(result, home.join("Documents/test.txt"));
        }
    }

    #[test]
    fn test_expand_tilde_just_tilde() {
        let result = expand_tilde("~");
        if let Some(home) = dirs::home_dir() {
            assert_eq!(result, home);
        }
    }

    #[test]
    fn test_expand_tilde_absolute_path() {
        let result = expand_tilde("/absolute/path");
        assert_eq!(result, PathBuf::from("/absolute/path"));
    }

    #[test]
    fn test_expand_tilde_relative_path() {
        let result = expand_tilde("relative/path");
        assert_eq!(result, PathBuf::from("relative/path"));
    }

    #[test]
    fn test_expand_tilde_with_slash() {
        let result = expand_tilde("~/");
        if let Some(home) = dirs::home_dir() {
            assert_eq!(result, home);
        }
    }
}
