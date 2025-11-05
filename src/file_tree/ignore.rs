//! Ignore pattern matching for file tree filtering
//!
//! This module provides functionality to filter files and directories based on:
//! - .gitignore patterns
//! - Custom glob patterns
//! - Hidden file detection
//!
//! Uses the `ignore` crate which provides robust .gitignore parsing
//! compatible with git's ignore rules.

use ignore::gitignore::{Gitignore, GitignoreBuilder};
use std::path::{Path, PathBuf};

/// Status of a file/directory with respect to ignore patterns
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IgnoreStatus {
    /// File is visible and not ignored
    Visible,
    /// File is ignored by .gitignore
    GitIgnored,
    /// File is hidden (starts with .)
    Hidden,
    /// File is ignored by custom pattern
    CustomIgnored,
}

/// Manages ignore patterns for file filtering
#[derive(Debug)]
pub struct IgnorePatterns {
    /// Gitignore matchers per directory
    /// Key: directory path, Value: gitignore rules for that directory
    gitignores: Vec<(PathBuf, Gitignore)>,

    /// Custom glob patterns to ignore
    custom_patterns: Vec<String>,

    /// Whether to show hidden files (starting with .)
    show_hidden: bool,

    /// Whether to show gitignored files
    show_gitignored: bool,

    /// Whether to show custom ignored files
    show_custom_ignored: bool,
}

impl IgnorePatterns {
    /// Create a new ignore pattern matcher
    pub fn new() -> Self {
        Self {
            gitignores: Vec::new(),
            custom_patterns: Vec::new(),
            show_hidden: false,
            show_gitignored: false,
            show_custom_ignored: false,
        }
    }

    /// Load .gitignore file from a directory
    ///
    /// This should be called when expanding a directory to load its .gitignore
    pub fn load_gitignore(&mut self, dir: &Path) -> std::io::Result<()> {
        let gitignore_path = dir.join(".gitignore");

        if !gitignore_path.exists() {
            return Ok(()); // No .gitignore, nothing to load
        }

        let mut builder = GitignoreBuilder::new(dir);
        builder.add(&gitignore_path);

        match builder.build() {
            Ok(gitignore) => {
                // Remove any existing gitignore for this directory
                self.gitignores.retain(|(path, _)| path != dir);
                // Add new gitignore
                self.gitignores.push((dir.to_path_buf(), gitignore));
                Ok(())
            }
            Err(e) => {
                tracing::warn!("Failed to load .gitignore from {:?}: {}", gitignore_path, e);
                Ok(()) // Don't fail if .gitignore is malformed
            }
        }
    }

    /// Add a custom glob pattern to ignore
    ///
    /// Examples: "*.o", "target/", "node_modules/"
    pub fn add_custom_pattern(&mut self, pattern: String) {
        if !self.custom_patterns.contains(&pattern) {
            self.custom_patterns.push(pattern);
        }
    }

    /// Remove a custom pattern
    pub fn remove_custom_pattern(&mut self, pattern: &str) {
        self.custom_patterns.retain(|p| p != pattern);
    }

    /// Check if a path should be ignored
    ///
    /// Returns true if the file should be hidden from the tree
    pub fn is_ignored(&self, path: &Path, is_dir: bool) -> bool {
        let status = self.get_status(path, is_dir);

        match status {
            IgnoreStatus::Visible => false,
            IgnoreStatus::GitIgnored => !self.show_gitignored,
            IgnoreStatus::Hidden => !self.show_hidden,
            IgnoreStatus::CustomIgnored => !self.show_custom_ignored,
        }
    }

    /// Get the ignore status of a path
    ///
    /// This is useful for rendering (e.g., gray out ignored files)
    pub fn get_status(&self, path: &Path, is_dir: bool) -> IgnoreStatus {
        // Check if hidden (starts with .)
        if let Some(name) = path.file_name() {
            if let Some(name_str) = name.to_str() {
                if name_str.starts_with('.') && name_str != ".." && name_str != "." {
                    return IgnoreStatus::Hidden;
                }
            }
        }

        // Check custom patterns
        if self.matches_custom_pattern(path) {
            return IgnoreStatus::CustomIgnored;
        }

        // Check gitignore
        if self.matches_gitignore(path, is_dir) {
            return IgnoreStatus::GitIgnored;
        }

        IgnoreStatus::Visible
    }

    /// Check if path matches any .gitignore rules
    fn matches_gitignore(&self, path: &Path, is_dir: bool) -> bool {
        // Find the most specific .gitignore (deepest directory)
        // that could apply to this path
        for (gitignore_dir, gitignore) in &self.gitignores {
            if path.starts_with(gitignore_dir) {
                let relative_path = path.strip_prefix(gitignore_dir).unwrap_or(path);
                let matched = gitignore.matched(relative_path, is_dir);

                if matched.is_ignore() {
                    return true;
                }
            }
        }

        false
    }

    /// Check if path matches any custom patterns
    fn matches_custom_pattern(&self, path: &Path) -> bool {
        let path_str = path.to_string_lossy();

        for pattern in &self.custom_patterns {
            // Simple pattern matching (could be improved with glob crate)
            if pattern.ends_with('/') {
                // Directory pattern
                if path_str.contains(pattern.trim_end_matches('/')) {
                    return true;
                }
            } else if pattern.starts_with('*') {
                // Extension pattern like "*.o"
                let ext = pattern.trim_start_matches('*');
                if path_str.ends_with(ext) {
                    return true;
                }
            } else {
                // Exact match
                if path_str.contains(pattern) {
                    return true;
                }
            }
        }

        false
    }

    /// Set whether to show hidden files
    pub fn set_show_hidden(&mut self, show: bool) {
        self.show_hidden = show;
    }

    /// Get whether hidden files are shown
    pub fn show_hidden(&self) -> bool {
        self.show_hidden
    }

    /// Set whether to show gitignored files
    pub fn set_show_gitignored(&mut self, show: bool) {
        self.show_gitignored = show;
    }

    /// Get whether gitignored files are shown
    pub fn show_gitignored(&self) -> bool {
        self.show_gitignored
    }

    /// Set whether to show custom ignored files
    pub fn set_show_custom_ignored(&mut self, show: bool) {
        self.show_custom_ignored = show;
    }

    /// Toggle showing gitignored files
    pub fn toggle_show_gitignored(&mut self) {
        self.show_gitignored = !self.show_gitignored;
    }

    /// Toggle showing hidden files
    pub fn toggle_show_hidden(&mut self) {
        self.show_hidden = !self.show_hidden;
    }

    /// Clear all gitignore rules
    pub fn clear_gitignores(&mut self) {
        self.gitignores.clear();
    }

    /// Clear all custom patterns
    pub fn clear_custom_patterns(&mut self) {
        self.custom_patterns.clear();
    }

    /// Get number of loaded .gitignore files
    pub fn gitignore_count(&self) -> usize {
        self.gitignores.len()
    }
}

impl Default for IgnorePatterns {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::io::Write;
    use tempfile::TempDir;

    #[test]
    fn test_hidden_file_detection() {
        let patterns = IgnorePatterns::new();

        assert_eq!(
            patterns.get_status(Path::new("/foo/.hidden"), false),
            IgnoreStatus::Hidden
        );

        assert_eq!(
            patterns.get_status(Path::new("/foo/visible.txt"), false),
            IgnoreStatus::Visible
        );

        // . and .. should not be considered hidden
        assert_eq!(
            patterns.get_status(Path::new("."), true),
            IgnoreStatus::Visible
        );
        assert_eq!(
            patterns.get_status(Path::new(".."), true),
            IgnoreStatus::Visible
        );
    }

    #[test]
    fn test_custom_patterns() {
        let mut patterns = IgnorePatterns::new();

        patterns.add_custom_pattern("*.o".to_string());
        patterns.add_custom_pattern("target/".to_string());

        assert_eq!(
            patterns.get_status(Path::new("/foo/main.o"), false),
            IgnoreStatus::CustomIgnored
        );

        assert_eq!(
            patterns.get_status(Path::new("/foo/target/debug"), true),
            IgnoreStatus::CustomIgnored
        );

        assert_eq!(
            patterns.get_status(Path::new("/foo/src/main.rs"), false),
            IgnoreStatus::Visible
        );
    }

    #[test]
    fn test_gitignore_loading() -> std::io::Result<()> {
        let temp_dir = TempDir::new()?;
        let gitignore_path = temp_dir.path().join(".gitignore");

        let mut file = fs::File::create(&gitignore_path)?;
        writeln!(file, "*.log")?;
        writeln!(file, "build/")?;
        writeln!(file, "# Comment")?;
        writeln!(file, "!important.log")?;

        let mut patterns = IgnorePatterns::new();
        patterns.load_gitignore(temp_dir.path())?;

        assert_eq!(patterns.gitignore_count(), 1);

        Ok(())
    }

    #[test]
    fn test_show_hidden_toggle() {
        let mut patterns = IgnorePatterns::new();
        let hidden_path = Path::new("/foo/.hidden");

        // Initially hidden files are not shown
        assert!(!patterns.show_hidden());
        assert!(patterns.is_ignored(hidden_path, false));

        // Toggle to show hidden files
        patterns.toggle_show_hidden();
        assert!(patterns.show_hidden());
        assert!(!patterns.is_ignored(hidden_path, false));
    }

    #[test]
    fn test_show_gitignored_toggle() {
        let mut patterns = IgnorePatterns::new();

        assert!(!patterns.show_gitignored());

        patterns.toggle_show_gitignored();
        assert!(patterns.show_gitignored());

        patterns.set_show_gitignored(false);
        assert!(!patterns.show_gitignored());
    }

    #[test]
    fn test_multiple_gitignores() -> std::io::Result<()> {
        let temp_root = TempDir::new()?;
        let sub_dir = temp_root.path().join("subdir");
        fs::create_dir(&sub_dir)?;

        // Root .gitignore
        let mut root_gitignore = fs::File::create(temp_root.path().join(".gitignore"))?;
        writeln!(root_gitignore, "*.tmp")?;

        // Subdir .gitignore
        let mut sub_gitignore = fs::File::create(sub_dir.join(".gitignore"))?;
        writeln!(sub_gitignore, "*.bak")?;

        let mut patterns = IgnorePatterns::new();
        patterns.load_gitignore(temp_root.path())?;
        patterns.load_gitignore(&sub_dir)?;

        assert_eq!(patterns.gitignore_count(), 2);

        Ok(())
    }
}
