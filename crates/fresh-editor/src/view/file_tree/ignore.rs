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
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

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

    /// Mtime of each loaded .gitignore at load time. Used to detect
    /// external edits and deletions during the file-tree poll.
    gitignore_mtimes: HashMap<PathBuf, SystemTime>,

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
            gitignore_mtimes: HashMap::new(),
            custom_patterns: Vec::new(),
            show_hidden: false,
            show_gitignored: false,
            show_custom_ignored: false,
        }
    }

    /// Install a gitignore for `dir` from already-read bytes.
    ///
    /// I/O lives in the caller (the editor's filesystem authority), keeping
    /// this module pure so it works uniformly over local and remote trees.
    /// Pass `mtime` so the poll can later detect external changes.
    pub fn load_gitignore_from_bytes(
        &mut self,
        dir: &Path,
        contents: &[u8],
        mtime: Option<SystemTime>,
    ) {
        let mut builder = GitignoreBuilder::new(dir);
        let source = dir.join(".gitignore");
        for line in contents.split(|&b| b == b'\n') {
            let line = std::str::from_utf8(line).unwrap_or("");
            if let Err(e) = builder.add_line(Some(source.clone()), line) {
                tracing::warn!("Malformed .gitignore line in {:?}: {}", source, e);
            }
        }

        match builder.build() {
            Ok(gitignore) => {
                self.gitignores.retain(|(path, _)| path != dir);
                self.gitignores.push((dir.to_path_buf(), gitignore));
                if let Some(mtime) = mtime {
                    self.gitignore_mtimes.insert(dir.to_path_buf(), mtime);
                } else {
                    self.gitignore_mtimes.remove(dir);
                }
            }
            Err(e) => {
                tracing::warn!("Failed to build .gitignore for {:?}: {}", dir, e);
            }
        }
    }

    /// Drop any loaded gitignore for `dir`.
    pub fn remove_gitignore(&mut self, dir: &Path) {
        self.gitignores.retain(|(d, _)| d != dir);
        self.gitignore_mtimes.remove(dir);
    }

    /// Dirs for which a gitignore is currently loaded.
    pub fn loaded_gitignore_dirs(&self) -> Vec<PathBuf> {
        self.gitignores.iter().map(|(d, _)| d.clone()).collect()
    }

    /// Mtime recorded when the gitignore for `dir` was last loaded.
    pub fn stored_gitignore_mtime(&self, dir: &Path) -> Option<SystemTime> {
        self.gitignore_mtimes.get(dir).copied()
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
    /// Each filter (hidden / custom / gitignored) is evaluated independently:
    /// a file is hidden from the tree if *any* enabled filter matches it. This
    /// way a file that is both hidden and gitignored still disappears when
    /// gitignored files are hidden, even if hidden files are shown.
    pub fn is_ignored(&self, path: &Path, is_dir: bool) -> bool {
        if !self.show_hidden && is_hidden_name(path) {
            return true;
        }
        if !self.show_custom_ignored && self.matches_custom_pattern(path) {
            return true;
        }
        if !self.show_gitignored && self.matches_gitignore(path, is_dir) {
            return true;
        }
        false
    }

    /// Get the ignore status of a path
    ///
    /// This is useful for rendering (e.g., gray out ignored files)
    pub fn get_status(&self, path: &Path, is_dir: bool) -> IgnoreStatus {
        if is_hidden_name(path) {
            return IgnoreStatus::Hidden;
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
        self.gitignore_mtimes.clear();
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

fn is_hidden_name(path: &Path) -> bool {
    path.file_name()
        .and_then(|n| n.to_str())
        .map(|n| n.starts_with('.') && n != "." && n != "..")
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn test_gitignore_loading() {
        let mut patterns = IgnorePatterns::new();
        patterns.load_gitignore_from_bytes(
            Path::new("/foo"),
            b"*.log\nbuild/\n# Comment\n!important.log\n",
            None,
        );
        assert_eq!(patterns.gitignore_count(), 1);
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
    fn test_hidden_gitignored_respects_gitignore_filter() {
        // Regression test for #1388: a file that is both hidden (starts with '.')
        // and matched by .gitignore must stay hidden when `show_gitignored` is
        // false, even if `show_hidden` is true. Hidden ≠ gitignored, and the
        // user's choice to hide gitignored files should take precedence.
        let root = Path::new("/repo");
        let mut patterns = IgnorePatterns::new();
        patterns.load_gitignore_from_bytes(root, b".DS_Store\n", None);
        patterns.set_show_hidden(true);
        patterns.set_show_gitignored(false);

        let ds_store = root.join(".DS_Store");
        assert!(
            patterns.is_ignored(&ds_store, false),
            ".DS_Store is gitignored; should be hidden despite show_hidden=true"
        );

        // A hidden file NOT in .gitignore should still be shown.
        let gitignore_file = root.join(".gitignore");
        assert!(
            !patterns.is_ignored(&gitignore_file, false),
            ".gitignore is hidden but not gitignored; should be visible \
             when show_hidden=true"
        );

        // With show_hidden=false, the hidden filter hides .DS_Store on its own
        // regardless of the gitignore filter state.
        patterns.set_show_hidden(false);
        patterns.set_show_gitignored(true);
        assert!(
            patterns.is_ignored(&ds_store, false),
            "show_hidden=false still hides .DS_Store (hidden filter)"
        );

        // Both filters disabled → fully visible.
        patterns.set_show_hidden(true);
        patterns.set_show_gitignored(true);
        assert!(!patterns.is_ignored(&ds_store, false));
    }

    #[test]
    fn test_multiple_gitignores() {
        let root = Path::new("/repo");
        let sub = root.join("subdir");

        let mut patterns = IgnorePatterns::new();
        patterns.load_gitignore_from_bytes(root, b"*.tmp\n", None);
        patterns.load_gitignore_from_bytes(&sub, b"*.bak\n", None);

        assert_eq!(patterns.gitignore_count(), 2);
    }
}
