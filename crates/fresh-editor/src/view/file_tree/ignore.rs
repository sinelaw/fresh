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
                let mtime = std::fs::metadata(&gitignore_path)
                    .ok()
                    .and_then(|m| m.modified().ok());
                // Remove any existing gitignore for this directory
                self.gitignores.retain(|(path, _)| path != dir);
                // Add new gitignore
                self.gitignores.push((dir.to_path_buf(), gitignore));
                if let Some(mtime) = mtime {
                    self.gitignore_mtimes.insert(dir.to_path_buf(), mtime);
                }
                Ok(())
            }
            Err(e) => {
                tracing::warn!("Failed to load .gitignore from {:?}: {}", gitignore_path, e);
                Ok(()) // Don't fail if .gitignore is malformed
            }
        }
    }

    /// Re-check every currently-loaded `.gitignore` on disk, reloading those
    /// whose mtime has changed and dropping those that have been deleted.
    ///
    /// Returns `true` if anything changed (so the caller knows to re-render).
    pub fn sync_gitignores_from_disk(&mut self) -> bool {
        let dirs: Vec<PathBuf> = self.gitignores.iter().map(|(d, _)| d.clone()).collect();
        let mut changed = false;

        for dir in dirs {
            let gitignore_path = dir.join(".gitignore");
            let current_mtime = std::fs::metadata(&gitignore_path)
                .ok()
                .and_then(|m| m.modified().ok());

            match current_mtime {
                None => {
                    // File vanished — drop the entry.
                    self.gitignores.retain(|(d, _)| d != &dir);
                    self.gitignore_mtimes.remove(&dir);
                    changed = true;
                }
                Some(mtime) => {
                    let stored = self.gitignore_mtimes.get(&dir).copied();
                    if stored != Some(mtime) {
                        // Content changed — reload. load_gitignore overwrites
                        // the entry and stores the new mtime.
                        if self.load_gitignore(&dir).is_ok() {
                            changed = true;
                        }
                    }
                }
            }
        }

        changed
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
    fn test_hidden_gitignored_respects_gitignore_filter() -> std::io::Result<()> {
        // Regression test for #1388: a file that is both hidden (starts with '.')
        // and matched by .gitignore must stay hidden when `show_gitignored` is
        // false, even if `show_hidden` is true. Hidden ≠ gitignored, and the
        // user's choice to hide gitignored files should take precedence.
        let temp_dir = TempDir::new()?;
        let mut gitignore = fs::File::create(temp_dir.path().join(".gitignore"))?;
        writeln!(gitignore, ".DS_Store")?;
        drop(gitignore);

        let mut patterns = IgnorePatterns::new();
        patterns.load_gitignore(temp_dir.path())?;
        patterns.set_show_hidden(true);
        patterns.set_show_gitignored(false);

        let ds_store = temp_dir.path().join(".DS_Store");
        assert!(
            patterns.is_ignored(&ds_store, false),
            ".DS_Store is gitignored; should be hidden despite show_hidden=true"
        );

        // A hidden file NOT in .gitignore should still be shown.
        let gitignore_file = temp_dir.path().join(".gitignore");
        assert!(
            !patterns.is_ignored(&gitignore_file, false),
            ".gitignore is hidden but not gitignored; should be visible \
             when show_hidden=true"
        );

        // Conversely, when show_gitignored is true, the file reappears even
        // if show_hidden is false (gitignored filter is independent).
        patterns.set_show_hidden(false);
        patterns.set_show_gitignored(true);
        assert!(
            patterns.is_ignored(&ds_store, false),
            ".DS_Store is still hidden, should respect show_hidden=false"
        );

        // Both filters disabled → fully visible.
        patterns.set_show_hidden(true);
        patterns.set_show_gitignored(true);
        assert!(!patterns.is_ignored(&ds_store, false));

        Ok(())
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
