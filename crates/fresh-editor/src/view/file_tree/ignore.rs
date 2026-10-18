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

    /// `file_explorer.custom_ignore_patterns`, matched with `.gitignore` syntax.
    custom: CustomIgnore,

    /// Whether to show hidden files (starting with .)
    show_hidden: bool,

    /// Whether to show gitignored files
    show_gitignored: bool,

    /// Whether `.gitignore` rules apply at all (`file_explorer.respect_gitignore`).
    /// When false the loaded rules are kept but never consulted, so gitignored
    /// entries are neither hidden nor marked, whatever `show_gitignored` says.
    respect_gitignore: bool,

    /// Whether to show custom ignored files
    show_custom_ignored: bool,
}

impl IgnorePatterns {
    /// Create a new ignore pattern matcher
    pub fn new() -> Self {
        Self {
            gitignores: Vec::new(),
            gitignore_mtimes: HashMap::new(),
            custom: CustomIgnore::default(),
            show_hidden: false,
            show_gitignored: false,
            respect_gitignore: true,
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

    /// Set the custom ignore patterns: `.gitignore` lines relative to `root`.
    ///
    /// Examples: "*.o", "target/", "node_modules/"
    pub fn set_custom_patterns(&mut self, root: &Path, patterns: &[String]) {
        self.custom = CustomIgnore::new(root, patterns);
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
        if !self.show_custom_ignored && self.custom.matches(path, is_dir) {
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
    #[cfg(test)]
    pub fn get_status(&self, path: &Path, is_dir: bool) -> IgnoreStatus {
        if is_hidden_name(path) {
            return IgnoreStatus::Hidden;
        }

        // Check custom patterns
        if self.custom.matches(path, is_dir) {
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
        // Single gate for the whole feature: with `respect_gitignore` off, both
        // the visibility filter and the render status see "no rules matched".
        if !self.respect_gitignore {
            return false;
        }
        // Find the most specific .gitignore (deepest directory)
        // that could apply to this path
        for (gitignore_dir, gitignore) in &self.gitignores {
            // A .gitignore at dir X governs entries *inside* X, not X itself —
            // otherwise a `*` pattern would hide the directory it lives in.
            if path.starts_with(gitignore_dir) && path != gitignore_dir.as_path() {
                let relative_path = path.strip_prefix(gitignore_dir).unwrap_or(path);
                let matched = gitignore.matched(relative_path, is_dir);

                if matched.is_ignore() {
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

    /// Set whether `.gitignore` rules apply at all.
    pub fn set_respect_gitignore(&mut self, respect: bool) {
        self.respect_gitignore = respect;
    }

    /// Get whether `.gitignore` rules apply at all.
    pub fn respect_gitignore(&self) -> bool {
        self.respect_gitignore
    }

    /// Toggle showing gitignored files
    pub fn toggle_show_gitignored(&mut self) {
        self.show_gitignored = !self.show_gitignored;
    }

    /// Toggle showing hidden files
    pub fn toggle_show_hidden(&mut self) {
        self.show_hidden = !self.show_hidden;
    }

    /// Get number of loaded .gitignore files
    #[cfg(test)]
    pub fn gitignore_count(&self) -> usize {
        self.gitignores.len()
    }
}

impl Default for IgnorePatterns {
    fn default() -> Self {
        Self::new()
    }
}

/// `file_explorer.custom_ignore_patterns`, read as `.gitignore` lines relative
/// to a root. The explorer and Quick Open both match with it, so a pattern
/// means the same thing in each.
#[derive(Debug, Default)]
pub struct CustomIgnore {
    root: PathBuf,
    matcher: Option<Gitignore>,
    /// Directory names a pattern ignores at every depth, so a walk can skip
    /// them instead of entering them and discarding everything inside. Left
    /// empty when any pattern negates, since a `!` line can re-include a name.
    prune_dirs: Vec<String>,
}

impl CustomIgnore {
    pub fn new(root: &Path, patterns: &[String]) -> Self {
        if patterns.is_empty() {
            return Self::default();
        }
        let mut builder = GitignoreBuilder::new(root);
        for pattern in patterns {
            if let Err(e) = builder.add_line(None, pattern) {
                tracing::warn!("Skipping custom ignore pattern {pattern:?}: {e}");
            }
        }
        let matcher = builder
            .build()
            .map_err(|e| tracing::warn!("Failed to build custom ignore patterns: {e}"))
            .ok();
        // As `.gitignore` reads lines: leading blanks are part of the pattern.
        let negates = patterns.iter().any(|p| p.starts_with('!'));
        let prune_dirs = if matcher.is_none() || negates {
            Vec::new()
        } else {
            patterns.iter().filter_map(|p| any_depth_name(p)).collect()
        };
        Self {
            root: root.to_path_buf(),
            matcher,
            prune_dirs,
        }
    }

    /// Whether `path` is ignored. It is either relative to the root or a
    /// rooted path under it; anything else, the root included, is not.
    pub fn matches(&self, path: &Path, is_dir: bool) -> bool {
        let Some(matcher) = &self.matcher else {
            return false;
        };
        // `has_root`, not `is_absolute`: on Windows `\foo` is rooted but not
        // absolute, and the matcher panics on any path that keeps a root.
        let rel = if path.has_root() {
            match path.strip_prefix(&self.root) {
                Ok(rel) => rel,
                Err(_) => return false,
            }
        } else {
            path
        };
        if rel.as_os_str().is_empty() {
            return false;
        }
        matcher.matched_path_or_any_parents(rel, is_dir).is_ignore()
    }

    /// Directory names to skip outright during a walk.
    pub fn prune_dirs(&self) -> &[String] {
        &self.prune_dirs
    }
}

/// The name a `.gitignore` pattern matches at any depth: `name` or `name/` with
/// no other slash and no glob. Such a pattern ignores every directory of that
/// name, which is exactly what a walk's `skip_dirs` does.
fn any_depth_name(pattern: &str) -> Option<String> {
    // `.gitignore` drops trailing blanks only; a leading one is part of the
    // name, which `contains(' ')` below then keeps out of `skip_dirs`.
    let pattern = pattern.trim_end();
    let name = pattern.strip_suffix('/').unwrap_or(pattern);
    let plain = !name.is_empty()
        && !name.starts_with('#')
        && !name.contains(['/', '\\', '*', '?', '[', ' ']);
    plain.then(|| name.to_string())
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
        patterns.set_custom_patterns(
            Path::new("/foo"),
            &["*.o".to_string(), "target/".to_string()],
        );

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

    /// Custom patterns follow `.gitignore` rules, not substring matching: a
    /// plain name matches that name only, a leading `/` anchors to the root,
    /// and `!` re-includes.
    #[test]
    fn test_custom_patterns_use_gitignore_rules() {
        let root = Path::new("/foo");
        let mut patterns = IgnorePatterns::new();
        patterns.set_custom_patterns(
            root,
            &[
                "build".to_string(),
                "/dist".to_string(),
                "*.log".to_string(),
                "!keep.log".to_string(),
            ],
        );
        let ignored = |p: &str, is_dir| patterns.is_ignored(&root.join(p), is_dir);

        assert!(ignored("build", true));
        assert!(ignored("src/build/out.o", false));
        assert!(!ignored("rebuild.txt", false), "substring of a pattern");
        assert!(!ignored("src/builder.rs", false), "substring of a pattern");
        assert!(ignored("dist", true));
        assert!(!ignored("src/dist", true), "/dist is anchored to the root");
        assert!(ignored("debug.log", false));
        assert!(!ignored("keep.log", false));
        assert!(!patterns.is_ignored(Path::new("/elsewhere/build"), true));
        assert!(!patterns.is_ignored(root, true));
    }

    /// Only a pattern that names a directory at any depth is pruned from a
    /// walk, and none is once a pattern negates.
    #[test]
    fn test_custom_ignore_prunes_only_any_depth_names() {
        let root = Path::new("/project");
        let patterns = |ps: &[&str]| ps.iter().map(|p| p.to_string()).collect::<Vec<_>>();

        let ignore = CustomIgnore::new(
            root,
            &patterns(&[".cache/", "build", "*.o", "/dist", "a/b/", "# note", ""]),
        );
        assert_eq!(ignore.prune_dirs(), [".cache", "build"]);

        let ignore = CustomIgnore::new(root, &patterns(&[".cache/", "!.cache/keep"]));
        assert!(ignore.prune_dirs().is_empty());
        assert!(ignore.matches(Path::new(".cache/other"), false));

        // Blanks as `.gitignore` reads them: trailing ones are dropped, a
        // leading one is part of the name, so ` build` is not `build`.
        let ignore = CustomIgnore::new(root, &patterns(&["out  ", " build"]));
        assert_eq!(ignore.prune_dirs(), ["out"]);
        assert!(!ignore.matches(Path::new("build/x"), false));
        let ignore = CustomIgnore::new(root, &patterns(&["build", " !build/keep"]));
        assert_eq!(ignore.prune_dirs(), ["build"], "` !x` is not a negation");
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
