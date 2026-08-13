//! Plugin-supplied visibility filters for the file explorer.
//!
//! Decorations ([`super::decorations`]) and slot overrides ([`super::slots`])
//! already flow plugin → explorer, changing how a row is *drawn*. A filter is
//! the same channel one step earlier: it decides whether the row is drawn at
//! all. That keeps the explorer ignorant of whatever the plugin is filtering
//! *on* — git status, diagnostics, the steps of a code tour — while still
//! offering "show me only the interesting files".
//!
//! Entries are namespaced exactly like decorations, so two plugins can filter
//! without clobbering each other. The namespaces **union**: a path claimed by
//! any namespace is shown. Intersecting would usually collapse the tree to
//! nothing the moment a second plugin joined, which is a baffling result for
//! the user and an easy bug for the plugin author.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

/// The union of every namespace's paths, precomputed for lookups during
/// rendering. Rebuilt whenever a namespace is set or cleared — the same
/// discipline as [`super::decorations::FileExplorerDecorationCache`].
#[derive(Debug, Default, Clone)]
pub struct ExplorerFilter {
    /// Paths some plugin explicitly asked to show.
    matched: HashSet<PathBuf>,
    /// Ancestor directories of every matched path. Kept visible so the
    /// matched entries below them are actually reachable — a filtered file
    /// three levels down is useless if its parents are hidden.
    ancestors: HashSet<PathBuf>,
}

impl ExplorerFilter {
    /// No namespace has contributed any path, so the explorer is unfiltered.
    /// Distinct from "a filter that matches nothing": a namespace set to an
    /// empty path list contributes nothing and leaves the tree unfiltered,
    /// which is what a plugin with zero results should produce.
    pub fn is_empty(&self) -> bool {
        self.matched.is_empty()
    }

    /// Whether `path` survives the filter.
    ///
    /// Three ways to pass: the path was matched outright; it is an ancestor
    /// of something matched; or it lives *under* a matched directory —
    /// filtering to a directory shows what is in it.
    pub fn allows(&self, path: &Path) -> bool {
        if self.ancestors.contains(path) {
            return true;
        }
        let mut current = Some(path);
        while let Some(candidate) = current {
            if self.matched.contains(candidate) {
                return true;
            }
            current = candidate.parent();
        }
        false
    }

    /// Recompute the union from the per-namespace path lists.
    pub fn rebuild(namespaces: &HashMap<String, Vec<PathBuf>>) -> Self {
        let mut matched = HashSet::new();
        let mut ancestors = HashSet::new();

        for paths in namespaces.values() {
            for path in paths {
                let mut parent = path.parent();
                while let Some(dir) = parent {
                    // Stop climbing once this ancestor is already recorded:
                    // everything above it was recorded on an earlier pass.
                    if !ancestors.insert(dir.to_path_buf()) {
                        break;
                    }
                    parent = dir.parent();
                }
                matched.insert(path.clone());
            }
        }

        Self { matched, ancestors }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn filter_of(paths: &[&str]) -> ExplorerFilter {
        let mut namespaces = HashMap::new();
        namespaces.insert(
            "test".to_string(),
            paths.iter().map(PathBuf::from).collect::<Vec<_>>(),
        );
        ExplorerFilter::rebuild(&namespaces)
    }

    #[test]
    fn empty_when_no_namespace_contributes_a_path() {
        assert!(ExplorerFilter::default().is_empty());
        assert!(filter_of(&[]).is_empty());
    }

    #[test]
    fn allows_the_matched_path_and_every_ancestor() {
        let filter = filter_of(&["/repo/src/deep/main.rs"]);

        assert!(filter.allows(Path::new("/repo/src/deep/main.rs")));
        assert!(filter.allows(Path::new("/repo/src/deep")));
        assert!(filter.allows(Path::new("/repo/src")));
        assert!(filter.allows(Path::new("/repo")));
    }

    #[test]
    fn rejects_siblings_of_a_matched_path() {
        let filter = filter_of(&["/repo/src/main.rs"]);

        assert!(!filter.allows(Path::new("/repo/src/other.rs")));
        assert!(!filter.allows(Path::new("/repo/docs")));
    }

    #[test]
    fn a_matched_directory_carries_its_contents() {
        let filter = filter_of(&["/repo/src"]);

        assert!(filter.allows(Path::new("/repo/src")));
        assert!(filter.allows(Path::new("/repo/src/main.rs")));
        assert!(filter.allows(Path::new("/repo/src/nested/deep.rs")));
        assert!(!filter.allows(Path::new("/repo/docs/readme.md")));
    }

    #[test]
    fn namespaces_union_rather_than_intersect() {
        let mut namespaces = HashMap::new();
        namespaces.insert("git".to_string(), vec![PathBuf::from("/repo/a.rs")]);
        namespaces.insert("lsp".to_string(), vec![PathBuf::from("/repo/b.rs")]);
        let filter = ExplorerFilter::rebuild(&namespaces);

        assert!(filter.allows(Path::new("/repo/a.rs")));
        assert!(filter.allows(Path::new("/repo/b.rs")));
        assert!(!filter.allows(Path::new("/repo/c.rs")));
    }
}
