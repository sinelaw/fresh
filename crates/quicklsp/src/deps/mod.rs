//! Dependency Source Indexing
//!
//! Lazy indexing of locally-installed dependency sources for hover, signature
//! help, and completion fallback. Per-ecosystem submodules handle manifest
//! parsing and path resolution.
//!
//! ## Design
//!
//! - Parses lock files (Cargo.lock, package-lock.json, go.sum) for the full
//!   transitive dependency closure — only declared deps are indexed.
//! - Dependencies may not be installed when the LSP starts. The index
//!   periodically re-probes known dependency directories so that packages
//!   installed later (via `cargo build`, `npm install`, etc.) are picked up.
//! - Indexing is incremental: the Workspace uses DashMap, so queries work
//!   immediately while background indexing is still in progress.
//! - Per-ecosystem modules handle the specifics of each packaging system.

pub mod cargo;
pub mod files;
pub mod go;
pub mod npm;
pub mod python;

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::RwLock;
use std::time::{Duration, Instant};

use dashmap::DashMap;

use crate::workspace::{SymbolLocation, Workspace};

/// How often to re-check whether dependency sources have appeared on disk.
const DEFAULT_RECHECK_INTERVAL: Duration = Duration::from_secs(30);

/// A dependency source ecosystem.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ecosystem {
    Cargo,
    Npm,
    Python,
    Go,
}

/// A resolved package directory ready for indexing.
#[derive(Debug)]
pub struct PackageDir {
    pub path: PathBuf,
    pub ecosystem: Ecosystem,
}

/// Dependency source index.
///
/// Holds a separate `Workspace` for dependency symbols, queried as a fallback
/// when the main workspace doesn't have hover/signature info for a symbol.
/// The DashMap-based Workspace supports concurrent reads during indexing.
pub struct DependencyIndex {
    /// Workspace holding indexed dependency files (concurrent via DashMap).
    deps: Workspace,

    /// Packages that have already been indexed. Key = package directory path.
    indexed_packages: DashMap<PathBuf, ()>,

    /// Resolved package directories waiting to be indexed.
    pending_packages: RwLock<Vec<PackageDir>>,

    /// Whether a background index pass is currently running.
    indexing_in_progress: AtomicBool,

    /// When we last resolved dependencies from manifests.
    last_resolve: RwLock<Option<Instant>>,

    /// Interval for re-checking dep availability.
    recheck_interval: Duration,

    /// Workspace root for re-resolution.
    workspace_root: RwLock<Option<PathBuf>>,
}

impl DependencyIndex {
    pub fn new() -> Self {
        Self {
            deps: Workspace::new(),
            indexed_packages: DashMap::new(),
            pending_packages: RwLock::new(Vec::new()),
            indexing_in_progress: AtomicBool::new(false),
            last_resolve: RwLock::new(None),
            recheck_interval: DEFAULT_RECHECK_INTERVAL,
            workspace_root: RwLock::new(None),
        }
    }

    /// Detect project ecosystems and resolve dependency package directories.
    ///
    /// Parses lock files for the full transitive closure, then checks which
    /// packages are actually installed on disk. Uninstalled packages are
    /// remembered so they can be picked up on re-check.
    pub fn detect_and_resolve(&self, workspace_root: &Path) {
        *self.workspace_root.write().unwrap() = Some(workspace_root.to_path_buf());

        let mut packages = Vec::new();

        // Rust / Cargo
        if workspace_root.join("Cargo.toml").exists() {
            let deps = cargo::parse_lock_file(workspace_root);
            tracing::info!("Cargo: {} dependencies in lock file", deps.len());
            packages.extend(cargo::resolve_package_dirs(workspace_root, &deps));
        }

        // Node / npm
        if workspace_root.join("package.json").exists() {
            let deps = npm::parse_package_json(workspace_root);
            tracing::info!("npm: {} declared dependencies", deps.len());
            packages.extend(npm::resolve_package_dirs(workspace_root, &deps));
        }

        // Python
        let has_python = workspace_root.join("requirements.txt").exists()
            || workspace_root.join("pyproject.toml").exists()
            || workspace_root.join("setup.py").exists()
            || workspace_root.join("Pipfile").exists();
        if has_python {
            let deps = python::parse_requirements(workspace_root);
            tracing::info!("Python: {} declared dependencies", deps.len());
            packages.extend(python::resolve_package_dirs(workspace_root, &deps));
        }

        // Go
        if workspace_root.join("go.mod").exists() {
            let deps = go::parse_go_sum(workspace_root);
            tracing::info!("Go: {} dependencies in go.sum", deps.len());
            packages.extend(go::resolve_package_dirs(workspace_root, &deps));
        }

        // Filter out already-indexed packages
        packages.retain(|p| !self.indexed_packages.contains_key(&p.path));

        tracing::info!(
            "Resolved {} new dependency packages to index",
            packages.len()
        );

        *self.pending_packages.write().unwrap() = packages;
        *self.last_resolve.write().unwrap() = Some(Instant::now());
    }

    /// Re-resolve dependencies if enough time has passed since the last check.
    ///
    /// This catches packages that were installed after the LSP started
    /// (e.g., user ran `cargo build` or `npm install`).
    pub fn refresh_if_stale(&self) {
        let should_refresh = {
            let last = self.last_resolve.read().unwrap();
            match *last {
                Some(t) => Instant::now().duration_since(t) > self.recheck_interval,
                None => true,
            }
        };

        if should_refresh {
            if let Some(root) = self.workspace_root.read().unwrap().clone() {
                self.detect_and_resolve(&root);
            }
        }
    }

    /// Index pending packages. Can be called from a background thread.
    ///
    /// Since the Workspace uses DashMap, queries against already-indexed
    /// symbols work concurrently while this method is still indexing.
    pub fn index_pending(&self) {
        // Prevent concurrent index passes
        if self
            .indexing_in_progress
            .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
            .is_err()
        {
            return; // Another thread is already indexing
        }

        let packages = {
            let mut pending = self.pending_packages.write().unwrap();
            std::mem::take(&mut *pending)
        };

        for pkg in packages {
            if self.indexed_packages.contains_key(&pkg.path) {
                continue;
            }

            let files = match pkg.ecosystem {
                Ecosystem::Cargo => cargo::collect_package_files(&pkg.path),
                Ecosystem::Npm => npm::collect_package_files(&pkg.path),
                Ecosystem::Python => python::collect_package_files(&pkg.path),
                Ecosystem::Go => go::collect_package_files(&pkg.path),
            };

            for file_path in &files {
                if let Ok(source) = std::fs::read_to_string(file_path) {
                    self.deps.index_file(file_path.clone(), source);
                }
            }

            if !files.is_empty() {
                tracing::debug!(
                    "Indexed {} files from {:?} ({:?})",
                    files.len(),
                    pkg.path.file_name().unwrap_or_default(),
                    pkg.ecosystem,
                );
            }

            self.indexed_packages.insert(pkg.path, ());
        }

        self.indexing_in_progress.store(false, Ordering::SeqCst);
    }

    /// Try to find hover info for a symbol, triggering index if needed.
    pub fn hover_info(&self, name: &str) -> Option<(Option<String>, Option<String>)> {
        // Check already-indexed deps (works during concurrent indexing)
        if let Some(info) = self.deps.hover_info(name) {
            return Some(info);
        }

        // If there are pending packages, index them
        self.refresh_if_stale();
        self.index_pending();

        // Try again after indexing
        self.deps.hover_info(name)
    }

    /// Find definitions in dependency sources.
    pub fn find_definitions(&self, name: &str) -> Vec<SymbolLocation> {
        self.deps.find_definitions(name)
    }

    /// Get completions from dependency sources.
    pub fn completions(&self, prefix: &str) -> Vec<SymbolLocation> {
        self.deps.completions(prefix)
    }

    /// Signature help from dependency sources.
    pub fn signature_help_at(
        &self,
        source: &str,
        line: usize,
        col: usize,
    ) -> Option<(SymbolLocation, usize)> {
        self.deps.signature_help_at(source, line, col)
    }

    /// Number of indexed dependency packages.
    pub fn package_count(&self) -> usize {
        self.indexed_packages.len()
    }

    /// Total number of definitions in the dependency index.
    pub fn definition_count(&self) -> usize {
        self.deps.definition_count()
    }

    /// Total number of indexed files in the dependency index.
    pub fn file_count(&self) -> usize {
        self.deps.file_count()
    }

    /// Number of packages waiting to be indexed.
    pub fn pending_count(&self) -> usize {
        self.pending_packages.read().unwrap().len()
    }

    /// Whether indexing is currently in progress.
    pub fn is_indexing(&self) -> bool {
        self.indexing_in_progress.load(Ordering::SeqCst)
    }

    /// Add package directories to the pending index queue.
    pub fn enqueue_packages(&self, packages: Vec<PackageDir>) {
        let mut pending = self.pending_packages.write().unwrap();
        pending.extend(packages);
    }
}

impl Default for DependencyIndex {
    fn default() -> Self {
        Self::new()
    }
}

// ── Shared helpers ─────────────────────────────────────────────────────

/// Get the user's home directory.
pub(crate) fn home_dir() -> Option<PathBuf> {
    std::env::var("HOME")
        .ok()
        .map(PathBuf::from)
        .or_else(|| std::env::var("USERPROFILE").ok().map(PathBuf::from))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dependency_index_lifecycle() {
        let idx = DependencyIndex::new();
        assert_eq!(idx.package_count(), 0);
        assert!(idx.hover_info("nonexistent").is_none());
    }

    #[test]
    fn index_pending_works_incrementally() {
        let dir = std::env::temp_dir().join("quicklsp_test_incremental");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("lib.rs"),
            "/// A dep helper.\nfn dep_helper(x: i32) -> bool { x > 0 }",
        )
        .unwrap();

        let idx = DependencyIndex::new();

        // Manually add a pending package
        {
            let mut pending = idx.pending_packages.write().unwrap();
            pending.push(PackageDir {
                path: dir.clone(),
                ecosystem: Ecosystem::Cargo,
            });
        }

        // Before indexing — no results
        assert!(idx.deps.hover_info("dep_helper").is_none());

        // Index pending
        idx.index_pending();

        // Now it should be found
        let info = idx.deps.hover_info("dep_helper");
        assert!(info.is_some());
        let (sig, doc) = info.unwrap();
        assert!(sig.unwrap().contains("dep_helper"));
        assert!(doc.unwrap().contains("dep helper"));

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn detect_and_resolve_cargo() {
        // Test with the actual repo
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(|p| p.parent())
            .expect("repo root");

        let idx = DependencyIndex::new();
        idx.detect_and_resolve(repo_root);

        // Should find Cargo deps
        let pending = idx.pending_count();
        let pkg_count = idx.package_count();

        // Either pending or already indexed
        assert!(
            pending > 0 || pkg_count > 0 || !repo_root.join("Cargo.lock").exists(),
            "Should have resolved some cargo deps from Cargo.lock"
        );
    }
}
