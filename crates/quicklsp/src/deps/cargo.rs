//! Cargo/Rust dependency resolution.
//!
//! Parses `Cargo.lock` for the full transitive dependency list and resolves
//! each crate to its source directory in `~/.cargo/registry/src/`.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use super::files;
use super::{Ecosystem, PackageDir};

/// Parse `Cargo.lock` to extract all dependency names and versions (transitive).
///
/// Returns `(name, version)` pairs for every package in the lock file.
pub fn parse_lock_file(workspace_root: &Path) -> Vec<(String, String)> {
    let lock_path = workspace_root.join("Cargo.lock");
    let content = match std::fs::read_to_string(&lock_path) {
        Ok(c) => c,
        Err(_) => return Vec::new(),
    };

    let mut deps = Vec::new();
    let mut current_name: Option<String> = None;

    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed == "[[package]]" {
            current_name = None;
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("name = ") {
            current_name = Some(unquote(rest).to_string());
        }
        if let Some(rest) = trimmed.strip_prefix("version = ") {
            if let Some(name) = current_name.take() {
                deps.push((name, unquote(rest).to_string()));
            }
        }
    }

    deps
}

/// Resolve declared dependencies to their source directories on disk.
///
/// Searches `~/.cargo/registry/src/*/` for directories matching `name-version`.
pub fn resolve_package_dirs(
    workspace_root: &Path,
    declared_deps: &[(String, String)],
) -> Vec<PackageDir> {
    let registry_roots = find_registry_roots();
    let mut result = Vec::new();

    // Build a set of expected directory names: "name-version"
    let expected: HashSet<String> = declared_deps
        .iter()
        .map(|(name, version)| format!("{name}-{version}"))
        .collect();

    for registry_root in &registry_roots {
        let entries = match std::fs::read_dir(registry_root) {
            Ok(e) => e,
            Err(_) => continue,
        };

        for entry in entries.flatten() {
            let dir = entry.path();
            if !dir.is_dir() {
                continue;
            }
            if let Some(dir_name) = dir.file_name().and_then(|n| n.to_str()) {
                if expected.contains(dir_name) {
                    result.push(PackageDir {
                        path: dir,
                        ecosystem: Ecosystem::Cargo,
                    });
                }
            }
        }
    }

    // Also check for path dependencies in the workspace
    // (Cargo workspace members that are local paths)
    let workspace_target = workspace_root.join("target").join("debug").join("build");
    if workspace_target.is_dir() {
        // Skip target/debug/build — it contains build script output, not source
    }

    result
}

/// Find all cargo registry source directories.
fn find_registry_roots() -> Vec<PathBuf> {
    let mut roots = Vec::new();
    if let Some(cargo_home) = cargo_home() {
        let registry_src = cargo_home.join("registry").join("src");
        if let Ok(entries) = std::fs::read_dir(&registry_src) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    roots.push(path);
                }
            }
        }
    }
    roots
}

/// Check if any registry root directories exist.
pub fn registry_roots_available() -> bool {
    !find_registry_roots().is_empty()
}

/// File extensions for Rust source files.
pub fn extensions() -> &'static [&'static str] {
    &["rs"]
}

/// Whether Rust prefers stubs over source (no — always index .rs directly).
pub fn prefers_stubs() -> bool {
    false
}

/// Collect source files for a Cargo package.
///
/// Prioritizes `src/lib.rs` (public API) and `src/` directory. Skips examples,
/// benches, and build scripts to keep indexing fast.
pub fn collect_package_files(pkg_dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    let src_dir = pkg_dir.join("src");

    // Priority 1: src/lib.rs contains the public API
    let lib_rs = src_dir.join("lib.rs");
    if lib_rs.is_file() {
        files.push(lib_rs);
    }

    // Priority 2: src/mod.rs or top-level .rs files
    let scan_dir = if src_dir.is_dir() { &src_dir } else { pkg_dir };
    files::collect_source_files(scan_dir, extensions(), prefers_stubs(), 0, &mut files);

    // Dedup (lib.rs may have been added twice)
    files.sort();
    files.dedup();

    if files.len() > files::MAX_FILES_PER_PACKAGE {
        files.truncate(files::MAX_FILES_PER_PACKAGE);
    }
    files
}

fn cargo_home() -> Option<PathBuf> {
    if let Ok(ch) = std::env::var("CARGO_HOME") {
        return Some(PathBuf::from(ch));
    }
    super::home_dir().map(|h| h.join(".cargo"))
}

fn unquote(s: &str) -> &str {
    s.trim().trim_matches('"')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_cargo_lock_format() {
        let dir = std::env::temp_dir().join("quicklsp_test_cargo_lock");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("Cargo.lock"),
            r#"# This file is automatically @generated by Cargo.
[[package]]
name = "ahash"
version = "0.8.12"

[[package]]
name = "dashmap"
version = "6.1.0"
dependencies = [
 "ahash",
]

[[package]]
name = "myproject"
version = "0.1.0"
"#,
        )
        .unwrap();

        let deps = parse_lock_file(&dir);
        assert_eq!(deps.len(), 3);
        assert!(deps.iter().any(|(n, v)| n == "ahash" && v == "0.8.12"));
        assert!(deps.iter().any(|(n, v)| n == "dashmap" && v == "6.1.0"));

        let _ = std::fs::remove_dir_all(&dir);
    }
}
