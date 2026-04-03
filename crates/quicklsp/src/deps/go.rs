//! Go module dependency resolution.
//!
//! Parses `go.sum` for the full transitive dependency list and resolves
//! modules from `$GOMODCACHE` (defaults to `~/go/pkg/mod/`).

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use super::files;
use super::{Ecosystem, PackageDir};

/// Parse `go.sum` to extract all transitive module paths and versions.
///
/// Returns `(module_path, version)` pairs. go.sum contains the full transitive
/// closure.
pub fn parse_go_sum(workspace_root: &Path) -> Vec<(String, String)> {
    let sum_path = workspace_root.join("go.sum");
    let content = match std::fs::read_to_string(&sum_path) {
        Ok(c) => c,
        Err(_) => return Vec::new(),
    };

    let mut seen = HashSet::new();
    let mut deps = Vec::new();

    for line in content.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() >= 2 {
            let module = parts[0];
            let version = parts[1].strip_suffix("/go.mod").unwrap_or(parts[1]);
            let key = format!("{module}@{version}");
            if seen.insert(key) {
                deps.push((module.to_string(), version.to_string()));
            }
        }
    }

    deps
}

/// Resolve declared Go modules to their cache directories.
///
/// Go module cache uses escaped paths: uppercase letters become `!` + lowercase.
pub fn resolve_package_dirs(
    _workspace_root: &Path,
    declared_deps: &[(String, String)],
) -> Vec<PackageDir> {
    let cache = match go_mod_cache() {
        Some(c) => c,
        None => return Vec::new(),
    };

    let mut result = Vec::new();

    for (module, version) in declared_deps {
        // Go module cache path: $GOMODCACHE/module@version
        // Module path has case-encoded segments
        let encoded = encode_module_path(module);
        let dir = cache.join(format!("{encoded}@{version}"));
        if dir.is_dir() {
            result.push(PackageDir {
                path: dir,
                ecosystem: Ecosystem::Go,
            });
        }
    }

    result
}

/// Check if the Go module cache exists.
pub fn mod_cache_available() -> bool {
    go_mod_cache().map_or(false, |c| c.is_dir())
}

/// File extensions for Go.
pub fn extensions() -> &'static [&'static str] {
    &["go"]
}

/// Go does not use stub files.
pub fn prefers_stubs() -> bool {
    false
}

/// Collect source files for a Go module.
pub fn collect_package_files(pkg_dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    files::collect_source_files(pkg_dir, extensions(), prefers_stubs(), 0, &mut files);
    if files.len() > files::MAX_FILES_PER_PACKAGE {
        files.truncate(files::MAX_FILES_PER_PACKAGE);
    }
    files
}

fn go_mod_cache() -> Option<PathBuf> {
    if let Ok(cache) = std::env::var("GOMODCACHE") {
        return Some(PathBuf::from(cache));
    }
    if let Ok(gopath) = std::env::var("GOPATH") {
        return Some(PathBuf::from(gopath).join("pkg").join("mod"));
    }
    super::home_dir().map(|h| h.join("go").join("pkg").join("mod"))
}

/// Encode a Go module path for the filesystem cache.
///
/// Go encodes uppercase letters as `!` followed by the lowercase letter.
fn encode_module_path(module: &str) -> String {
    let mut encoded = String::with_capacity(module.len());
    for ch in module.chars() {
        if ch.is_ascii_uppercase() {
            encoded.push('!');
            encoded.push(ch.to_ascii_lowercase());
        } else {
            encoded.push(ch);
        }
    }
    encoded
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_go_sum_format() {
        let dir = std::env::temp_dir().join("quicklsp_test_go");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("go.sum"),
            "github.com/pkg/errors v0.9.1 h1:abc123=\n\
             github.com/pkg/errors v0.9.1/go.mod h1:def456=\n\
             golang.org/x/sync v0.7.0 h1:ghi789=\n",
        )
        .unwrap();

        let deps = parse_go_sum(&dir);
        assert_eq!(deps.len(), 2); // deduped
        assert!(deps.iter().any(|(m, _)| m == "github.com/pkg/errors"));
        assert!(deps.iter().any(|(m, _)| m == "golang.org/x/sync"));

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn encode_module_path_cases() {
        assert_eq!(encode_module_path("github.com/Azure/go-sdk"), "github.com/!azure/go-sdk");
        assert_eq!(encode_module_path("github.com/pkg/errors"), "github.com/pkg/errors");
    }
}
