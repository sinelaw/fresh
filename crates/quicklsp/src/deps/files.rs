//! Shared file collection utilities for dependency source indexing.

use std::path::{Path, PathBuf};

/// Maximum number of files to index per package (safety limit).
/// Keeps per-package indexing fast. Most useful doc/signature info is in the
/// top-level source files anyway.
pub const MAX_FILES_PER_PACKAGE: usize = 20;

/// Maximum directory depth to recurse into a package.
const MAX_DEPTH: usize = 4;

/// Recursively collect source files from a package directory.
///
/// When `prefers_stubs` is true and both a stub (.pyi/.d.ts) and source
/// (.py/.ts) exist for the same module, only the stub is included.
pub fn collect_source_files(
    dir: &Path,
    extensions: &[&str],
    prefers_stubs: bool,
    depth: usize,
    out: &mut Vec<PathBuf>,
) {
    if depth > MAX_DEPTH || out.len() >= MAX_FILES_PER_PACKAGE {
        return;
    }

    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };

    let mut files_here: Vec<PathBuf> = Vec::new();
    let mut subdirs: Vec<PathBuf> = Vec::new();

    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            // Skip test/example/benchmark directories in deps
            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                if matches!(
                    name,
                    "test" | "tests" | "testing" | "examples" | "benches" | "benchmarks"
                        | "__pycache__" | ".git" | "node_modules"
                ) {
                    continue;
                }
            }
            subdirs.push(path);
        } else if path.is_file() {
            let path_str = path.to_string_lossy();
            for ext in extensions {
                if path_str.ends_with(ext) {
                    files_here.push(path);
                    break;
                }
            }
        }
    }

    // Stub preference filtering: if both foo.pyi and foo.py exist, keep only .pyi
    if prefers_stubs && extensions.len() >= 2 {
        let stub_ext = extensions[0]; // e.g., "pyi" or "d.ts"
        let stub_stems: std::collections::HashSet<String> = files_here
            .iter()
            .filter(|f| f.to_string_lossy().ends_with(stub_ext))
            .filter_map(|f| {
                let s = f.to_string_lossy();
                let stem = s.strip_suffix(stub_ext)?;
                Some(stem.to_string())
            })
            .collect();

        if !stub_stems.is_empty() {
            files_here.retain(|f| {
                let s = f.to_string_lossy();
                if s.ends_with(stub_ext) {
                    return true; // always keep stubs
                }
                for ext in &extensions[1..] {
                    if let Some(stem) = s.strip_suffix(ext) {
                        if stub_stems.contains(&format!("{stem}{stub_ext}"))
                            || stub_stems.contains(stem)
                        {
                            return false; // skip source, stub exists
                        }
                    }
                }
                true
            });
        }
    }

    out.extend(files_here);

    for subdir in subdirs {
        if out.len() >= MAX_FILES_PER_PACKAGE {
            break;
        }
        collect_source_files(&subdir, extensions, prefers_stubs, depth + 1, out);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn collect_files_respects_depth_limit() {
        let dir = std::env::temp_dir().join("quicklsp_test_depth");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("a/b/c/d/e/f/g")).unwrap();
        std::fs::write(dir.join("a/b/c/d/e/f/g/deep.rs"), "fn deep() {}").unwrap();
        std::fs::write(dir.join("a/top.rs"), "fn top() {}").unwrap();

        let mut files = Vec::new();
        collect_source_files(&dir, &["rs"], false, 0, &mut files);

        assert!(files.iter().any(|f| f.ends_with("top.rs")));

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn collect_files_skips_test_dirs() {
        let dir = std::env::temp_dir().join("quicklsp_test_skip");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("src")).unwrap();
        std::fs::create_dir_all(dir.join("tests")).unwrap();
        std::fs::write(dir.join("src/lib.rs"), "fn lib() {}").unwrap();
        std::fs::write(dir.join("tests/test.rs"), "fn test() {}").unwrap();

        let mut files = Vec::new();
        collect_source_files(&dir, &["rs"], false, 0, &mut files);

        assert!(files.iter().any(|f| f.ends_with("lib.rs")));
        assert!(!files.iter().any(|f| f.ends_with("test.rs")));

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn stub_preference_filters_source() {
        let dir = std::env::temp_dir().join("quicklsp_test_stubs");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("utils.pyi"), "def foo(x: int) -> str: ...").unwrap();
        std::fs::write(dir.join("utils.py"), "def foo(x): return str(x)").unwrap();
        std::fs::write(dir.join("helper.py"), "def bar(): pass").unwrap();

        let mut files = Vec::new();
        collect_source_files(&dir, &["pyi", "py"], true, 0, &mut files);

        assert!(files.iter().any(|f| f.ends_with("utils.pyi")));
        assert!(!files.iter().any(|f| f.ends_with("utils.py")));
        assert!(files.iter().any(|f| f.ends_with("helper.py")));

        let _ = std::fs::remove_dir_all(&dir);
    }
}
