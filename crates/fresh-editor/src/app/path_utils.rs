//! Path manipulation helpers.
//!
//! Pure functions operating on `std::path::Path` values. No state, no I/O,
//! no dependencies on `Editor`. See `docs/internal/editor-modules-refactor-plan.md`
//! (phase 1) for why these live here instead of on `Editor`.

use std::path::{Component, Path, PathBuf};

/// Exact counts for the editor-thread cost of explorer path admission.
///
/// `normalize_explorer_plugin_path` exists so ordinary already-canonical
/// paths never touch the filesystem: a plugin sending 25 000 decorations
/// pays one lexical prefix check each, not 25 000 canonicalizations. A
/// duration cannot pin that in CI — it moves with the machine, the build
/// profile and the runner's load — but whether the canonical fallback ran
/// is exact, so a test can assert the shape directly (see the `stats`
/// tests below, the same doctrine as `PerfCounters`).
///
/// Thread-local, because the editor dispatches plugin commands on one
/// thread and tests run one editor per thread. Counted only with debug
/// assertions on; the release build pays nothing.
pub(crate) mod stats {
    #[cfg(debug_assertions)]
    use std::cell::Cell;

    #[cfg(debug_assertions)]
    thread_local! {
        static CANONICAL_FALLBACKS: Cell<u64> = const { Cell::new(0) };
    }

    #[inline]
    pub(crate) fn note_canonical_fallback() {
        #[cfg(debug_assertions)]
        CANONICAL_FALLBACKS.with(|c| c.set(c.get().saturating_add(1)));
    }

    /// The canonical fallbacks since the last call, which resets the count.
    #[cfg(all(test, debug_assertions))]
    pub(crate) fn take_canonical_fallbacks() -> u64 {
        CANONICAL_FALLBACKS.with(|c| c.replace(0))
    }
}

/// Normalize a plugin-supplied explorer path so it matches the native paths
/// stored on file-tree nodes.
///
/// Plugins build paths via `editor.pathJoin()`, which always emits forward
/// slashes even on Windows. The explorer tree is rooted at a canonicalized
/// `window.root` with native separators, so a naïve `starts_with` / hash
/// lookup would silently drop every decoration/slot override on Windows.
pub(crate) fn normalize_explorer_plugin_path(path: &Path, root: &Path) -> PathBuf {
    let path = normalize_path(path);
    if path.starts_with(root) {
        return path;
    }
    stats::note_canonical_fallback();
    let root_key = explorer_path_key(root);

    for candidate in explorer_path_candidates(&path) {
        let key = explorer_path_key(&candidate);
        if let Ok(relative) = key.strip_prefix(&root_key) {
            return if relative.as_os_str().is_empty() {
                root.to_path_buf()
            } else {
                root.join(relative)
            };
        }
    }

    path
}

/// Return true when `path` lies under `root`, tolerant of Windows separator
/// and `\\?\` extended-prefix differences between plugin and tree paths.
pub(crate) fn explorer_path_under_root(path: &Path, root: &Path) -> bool {
    if path.starts_with(root) {
        return true;
    }
    let root_key = explorer_path_key(root);
    explorer_path_candidates(path)
        .into_iter()
        .any(|candidate| explorer_path_key(&candidate).starts_with(&root_key))
}

fn explorer_path_candidates(path: &Path) -> Vec<PathBuf> {
    let mut out = vec![path.to_path_buf()];
    let lossy = path.to_string_lossy();
    if lossy.contains('/') {
        out.push(PathBuf::from(lossy.replace('/', "\\")));
    }
    if lossy.contains('\\') {
        out.push(PathBuf::from(lossy.replace('\\', "/")));
    }
    out
}

fn explorer_path_key(path: &Path) -> PathBuf {
    #[cfg(windows)]
    {
        if let Ok(c) = path.canonicalize() {
            return strip_windows_extended_prefix(c);
        }
        if let Some(c) = canonicalize_deepest_existing(path) {
            return strip_windows_extended_prefix(c);
        }
        return strip_windows_extended_prefix(path.to_path_buf());
    }
    #[cfg(not(windows))]
    {
        path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
    }
}

#[cfg(windows)]
fn strip_windows_extended_prefix(path: PathBuf) -> PathBuf {
    let s = path.to_string_lossy();
    if let Some(stripped) = s.strip_prefix(r"\\?\") {
        PathBuf::from(stripped)
    } else {
        path
    }
}

#[cfg(windows)]
fn canonicalize_deepest_existing(path: &Path) -> Option<PathBuf> {
    if let Ok(c) = path.canonicalize() {
        return Some(c);
    }
    let mut tail: Vec<&std::ffi::OsStr> = Vec::new();
    let mut ancestor = path;
    loop {
        let parent = ancestor.parent()?;
        if let Some(name) = ancestor.file_name() {
            tail.push(name);
        }
        if let Ok(c) = parent.canonicalize() {
            let mut out = c;
            for name in tail.iter().rev() {
                out.push(name);
            }
            return Some(out);
        }
        ancestor = parent;
    }
}

/// Normalize a path by resolving `.` and `..` components without requiring
/// the path to exist. Similar to `canonicalize` but works on paths that
/// don't exist yet.
pub(crate) fn normalize_path(path: &Path) -> PathBuf {
    let mut components = Vec::new();

    for component in path.components() {
        match component {
            Component::CurDir => {
                // Skip "." components
            }
            Component::ParentDir => {
                // Pop the last component if it's a normal component
                if let Some(Component::Normal(_)) = components.last() {
                    components.pop();
                } else {
                    // Keep ".." if we can't go up further (for relative paths)
                    components.push(component);
                }
            }
            _ => {
                components.push(component);
            }
        }
    }

    if components.is_empty() {
        PathBuf::from(".")
    } else {
        components.iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn normalize_explorer_plugin_path_accepts_forward_slash_absolute_paths() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let file = root.join("foo.txt");
        fs::write(&file, "hello").unwrap();

        let plugin_style = PathBuf::from(format!(
            "{}/foo.txt",
            root.to_string_lossy().replace('\\', "/")
        ));

        let normalized = normalize_explorer_plugin_path(&plugin_style, &root);
        assert_eq!(normalized, file);
        assert!(explorer_path_under_root(&plugin_style, &root));
    }

    #[test]
    fn normalize_path_resolves_dot_segments() {
        let path = Path::new("/foo/./bar/../baz");
        assert_eq!(normalize_path(path), PathBuf::from("/foo/baz"));
    }

    #[test]
    #[cfg(debug_assertions)]
    fn in_root_paths_skip_the_canonical_fallback() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let src = root.join("src");
        fs::create_dir_all(&src).unwrap();

        stats::take_canonical_fallbacks();
        for i in 0..25_000 {
            let path = normalize_explorer_plugin_path(&src.join(format!("gen_{i}.rs")), &root);
            assert!(path.starts_with(&root));
        }
        assert_eq!(
            stats::take_canonical_fallbacks(),
            0,
            "already-canonical paths paid the canonical fallback: the lexical \
             fast path regressed, so a plugin decoration batch costs a \
             filesystem canonicalization per path again"
        );
    }

    #[test]
    #[cfg(debug_assertions)]
    fn off_root_and_alias_paths_still_use_the_fallback() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let outside = temp.path().join("elsewhere").join("a.rs");

        stats::take_canonical_fallbacks();
        normalize_explorer_plugin_path(&outside, &root);
        assert!(
            stats::take_canonical_fallbacks() > 0,
            "a path outside the root must keep the canonical fallback available"
        );
    }
}
