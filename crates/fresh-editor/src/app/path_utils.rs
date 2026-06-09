//! Path manipulation helpers.
//!
//! Pure functions operating on `std::path::Path` values. No state, no I/O,
//! no dependencies on `Editor`. See `docs/internal/editor-modules-refactor-plan.md`
//! (phase 1) for why these live here instead of on `Editor`.

use std::path::{Component, Path, PathBuf};

/// Normalize a plugin-supplied explorer path so it matches the native paths
/// stored on file-tree nodes.
///
/// Plugins build paths via `editor.pathJoin()`, which always emits forward
/// slashes even on Windows. The explorer tree is rooted at a canonicalized
/// `window.root` with native separators, so a naïve `starts_with` / hash
/// lookup would silently drop every decoration/slot override on Windows.
pub(crate) fn normalize_explorer_plugin_path(path: &Path, root: &Path) -> PathBuf {
    let path = normalize_path(path);
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
}
