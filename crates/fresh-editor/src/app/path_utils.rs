//! Path manipulation helpers.
//!
//! Pure functions operating on `std::path::Path` values. No state, no I/O,
//! no dependencies on `Editor`. See `docs/internal/editor-modules-refactor-plan.md`
//! (phase 1) for why these live here instead of on `Editor`.

use std::path::{Component, Path, PathBuf};

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
