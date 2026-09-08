//! Path manipulation helpers.
//!
//! Pure functions operating on `std::path::Path` values. No state, no I/O,
//! no dependencies on `Editor`. See `docs/internal/editor-modules-refactor-plan.md`
//! (phase 1) for why these live here instead of on `Editor`.

use std::cell::OnceCell;
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

    #[cfg(debug_assertions)]
    thread_local! {
        static ROOT_KEYS: Cell<u64> = const { Cell::new(0) };
    }

    /// Counted where the root's canonical spelling is actually built.
    ///
    /// The root's key is one answer for a whole batch, so this is the
    /// counter that says whether it is being rebuilt per path — the cost
    /// [`super::ExplorerRoot`] exists to remove, and the one a duration
    /// cannot pin in CI any more than the fallback count above.
    #[inline]
    pub(crate) fn note_root_key_build() {
        #[cfg(debug_assertions)]
        ROOT_KEYS.with(|c| c.set(c.get().saturating_add(1)));
    }

    /// The root keys built since the last call, which resets the count.
    #[cfg(all(test, debug_assertions))]
    pub(crate) fn take_root_key_builds() -> u64 {
        ROOT_KEYS.with(|c| c.replace(0))
    }
}

/// A window root prepared to admit a batch of plugin-supplied explorer paths.
///
/// The root's canonical spelling is one answer for every path in a batch,
/// but the free functions below each rebuilt it per path: two
/// `canonicalize` syscalls per decoration whose result never varies, so a
/// plugin sending 25 000 decorations paid 50 000 of them to learn one
/// thing. Hold it here instead, built at most once for the whole batch.
///
/// Built *lazily*, not eagerly. The lexical fast path admits an ordinary
/// in-root path without consulting the filesystem at all, so a batch that
/// stays on it must not pay even one canonicalization — an eager key would
/// put a syscall back into the case that already has none. The `stats`
/// counters above pin both halves of that: zero root keys for an in-root
/// batch, exactly one for a batch that needs the fallback.
pub(crate) struct ExplorerRoot<'a> {
    root: &'a Path,
    key: OnceCell<PathBuf>,
}

impl<'a> ExplorerRoot<'a> {
    /// Prepare `root` — canonical, native-separator, as the file tree
    /// stores it — to admit paths. Cheap: nothing is resolved until a path
    /// actually needs the canonical spelling.
    pub(crate) fn new(root: &'a Path) -> Self {
        Self {
            root,
            key: OnceCell::new(),
        }
    }

    /// The root's canonical key, resolved on first use and kept.
    fn key(&self) -> &Path {
        self.key.get_or_init(|| {
            stats::note_root_key_build();
            explorer_path_key(self.root)
        })
    }

    /// Admit a plugin path into this root, rewritten into the spelling the
    /// file tree stores — or `None` when it lies outside the root.
    ///
    /// One pass where the call sites used to make two. Testing the path and
    /// rewriting it asked the same question (`starts_with` and
    /// `strip_prefix` over the same keys), so the normalization, the
    /// candidate list and the root key were all built twice per path to
    /// produce one decision. The gate is now the rewrite: a path is in the
    /// root exactly when this returns its rewritten form.
    pub(crate) fn admit(&self, path: &Path) -> Option<PathBuf> {
        let path = normalize_path(path);
        if path.starts_with(self.root) {
            return Some(path);
        }
        stats::note_canonical_fallback();
        let root_key = self.key();

        for candidate in explorer_path_candidates(&path) {
            let key = explorer_path_key(&candidate);
            if let Ok(relative) = key.strip_prefix(root_key) {
                return Some(if relative.as_os_str().is_empty() {
                    self.root.to_path_buf()
                } else {
                    self.root.join(relative)
                });
            }
        }

        None
    }
}

/// Normalize a plugin-supplied explorer path so it matches the native paths
/// stored on file-tree nodes.
///
/// Plugins build paths via `editor.pathJoin()`, which always emits forward
/// slashes even on Windows. The explorer tree is rooted at a canonicalized
/// `window.root` with native separators, so a naïve `starts_with` / hash
/// lookup would silently drop every decoration/slot override on Windows.
///
/// Single-path form. A batch should build one [`ExplorerRoot`] and call
/// [`ExplorerRoot::admit`], which resolves the root once for all of them.
pub(crate) fn normalize_explorer_plugin_path(path: &Path, root: &Path) -> PathBuf {
    ExplorerRoot::new(root)
        .admit(path)
        .unwrap_or_else(|| normalize_path(path))
}

/// Return true when `path` lies under `root`, tolerant of Windows separator
/// and `\\?\` extended-prefix differences between plugin and tree paths.
///
/// Single-path form, as above.
pub(crate) fn explorer_path_under_root(path: &Path, root: &Path) -> bool {
    ExplorerRoot::new(root).admit(path).is_some()
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
    fn a_batch_builds_the_root_key_at_most_once() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().canonicalize().unwrap();

        // Off-root, so every path is forced past the lexical fast path and
        // onto the fallback that needs the root's canonical spelling.
        let elsewhere = TempDir::new().unwrap();

        let explorer_root = ExplorerRoot::new(&root);
        stats::take_root_key_builds();
        stats::take_canonical_fallbacks();
        for i in 0..25_000 {
            assert!(
                explorer_root
                    .admit(&elsewhere.path().join(format!("gen_{i}.rs")))
                    .is_none(),
                "an off-root path must not be admitted"
            );
        }

        assert_eq!(
            stats::take_canonical_fallbacks(),
            25_000,
            "every off-root path should have reached the fallback"
        );
        assert_eq!(
            stats::take_root_key_builds(),
            1,
            "the root's canonical spelling is one answer for the batch: \
             rebuilding it per path is a `canonicalize` syscall per \
             decoration that the batch already knows"
        );
    }

    #[test]
    #[cfg(debug_assertions)]
    fn an_in_root_batch_builds_no_root_key_at_all() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let src = root.join("src");
        fs::create_dir_all(&src).unwrap();

        let explorer_root = ExplorerRoot::new(&root);
        stats::take_root_key_builds();
        for i in 0..25_000 {
            assert!(explorer_root
                .admit(&src.join(format!("gen_{i}.rs")))
                .is_some());
        }

        assert_eq!(
            stats::take_root_key_builds(),
            0,
            "the lexical fast path answers without the filesystem, so the \
             root key must stay unbuilt: building it eagerly would put a \
             syscall back into the case that has none"
        );
    }

    #[test]
    fn admit_agrees_with_the_single_path_helpers() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let inside = root.join("src/lib.rs");
        let elsewhere = TempDir::new().unwrap();
        let outside = elsewhere.path().join("a.rs");

        let explorer_root = ExplorerRoot::new(&root);
        for path in [inside.as_path(), outside.as_path(), root.as_path()] {
            let admitted = explorer_root.admit(path);
            assert_eq!(
                admitted.is_some(),
                explorer_path_under_root(path, &root),
                "admit must gate exactly as explorer_path_under_root did for {path:?}"
            );
            if let Some(admitted) = admitted {
                assert_eq!(
                    admitted,
                    normalize_explorer_plugin_path(path, &root),
                    "admit must rewrite exactly as normalize_explorer_plugin_path did \
                     for {path:?}"
                );
            }
        }
    }

    #[test]
    #[cfg(debug_assertions)]
    fn off_root_and_alias_paths_still_use_the_fallback() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().canonicalize().unwrap();

        // The second temp dir is a sibling, so it is outside the root on
        // every platform. A path spelled inside the first temp dir would
        // not be: on Linux the temp dir is already its canonical spelling,
        // so it stays on the lexical fast path and never reaches the
        // fallback.
        let elsewhere = TempDir::new().unwrap();
        let off_root = elsewhere.path().join("a.rs");
        fs::write(&off_root, "").unwrap();

        stats::take_canonical_fallbacks();
        let admitted = normalize_explorer_plugin_path(&off_root, &root);
        assert!(
            stats::take_canonical_fallbacks() > 0,
            "a path outside the root must keep the canonical fallback available"
        );
        assert!(
            !admitted.starts_with(&root),
            "an off-root path must stay off-root, not be grafted under the root"
        );

        // An alias spelling of an in-root path must resolve back under the
        // root (the same symlink fixture the file_explorer e2e suite uses).
        #[cfg(unix)]
        {
            let in_root = temp.path().join("b.rs");
            fs::write(&in_root, "").unwrap();
            let alias = elsewhere.path().join("into_root");
            std::os::unix::fs::symlink(temp.path(), &alias).unwrap();

            stats::take_canonical_fallbacks();
            let normalized = normalize_explorer_plugin_path(&alias.join("b.rs"), &root);
            assert!(
                stats::take_canonical_fallbacks() > 0,
                "an alias spelling must still reach the canonical fallback"
            );
            assert_eq!(
                normalized,
                root.join("b.rs"),
                "an in-root alias must normalize back onto the canonical root"
            );
        }
    }
}
