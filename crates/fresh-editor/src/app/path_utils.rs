//! Path manipulation helpers.
//!
//! Helpers over `std::path::Path` values, with no state and no dependency on
//! `Editor`. See `docs/internal/editor-modules-refactor-plan.md` (phase 1)
//! for why these live here instead of on `Editor`.
//!
//! They are not all *pure*: resolving a path through symlinks is a question
//! only a filesystem can answer, and the ones that need it take the
//! filesystem rather than reaching for `std::fs`, which would answer for the
//! client when the path belongs to a remote host.

use crate::model::filesystem::FileSystem;
use fresh_editor_core::model::host_path::{AsHostPath, HostPathBuf, PathStyle};
use std::cell::OnceCell;
use std::path::{Path, PathBuf};

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
    fs: &'a dyn FileSystem,
    root: &'a Path,
    key: OnceCell<PathBuf>,
}

impl<'a> ExplorerRoot<'a> {
    /// Prepare `root` — as the file tree stores it — to admit paths. `fs` is
    /// the filesystem the root lives on: it says how the paths read, and it
    /// is the one asked to canonicalize them. Cheap: nothing is resolved
    /// until a path actually needs the canonical spelling.
    pub(crate) fn new(fs: &'a dyn FileSystem, root: &'a Path) -> Self {
        Self {
            fs,
            root,
            key: OnceCell::new(),
        }
    }

    /// The root's canonical key, resolved on first use and kept.
    fn key(&self) -> &Path {
        self.key.get_or_init(|| {
            stats::note_root_key_build();
            explorer_path_key(self.fs, self.root)
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
        let path = normalize_path(self.fs.style(), path);

        // Both branches end the same way: the admitted path is rebuilt **on
        // the root**, never handed back as it arrived. The caller stores it on
        // a tree node and hashes it as a cache key, and the tree is spelled
        // the way `window.root` is, so a plugin's `//?/D:/a` has to come back
        // as the root's `\\?\D:\a` to match anything.
        //
        // The old lexical branch returned the path unchanged, and got away
        // with it only because `Path::starts_with` compares a *parsed* Windows
        // prefix: `//?/D:` parses as `UNC("?", "D:")` and `\\?\D:` as
        // `VerbatimDisk`, so it never matched and every such path fell to the
        // fallback, which did rebuild it. Reading the paths under the host's
        // rules fixed that comparison — `/` and `\` are one separator on
        // Windows — and so exposed the return value: the fast path started
        // hitting, and started returning the plugin's spelling.
        let rebuilt_on_root = |relative: String| {
            if relative.is_empty() {
                self.root.to_path_buf()
            } else {
                self.fs
                    .at(self.root.as_host())
                    .join_rel(&relative)
                    .into_path_buf()
            }
        };

        // Asked under the root's own rules. A Windows host treats `/` and `\`
        // as the same separator, so a plugin's forward slashes match a tree
        // spelled with backslashes; a POSIX host keeps `\` inside a name, so
        // they do not get confused for separators there. `Path::starts_with`
        // asked the *client*, and the separator spellings it could not match
        // were papered over by re-spelling every path three ways and trying
        // each — which on a POSIX host invented paths naming other files.
        //
        // `rel_to` is the same question as `starts_with` and answers with the
        // tail as well, so the gate and the rewrite are one call.
        if let Some(relative) = self.fs.at(path.as_host()).rel_to(self.root.as_host()) {
            return Some(rebuilt_on_root(relative));
        }
        stats::note_canonical_fallback();
        let root_key = self.key();
        let key = explorer_path_key(self.fs, &path);
        let relative = self.fs.at(key.as_host()).rel_to(root_key.as_host())?;
        Some(rebuilt_on_root(relative))
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
/// Single-path form, kept as the tests' statement of what
/// [`ExplorerRoot::admit`] must do for one path; the editor builds one
/// [`ExplorerRoot`] per batch and admits through it.
#[cfg(test)]
pub(crate) fn normalize_explorer_plugin_path(
    fs: &dyn FileSystem,
    path: &Path,
    root: &Path,
) -> PathBuf {
    ExplorerRoot::new(fs, root)
        .admit(path)
        .unwrap_or_else(|| normalize_path(fs.style(), path))
}

/// Return true when `path` lies under `root`, tolerant of Windows separator
/// and `\\?\` extended-prefix differences between plugin and tree paths.
///
/// Single-path form.
pub(crate) fn explorer_path_under_root(fs: &dyn FileSystem, path: &Path, root: &Path) -> bool {
    ExplorerRoot::new(fs, root).admit(path).is_some()
}

/// Canonicalize the deepest existing ancestor of `path` and put its missing
/// components back on. Unlike `canonicalize` alone, this also answers for a
/// target file, and parent directories, that have not been created yet.
///
/// Climbing the path is the filesystem's business too: `Path::parent` splits
/// on the client's separator, so on a Windows client a remote
/// `/srv/proj/a/b` had no parent to climb to at all and the walk gave up at
/// the first step.
pub(crate) fn canonicalize_deepest_existing(
    fs: &dyn FileSystem,
    path: &Path,
) -> std::io::Result<HostPathBuf> {
    let mut ancestor = path.as_host().to_host_path_buf();
    let mut missing_tail: Vec<std::ffi::OsString> = Vec::new();

    loop {
        match fs.canonicalize(ancestor.as_host_path()) {
            Ok(mut canonical) => {
                for component in missing_tail.iter().rev() {
                    canonical.push_name_in(fs.style(), component);
                }
                return Ok(fs.at(&canonical).normalize());
            }
            // Only a missing component means "not created yet". Treating
            // every failure that way (EACCES, ELOOP, …) would silently
            // downgrade the symlink escape check to a lexical one, so
            // surface anything else to the caller.
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                // The last *component*, not the file name: `..` is a
                // component this walk must step over and put back (the
                // `normalize` above resolves it once an ancestor has
                // canonicalized), and `file_name` reports `..` as no name at
                // all, as `Path::file_name` does. Taking the file name here
                // made the walk give up on `<root>/src/../app.rs` and return
                // the `NotFound` instead of climbing.
                let at = fs.at(ancestor.as_host_path());
                let (Some(parent), Some(component)) = (at.parent(), at.components().next_back())
                else {
                    return Err(error);
                };
                missing_tail.push(component.to_os_string());
                ancestor = parent.to_host_path_buf();
            }
            Err(error) => return Err(error),
        }
    }
}

/// The spelling two paths have to be reduced to before they can be compared:
/// resolved through symlinks and aliases by the filesystem that owns them,
/// with a Windows verbatim prefix taken off.
///
/// A path that does not exist yet still gets a key, from its deepest existing
/// ancestor — a plugin may decorate a file it is about to create.
///
/// It used to call `Path::canonicalize`, which resolves on *this* machine. For
/// a remote root that is the wrong filesystem: every answer was either a
/// coincidence of the client having a path of the same name, or a failure
/// falling back to the path unchanged.
fn explorer_path_key(fs: &dyn FileSystem, path: &Path) -> PathBuf {
    let canonical = fs
        .canonicalize(path.as_host())
        .or_else(|_| canonicalize_deepest_existing(fs, path))
        .unwrap_or_else(|_| path.as_host().to_host_path_buf());
    fs.at(canonical.as_host_path())
        .strip_verbatim_prefix()
        .into_path_buf()
}

/// Normalize a path by resolving `.` and `..` components without requiring
/// the path to exist. Similar to `canonicalize` but works on paths that
/// don't exist yet.
///
/// Normalization and spelling are two jobs, and this does them in that order.
///
/// `HostPath` resolves the `.` and `..`, keeping the separators `path`
/// arrived with — rebuilding from `Path::components()` re-spells them all as
/// the *client's* `MAIN_SEPARATOR`, which on a Windows client turned a remote
/// host's `/home/user/x` into `\home\user\x`, one relative file name as far
/// as that host is concerned.
///
/// Then, for a *local* Windows path only, the result is handed back in std's
/// spelling. This looks redundant and is not: the file tree stores paths as
/// `canonicalize` spells them, and callers compare and **hash** what comes
/// back byte for byte (`FileExplorerDecoration::path` is a cache key), so a
/// plugin's `D:/a/b` — `editor.pathJoin` emits forward slashes — has to come
/// back as `D:\a\b` or every lookup misses. `HostPath` preserves the bytes it
/// is given by design, which is right for a remote path and not enough for
/// this one.
///
/// Doing it in this order is what makes both true. The `#[cfg(windows)]`
/// branch used to come *first* and return early, which re-spelled the path
/// but skipped the normalization: `Path::components()` drops `.` and does not
/// resolve `..`, the one thing this function exists for.
pub(crate) fn normalize_path(style: PathStyle, path: &Path) -> PathBuf {
    let normalized = path.as_host().under(style).normalize().into_path_buf();
    #[cfg(windows)]
    if style == PathStyle::Windows
        && matches!(
            normalized.components().next(),
            Some(std::path::Component::Prefix(_))
        )
    {
        return normalized.components().collect();
    }
    normalized
}

/// Resolve what the user typed into a prompt — a file to open, a project to
/// switch to, a name to save as — against the working directory, asking the
/// filesystem the path will be handed to.
///
/// It owns all three questions, which the prompt handlers used to ask of the
/// client in four duplicated copies: where `~` is, whether the path is
/// already rooted, and how to join it onto the working directory. The
/// filesystem answers the first two
/// ([`FileSystem::resolve_typed_path`](crate::model::filesystem::FileSystem::resolve_typed_path));
/// `normalize_path` resolves `.` and `..` without re-spelling separators.
pub(crate) fn resolve_prompt_path(
    fs: &dyn crate::model::filesystem::FileSystem,
    working_dir: &Path,
    typed: &str,
) -> PathBuf {
    let resolved = fs.resolve_typed_path(working_dir.as_host(), typed);
    normalize_path(fs.style(), &resolved.into_path_buf())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::filesystem::StdFileSystem;
    use std::fs;
    use tempfile::TempDir;

    /// The local filesystem, which is the one these fixtures build their
    /// temp trees on — so it is the one that should be answering for them.
    const LOCAL: &StdFileSystem = &StdFileSystem;

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

        let normalized = normalize_explorer_plugin_path(LOCAL, &plugin_style, &root);
        assert_eq!(normalized, file);
        assert!(explorer_path_under_root(LOCAL, &plugin_style, &root));
    }

    #[test]
    fn normalize_path_resolves_dot_segments() {
        let path = Path::new("/foo/./bar/../baz");
        assert_eq!(
            normalize_path(PathStyle::host(), path),
            PathBuf::from("/foo/baz")
        );
    }

    /// An admitted path comes back in the **root's** spelling, not the
    /// caller's. On Windows that is the whole point: a plugin's `//?/D:/a`
    /// has to become the tree's `\\?\D:\a`, since `std` parses the first as
    /// `UNC("?", "D:")` and the second as `VerbatimDisk`, and a tree node
    /// keyed by one is not found by the other. The stand-in here is a
    /// redundant separator and a `.`, which normalization drops.
    #[test]
    fn an_admitted_path_comes_back_in_the_roots_spelling() {
        let temp = TempDir::new().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let src = root.join("src");
        fs::create_dir_all(&src).unwrap();

        let same_path_spelled_oddly =
            PathBuf::from(format!("{}//src/./x.rs", root.to_string_lossy()));
        assert_eq!(
            ExplorerRoot::new(LOCAL, &root).admit(&same_path_spelled_oddly),
            Some(src.join("x.rs"))
        );
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
            let path =
                normalize_explorer_plugin_path(LOCAL, &src.join(format!("gen_{i}.rs")), &root);
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

        let explorer_root = ExplorerRoot::new(LOCAL, &root);
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

        let explorer_root = ExplorerRoot::new(LOCAL, &root);
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

        let explorer_root = ExplorerRoot::new(LOCAL, &root);
        for path in [inside.as_path(), outside.as_path(), root.as_path()] {
            let admitted = explorer_root.admit(path);
            assert_eq!(
                admitted.is_some(),
                explorer_path_under_root(LOCAL, path, &root),
                "admit must gate exactly as explorer_path_under_root did for {path:?}"
            );
            if let Some(admitted) = admitted {
                assert_eq!(
                    admitted,
                    normalize_explorer_plugin_path(LOCAL, path, &root),
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
        let admitted = normalize_explorer_plugin_path(LOCAL, &off_root, &root);
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
            let normalized = normalize_explorer_plugin_path(LOCAL, &alias.join("b.rs"), &root);
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
