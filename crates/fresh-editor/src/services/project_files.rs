//! The project's files: one definition shared by Quick Open, `grepProject`
//! and the streaming project search behind `beginSearch`.
//!
//! In a git work tree that is what `git ls-files --cached --others
//! --exclude-standard` lists — tracked files plus untracked ones `.gitignore`
//! does not exclude — and, walked, the contents of the submodules and nested
//! repositories git lists only by name. Elsewhere (or when git cannot run) it
//! is a walk of the root that never enters [`IGNORED_DIRS`]. Either way, files
//! `file_explorer.custom_ignore_patterns` match are left out.
//!
//! Both sources go through the authority's [`ProcessSpawner`] and
//! [`FileSystem`], so a remote session lists the remote project.
//!
//! A file whose path is not UTF-8 is left out: every consumer hands paths on
//! as strings (to plugins, to the picker), and the lossy string names a
//! different file — opening it shows an empty buffer, and a project replace
//! would write a new file under the mangled name.

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::model::filesystem::{EntryType, FileSystem, WalkOptions};
use crate::services::remote::ProcessSpawner;
use crate::view::file_tree::ignore::CustomIgnore;

/// Directory names a project walk never enters.
pub const IGNORED_DIRS: &[&str] = &[
    ".git",
    "node_modules",
    "target",
    "__pycache__",
    ".hg",
    ".svn",
    ".DS_Store",
];

/// [`IGNORED_DIRS`] plus the directories `custom_ignore` prunes outright.
pub fn skip_dirs(custom_ignore: &CustomIgnore) -> Vec<&str> {
    IGNORED_DIRS
        .iter()
        .copied()
        .chain(custom_ignore.prune_dirs().iter().map(String::as_str))
        .collect()
}

/// Whether `custom_ignore` leaves out the file at `rel` (relative to the
/// project root, `/`-separated).
fn is_custom_ignored(custom_ignore: &CustomIgnore, rel: &str) -> bool {
    custom_ignore.matches(Path::new(rel), false)
}

/// Which files a listing reports.
#[derive(Clone, Copy)]
pub struct Listing<'a> {
    /// Report hidden (dot-named) files that git does not track, and enter
    /// hidden directories when walking. Tracked dotfiles are always listed.
    pub include_hidden: bool,
    /// In a git work tree, also list what `.gitignore` excludes.
    pub include_gitignored: bool,
    /// Files these patterns match are left out.
    pub custom_ignore: &'a CustomIgnore,
}

/// The project files under `root` that git knows about, relative to `root`
/// and `/`-separated: at most `limit` of them, minus what `custom_ignore`
/// matches. `None` when `root` is not in a git work tree or git cannot run
/// there.
///
/// Must be called from the runtime's blocking pool: running there is what
/// establishes the runtime is alive for the duration, which a
/// `Handle::current` taken anywhere else cannot.
pub fn git_files_blocking(
    spawner: &Arc<dyn ProcessSpawner>,
    fs: &dyn FileSystem,
    root: &Path,
    listing: Listing<'_>,
    cancel: &AtomicBool,
    limit: usize,
) -> Option<Vec<String>> {
    // Running on the runtime's own blocking pool, so the runtime provably
    // exists for as long as this closure does — the liveness a `Handle` cannot
    // carry on its own is established by where we are, not by the handle.
    #[allow(clippy::disallowed_types)]
    let handle = tokio::runtime::Handle::try_current().ok()?;
    git_files_with_handle(spawner, &handle, fs, root, listing, cancel, limit)
}

/// [`git_files_blocking`] on an explicit `handle`, which must belong to a
/// runtime the caller has already established is alive — either by holding a
/// `LiveRuntime` or by running on that runtime's blocking pool. A `Handle`
/// carries no such guarantee on its own, which is why nothing stores one; see
/// `services::runtime::LiveRuntime`.
#[allow(clippy::disallowed_types)] // liveness established by the caller, above
pub fn git_files_with_handle(
    spawner: &Arc<dyn ProcessSpawner>,
    handle: &tokio::runtime::Handle,
    fs: &dyn FileSystem,
    root: &Path,
    listing: Listing<'_>,
    cancel: &AtomicBool,
    limit: usize,
) -> Option<Vec<String>> {
    let mut files = Vec::new();
    let walked = for_each_git_file(spawner, handle, fs, root, listing, cancel, &mut |_, rel| {
        files.push(rel.to_string());
        files.len() < limit
    })?;
    // A nested repository that can't be walked leaves out its own files only.
    if let Err(e) = walked {
        tracing::debug!(
            "listing nested repositories under {}: {}",
            root.display(),
            e
        );
    }
    Some(files)
}

/// Call `on_file(path, rel)` for each file git lists under `root` (see the
/// module docs), entering submodules and nested repositories with a walk.
/// `None` when `root` is not in a git work tree or git cannot run there;
/// otherwise the result of the walks, which stop early on `cancel` or when
/// `on_file` returns `false`.
#[allow(clippy::disallowed_types)] // liveness established by the caller, above
fn for_each_git_file(
    spawner: &Arc<dyn ProcessSpawner>,
    handle: &tokio::runtime::Handle,
    fs: &dyn FileSystem,
    root: &Path,
    listing: Listing<'_>,
    cancel: &AtomicBool,
    on_file: &mut dyn FnMut(&Path, &str) -> bool,
) -> Option<std::io::Result<()>> {
    #[allow(clippy::disallowed_methods)] // liveness established by the caller, above
    let result = handle
        .block_on(spawner.spawn_raw(
            "git".to_string(),
            git_ls_files_args(listing.include_gitignored, listing.include_hidden),
            Some(root.to_string_lossy().into_owned()),
        ))
        .ok()?;
    if result.exit_code != 0 {
        return None;
    }

    let LsFiles {
        files,
        nested_repos,
    } = parse_ls_files(&result.stdout, listing.custom_ignore);
    for rel in files {
        if cancel.load(Ordering::Relaxed) || !on_file(&join_rel(root, &rel), &rel) {
            return Some(Ok(()));
        }
    }
    let skip = skip_dirs(listing.custom_ignore);
    for dir in nested_repos {
        // The walk of the root would not have gone in either.
        let left_out = dir
            .split('/')
            .any(|name| skip.contains(&name) || (!listing.include_hidden && name.starts_with('.')))
            || listing.custom_ignore.matches(Path::new(&dir), true);
        if left_out {
            continue;
        }
        let mut go_on = true;
        let walked = walk_files_under(
            fs,
            root,
            &dir,
            listing.include_hidden,
            listing.custom_ignore,
            cancel,
            &mut |path, rel| {
                go_on = on_file(path, rel);
                go_on
            },
        );
        if walked.is_err() || !go_on || cancel.load(Ordering::Relaxed) {
            return Some(walked);
        }
    }
    Some(Ok(()))
}

/// `root` joined with the `/`-separated `rel`, a component at a time so the
/// path uses the platform's separator, as a walk's would.
fn join_rel(root: &Path, rel: &str) -> PathBuf {
    rel.split('/').fold(root.to_path_buf(), |p, c| p.join(c))
}

/// Arguments for the `git ls-files` that lists a repo's files: tracked plus
/// untracked, leaving out what `.gitignore` ignores unless
/// `include_gitignored`. Without `--exclude-standard`, `--others` lists
/// ignored files too; the `IGNORED_DIRS` excludes then keep dependency and
/// build trees out as the directory walk does, and unless `include_hidden`
/// so does `.*`, which would otherwise bring in every `.venv/` and
/// `.mypy_cache/` (`--exclude` only applies to untracked files, so a tracked
/// file under such a directory stays). Git never lists `.git/` itself.
///
/// `-z`: NUL-terminated and unquoted, so names with spaces or non-ASCII
/// characters come back verbatim rather than `core.quotePath`-escaped. `-s`:
/// each tracked entry carries its mode, which is what tells a submodule
/// (`160000`) from a file.
fn git_ls_files_args(include_gitignored: bool, include_hidden: bool) -> Vec<String> {
    let mut args = vec![
        "ls-files".to_string(),
        "-z".to_string(),
        "-s".to_string(),
        "--cached".to_string(),
        "--others".to_string(),
    ];
    if include_gitignored {
        args.extend(IGNORED_DIRS.iter().map(|dir| format!("--exclude={dir}")));
        if !include_hidden {
            args.push("--exclude=.*".to_string());
        }
    } else {
        args.push("--exclude-standard".to_string());
    }
    args
}

/// `git ls-files -z -s --cached --others` output, taken apart.
#[derive(Debug, PartialEq)]
struct LsFiles {
    /// Each file, relative to the root and `/`-separated.
    files: Vec<String>,
    /// Directories git lists but does not look into: submodules and
    /// untracked nested repositories, without the trailing `/`.
    nested_repos: Vec<String>,
}

/// Split `git ls-files -z -s` output on NUL. A tracked entry reads
/// `<mode> <object> <stage>\t<path>` and an untracked one is the bare path
/// (`<path>/` for a nested repository). Entries under `.git/`, what
/// `custom_ignore` matches, names that are not UTF-8 (see the module docs)
/// and the extra stages of an unmerged path are dropped.
fn parse_ls_files(stdout: &[u8], custom_ignore: &CustomIgnore) -> LsFiles {
    let mut out = LsFiles {
        files: Vec::new(),
        nested_repos: Vec::new(),
    };
    let mut last: Option<&[u8]> = None;
    for entry in stdout.split(|&b| b == 0).filter(|e| !e.is_empty()) {
        let (mode, path) = split_stage_entry(entry).unwrap_or((None, entry));
        if last == Some(path) {
            continue; // another stage of an unmerged path
        }
        last = Some(path);
        let is_dir = mode == Some(b"160000".as_slice()) || path.ends_with(b"/");
        let path = path.strip_suffix(b"/").unwrap_or(path);
        // Not UTF-8: left out (see the module docs).
        let Ok(rel) = std::str::from_utf8(path) else {
            continue;
        };
        if rel == ".git" || rel.starts_with(".git/") {
            continue;
        }
        if is_dir {
            out.nested_repos.push(rel.to_string());
        } else if !is_custom_ignored(custom_ignore, rel) {
            out.files.push(rel.to_string());
        }
    }
    out
}

/// `(mode, path)` from a `-s` entry, or `None` for an untracked one.
fn split_stage_entry(entry: &[u8]) -> Option<(Option<&[u8]>, &[u8])> {
    let tab = entry.iter().position(|&b| b == b'\t')?;
    let mut fields = entry[..tab].split(|&b| b == b' ');
    let mode = fields.next()?;
    let object = fields.next()?;
    let stage = fields.next()?;
    let well_formed = mode.len() == 6
        && mode.iter().all(u8::is_ascii_digit)
        && object.len() >= 40
        && object.iter().all(u8::is_ascii_hexdigit)
        && stage.len() == 1
        && fields.next().is_none();
    well_formed.then(|| (Some(mode), &entry[tab + 1..]))
}

/// Walk `root` for project files, calling `on_file(path, rel)` for each one
/// [`IGNORED_DIRS`] and `custom_ignore` leave in; `rel` is relative to `root`
/// and `/`-separated. `on_file` returns `false` to stop. Hidden entries are
/// reported only with `include_hidden`.
pub fn walk_files(
    fs: &dyn FileSystem,
    root: &Path,
    include_hidden: bool,
    custom_ignore: &CustomIgnore,
    cancel: &AtomicBool,
    on_file: &mut dyn FnMut(&Path, &str) -> bool,
) -> std::io::Result<()> {
    walk_files_under(fs, root, "", include_hidden, custom_ignore, cancel, on_file)
}

/// [`walk_files`] of the directory `sub` (relative to `root`, `/`-separated,
/// `""` for `root` itself), reporting `rel` relative to `root`.
fn walk_files_under(
    fs: &dyn FileSystem,
    root: &Path,
    sub: &str,
    include_hidden: bool,
    custom_ignore: &CustomIgnore,
    cancel: &AtomicBool,
    on_file: &mut dyn FnMut(&Path, &str) -> bool,
) -> std::io::Result<()> {
    let skip_dirs = skip_dirs(custom_ignore);
    let opts = WalkOptions {
        skip_dirs: &skip_dirs,
        include_hidden,
        max_entries: usize::MAX,
        ..WalkOptions::default()
    };
    let start = sub
        .split('/')
        .filter(|c| !c.is_empty())
        .fold(root.to_path_buf(), |p, c| p.join(c));
    fs.walk(&start, &opts, cancel, &mut |entry| {
        // A name that is not UTF-8 has only a lossy `rel` (see the module docs).
        if entry.entry_type != EntryType::File || entry.path.to_str().is_none() {
            return true;
        }
        let rel = if sub.is_empty() {
            std::borrow::Cow::Borrowed(entry.rel)
        } else {
            std::borrow::Cow::Owned(format!("{sub}/{}", entry.rel))
        };
        if is_custom_ignored(custom_ignore, &rel) {
            return true;
        }
        on_file(entry.path, &rel)
    })
}

/// Call `on_file(path, rel)` for every project file under `root`, hidden and
/// gitignored files left out: the git listing when `root` is in a git work
/// tree, otherwise a [`walk_files`]. `on_file` returns `false` to stop, and
/// `cancel` stops either source between files.
///
/// Must be called from the runtime's blocking pool (see
/// [`git_files_blocking`]).
pub fn for_each_file_blocking(
    spawner: &Arc<dyn ProcessSpawner>,
    fs: &dyn FileSystem,
    root: &Path,
    custom_ignore: &CustomIgnore,
    cancel: &AtomicBool,
    on_file: &mut dyn FnMut(&Path, &str) -> bool,
) -> std::io::Result<()> {
    if cancel.load(Ordering::Relaxed) {
        return Ok(());
    }
    let listing = Listing {
        include_hidden: false,
        include_gitignored: false,
        custom_ignore,
    };
    #[allow(clippy::disallowed_types)] // on the blocking pool, as documented
    let git = tokio::runtime::Handle::try_current()
        .ok()
        .and_then(|handle| for_each_git_file(spawner, &handle, fs, root, listing, cancel, on_file));
    match git {
        Some(result) => result,
        None => walk_files(fs, root, false, custom_ignore, cancel, on_file),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::filesystem::StdFileSystem;
    use crate::services::remote::{RawSpawnResult, SpawnError};
    use crate::services::runtime::LiveRuntime;

    /// A spawner standing in for `git`: answers every command with `stdout`
    /// and `exit_code`, and records the arguments it was asked to run.
    struct FakeGit {
        stdout: Vec<u8>,
        exit_code: i32,
        calls: std::sync::Mutex<Vec<(String, Vec<String>, Option<String>)>>,
    }

    impl FakeGit {
        fn new(stdout: &[u8], exit_code: i32) -> Arc<Self> {
            Arc::new(Self {
                stdout: stdout.to_vec(),
                exit_code,
                calls: std::sync::Mutex::new(Vec::new()),
            })
        }
    }

    #[async_trait::async_trait]
    impl ProcessSpawner for FakeGit {
        async fn spawn_raw(
            &self,
            command: String,
            args: Vec<String>,
            cwd: Option<String>,
        ) -> Result<RawSpawnResult, SpawnError> {
            self.calls.lock().unwrap().push((command, args, cwd));
            Ok(RawSpawnResult {
                stdout: self.stdout.clone(),
                stderr: String::new(),
                exit_code: self.exit_code,
            })
        }
    }

    fn patterns(lines: &[&str]) -> Vec<String> {
        lines.iter().map(|s| s.to_string()).collect()
    }

    /// Create `rel` (and its parents) under `root`.
    fn touch(root: &Path, rel: &str) {
        let path = root.join(rel);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, "x\n").unwrap();
    }

    /// Every file the walk reports, as sorted `rel` paths.
    fn walked(root: &Path, include_hidden: bool, custom_ignore: &CustomIgnore) -> Vec<String> {
        let mut files = Vec::new();
        walk_files(
            &StdFileSystem,
            root,
            include_hidden,
            custom_ignore,
            &AtomicBool::new(false),
            &mut |path, rel| {
                assert_eq!(path, root.join(rel), "path must be root joined with rel");
                files.push(rel.to_string());
                true
            },
        )
        .unwrap();
        files.sort();
        files
    }

    /// Run `for_each_file_blocking` on a runtime's blocking pool, as the
    /// searches do, and collect sorted `rel` paths.
    fn listed(
        spawner: Arc<dyn ProcessSpawner>,
        root: &Path,
        custom_ignore: CustomIgnore,
        cancel: Arc<AtomicBool>,
    ) -> Vec<String> {
        let runtime = LiveRuntime::multi_thread("project-files-test", 1).unwrap();
        let root = root.to_path_buf();
        let (tx, rx) = std::sync::mpsc::channel();
        runtime.spawn_blocking(move || {
            let mut files = Vec::new();
            for_each_file_blocking(
                &spawner,
                &StdFileSystem,
                &root,
                &custom_ignore,
                &cancel,
                &mut |path, rel| {
                    assert_eq!(path, root.join(rel), "path must be root joined with rel");
                    files.push(rel.to_string());
                    true
                },
            )
            .unwrap();
            tx.send(files).unwrap();
        });
        let mut files = rx.recv().unwrap();
        files.sort();
        files
    }

    /// The walk never enters `IGNORED_DIRS`, and leaves out what the custom
    /// patterns match, whether a whole directory or a glob over files.
    #[test]
    fn walk_skips_ignored_dirs_and_custom_patterns() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        for rel in [
            "src/main.rs",
            "src/gen/out.rs",
            "node_modules/pkg/index.js",
            "target/debug/app",
            "build/artifact.bin",
            "notes.log",
            "docs/readme.md",
        ] {
            touch(root, rel);
        }
        let custom = CustomIgnore::new(root, &patterns(&["build/", "*.log", "src/gen/"]));

        assert_eq!(
            walked(root, false, &custom),
            vec!["docs/readme.md", "src/main.rs"],
        );
        assert_eq!(
            walked(root, false, &CustomIgnore::default()),
            vec![
                "build/artifact.bin",
                "docs/readme.md",
                "notes.log",
                "src/gen/out.rs",
                "src/main.rs",
            ],
            "without custom patterns only IGNORED_DIRS are left out",
        );
    }

    /// Hidden entries are walked only on request, and `.git` stays out even
    /// then.
    #[test]
    fn walk_reports_hidden_files_only_when_asked() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        for rel in [".env", ".github/ci.yml", ".git/config", "a.txt"] {
            touch(root, rel);
        }
        let none = CustomIgnore::default();

        assert_eq!(walked(root, false, &none), vec!["a.txt"]);
        assert_eq!(
            walked(root, true, &none),
            vec![".env", ".github/ci.yml", "a.txt"],
        );
    }

    /// A plain directory-name pattern is pruned from the walk instead of being
    /// entered and filtered; anything with a slash or glob is filtered only.
    #[test]
    fn skip_dirs_adds_prunable_custom_patterns() {
        let root = Path::new("/project");
        let custom = CustomIgnore::new(root, &patterns(&["dist/", "vendor", "*.o", "a/b/"]));
        let skip = skip_dirs(&custom);

        for name in IGNORED_DIRS {
            assert!(skip.contains(name), "{name} must always be skipped");
        }
        assert!(skip.contains(&"dist"));
        assert!(skip.contains(&"vendor"));
        assert_eq!(skip.len(), IGNORED_DIRS.len() + 2, "got {skip:?}");
    }

    /// The `rel` of each file `parse_ls_files` keeps.
    fn rels(listed: &LsFiles) -> Vec<&str> {
        listed.files.iter().map(String::as_str).collect()
    }

    /// `git ls-files -z` output is split on NUL, so names with spaces,
    /// newlines or non-ASCII characters survive; `.git/` entries,
    /// custom-ignored files and names that are not UTF-8 are dropped.
    #[test]
    fn ls_files_output_is_split_on_nul_and_filtered() {
        let custom = CustomIgnore::new(Path::new("/p"), &patterns(&["*.log", "gen/"]));
        let stdout = b"a b.txt\0a\xc3\xb1o.rs\0line\nbreak.md\0.git/HEAD\0x.log\0gen/out.rs\0.github/ci.yml\0caf\xe9.txt\0";

        assert_eq!(
            rels(&parse_ls_files(stdout, &custom)),
            vec!["a b.txt", "año.rs", "line\nbreak.md", ".github/ci.yml"],
        );
    }

    /// With `-s`, a tracked entry carries its mode: a submodule (`160000`)
    /// and an untracked nested repository (`dir/`) are directories to walk,
    /// not files, and an unmerged path's three stages are one file.
    #[test]
    fn ls_files_stage_output_separates_nested_repos() {
        let hash = "0123456789abcdef0123456789abcdef01234567";
        let stdout = format!(
            "100644 {hash} 0\ta.txt\0160000 {hash} 0\tvendor/sub\0\
             100644 {hash} 1\tc.rs\0100644 {hash} 2\tc.rs\0100644 {hash} 3\tc.rs\0\
             nested/\0new.txt\0100755 {hash} 0\tbin/run\0"
        );

        let listed = parse_ls_files(stdout.as_bytes(), &CustomIgnore::default());

        assert_eq!(rels(&listed), vec!["a.txt", "c.rs", "new.txt", "bin/run"]);
        assert_eq!(listed.nested_repos, vec!["vendor/sub", "nested"]);
    }

    /// Showing gitignored files drops `--exclude-standard` (so `--others`
    /// lists ignored files too) but still keeps the walk's heavy directories
    /// out.
    #[test]
    fn test_git_ls_files_args_follow_include_gitignored() {
        for include_hidden in [false, true] {
            let args = git_ls_files_args(false, include_hidden);
            assert_eq!(
                args,
                vec![
                    "ls-files",
                    "-z",
                    "-s",
                    "--cached",
                    "--others",
                    "--exclude-standard"
                ]
            );
        }

        let args = git_ls_files_args(true, true);
        assert_eq!(
            &args[..5],
            &["ls-files", "-z", "-s", "--cached", "--others"]
        );
        assert!(!args.iter().any(|a| a == "--exclude-standard"), "{args:?}");
        for dir in IGNORED_DIRS {
            assert!(args.contains(&format!("--exclude={dir}")), "{args:?}");
        }
        assert!(!args.iter().any(|a| a == "--exclude=.*"), "{args:?}");

        // Gitignored but not hidden: untracked dot-entries (`.venv/`, …) stay
        // out, as the explorer hides them.
        let args = git_ls_files_args(true, false);
        assert!(args.iter().any(|a| a == "--exclude=.*"), "{args:?}");
    }

    /// In a git work tree the project's files are what `git ls-files` lists
    /// — tracked dotfiles included, gitignored files not — run in the root
    /// through the given spawner, minus the custom patterns.
    #[test]
    fn git_listing_is_used_when_git_answers() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        // On disk but not listed: stands in for a gitignored file, which the
        // walk would have reported.
        touch(root, "ignored.txt");
        let git = FakeGit::new(b"src/lib.rs\0.github/ci.yml\0vendor/dep.rs\0", 0);
        let custom = CustomIgnore::new(root, &patterns(&["vendor/"]));

        let files = listed(git.clone(), root, custom, Arc::default());

        assert_eq!(files, vec![".github/ci.yml", "src/lib.rs"]);
        let calls = git.calls.lock().unwrap();
        assert_eq!(calls.len(), 1);
        let (command, args, cwd) = &calls[0];
        assert_eq!(command, "git");
        assert_eq!(
            args,
            &[
                "ls-files",
                "-z",
                "-s",
                "--cached",
                "--others",
                "--exclude-standard"
            ]
        );
        assert_eq!(cwd.as_deref(), Some(root.to_string_lossy().as_ref()));
    }

    /// Outside a git work tree `git ls-files` fails, and the walk takes over
    /// with the same exclusions.
    #[test]
    fn walk_is_used_when_git_fails() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        for rel in ["a.txt", "node_modules/x.js", "out/b.txt", ".hidden"] {
            touch(root, rel);
        }
        let git = FakeGit::new(b"", 128);
        let custom = CustomIgnore::new(root, &patterns(&["out/"]));

        assert_eq!(listed(git, root, custom, Arc::default()), vec!["a.txt"]);
    }

    /// A cancelled listing reports nothing, from either source.
    #[test]
    fn cancelled_listing_reports_nothing() {
        let dir = tempfile::tempdir().unwrap();
        touch(dir.path(), "a.txt");
        let cancel = Arc::new(AtomicBool::new(true));

        assert!(listed(
            FakeGit::new(b"a.txt\0", 0),
            dir.path(),
            CustomIgnore::default(),
            Arc::clone(&cancel),
        )
        .is_empty());
        assert!(listed(
            FakeGit::new(b"", 128),
            dir.path(),
            CustomIgnore::default(),
            cancel,
        )
        .is_empty());
    }

    /// Against a real repository: `.gitignore`d files are left out, untracked
    /// ones are listed, and so is a tracked dotfile. Skipped when `git` is not
    /// installed.
    #[test]
    fn real_git_repository_respects_gitignore() {
        use crate::services::env_provider::EnvProvider;
        use crate::services::remote::LocalProcessSpawner;
        use crate::services::workspace_trust::WorkspaceTrust;

        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        let git = |args: &[&str]| {
            std::process::Command::new("git")
                .args(args)
                .current_dir(root)
                .output()
                .map(|o| o.status.success())
                .unwrap_or(false)
        };
        if !git(&["init", "-q"]) {
            eprintln!("git not available; skipping");
            return;
        }
        std::fs::write(root.join(".gitignore"), "*.tmp\nbuild/\n").unwrap();
        for rel in [
            "tracked.rs",
            ".github/ci.yml",
            "untracked.rs",
            "scratch.tmp",
            "build/out.bin",
            "local/notes.md",
        ] {
            touch(root, rel);
        }
        assert!(git(&["add", "tracked.rs", ".github/ci.yml", ".gitignore"]));

        let spawner: Arc<dyn ProcessSpawner> = Arc::new(LocalProcessSpawner::new(
            Arc::new(EnvProvider::inactive()),
            Arc::new(WorkspaceTrust::permissive()),
        ));
        let custom = CustomIgnore::new(root, &patterns(&["local/"]));

        assert_eq!(
            listed(spawner, root, custom, Arc::default()),
            vec![".github/ci.yml", ".gitignore", "tracked.rs", "untracked.rs"],
        );
    }

    /// Git lists a submodule and an untracked nested repository by name only;
    /// their files are walked, with the same exclusions as the root's walk.
    /// Skipped when `git` is not installed.
    #[test]
    fn real_git_repository_enters_submodules_and_nested_repos() {
        use crate::services::env_provider::EnvProvider;
        use crate::services::remote::LocalProcessSpawner;
        use crate::services::workspace_trust::WorkspaceTrust;

        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        let git = |cwd: &Path, args: &[&str]| {
            std::process::Command::new("git")
                .args(args)
                .current_dir(cwd)
                .output()
                .map(|o| o.status.success())
                .unwrap_or(false)
        };
        if !git(root, &["init", "-q"]) {
            eprintln!("git not available; skipping");
            return;
        }
        for rel in [
            "a.txt",
            "vendor/sub/s.txt",
            "vendor/sub/node_modules/m.js",
            "vendor/sub/.hidden/h.txt",
            "nested/n.txt",
            "nested/out/skip.log",
        ] {
            touch(root, rel);
        }
        assert!(git(&root.join("nested"), &["init", "-q"]));
        assert!(git(root, &["add", "a.txt"]));
        // A submodule as the index records it: a gitlink entry.
        assert!(git(
            root,
            &[
                "update-index",
                "--add",
                "--cacheinfo",
                "160000,0123456789abcdef0123456789abcdef01234567,vendor/sub",
            ],
        ));

        let spawner: Arc<dyn ProcessSpawner> = Arc::new(LocalProcessSpawner::new(
            Arc::new(EnvProvider::inactive()),
            Arc::new(WorkspaceTrust::permissive()),
        ));
        let custom = CustomIgnore::new(root, &patterns(&["*.log"]));

        assert_eq!(
            listed(spawner, root, custom, Arc::default()),
            vec!["a.txt", "nested/n.txt", "vendor/sub/s.txt"],
        );
    }

    /// A name that is not UTF-8 is left out by the git listing and the walk
    /// alike: its lossy string would name a different file. Skipped where git
    /// or the filesystem won't take such a name.
    #[cfg(unix)]
    #[test]
    fn non_utf8_names_are_left_out() {
        use crate::services::env_provider::EnvProvider;
        use crate::services::remote::LocalProcessSpawner;
        use crate::services::workspace_trust::WorkspaceTrust;
        use std::os::unix::ffi::OsStrExt;

        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        touch(root, "ok.txt");
        let name = std::ffi::OsStr::from_bytes(b"caf\xe9.txt");
        if std::fs::write(root.join(name), "x\n").is_err() {
            eprintln!("non-UTF-8 names unsupported here; skipping");
            return;
        }
        assert_eq!(
            walked(root, false, &CustomIgnore::default()),
            vec!["ok.txt"]
        );

        if !std::process::Command::new("git")
            .args(["init", "-q"])
            .current_dir(root)
            .status()
            .is_ok_and(|s| s.success())
        {
            eprintln!("git not available; skipping");
            return;
        }
        let spawner: Arc<dyn ProcessSpawner> = Arc::new(LocalProcessSpawner::new(
            Arc::new(EnvProvider::inactive()),
            Arc::new(WorkspaceTrust::permissive()),
        ));
        assert_eq!(
            listed(spawner, root, CustomIgnore::default(), Arc::default()),
            vec!["ok.txt"]
        );
    }
}
