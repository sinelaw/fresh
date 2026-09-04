//! Git test helper - creates hermetic git repositories for testing

use super::harness::{copy_plugin, copy_plugin_lib};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

/// Build a `git` command for a test repo that is fully isolated from the host
/// machine's git configuration.
///
/// Why this matters: without isolation, every test repo inherits the global /
/// system git config of whatever machine runs the suite (a developer's box, a
/// Linux CI runner, a Windows CI runner). That config can enable signing
/// programs, background `gc`/`maintenance`, `init.templateDir` hooks,
/// `fsmonitor`, etc. — any of which can make a commit fail in
/// environment-specific ways. The `git_log_commit_list_scrolls_with_wheel`
/// flake ("fatal: unable to read <hash>" during commit) was a symptom of this
/// leakage.
///
/// We therefore:
///   * ignore system config (`GIT_CONFIG_NOSYSTEM`) and point global config at
///     a nonexistent file (`GIT_CONFIG_GLOBAL`), which git treats as empty —
///     cross-platform, unlike `/dev/null`;
///   * pin identity and disable signing via `-c` so it applies to *every*
///     subcommand including `init`;
///   * disable automatic `gc`/`maintenance` so loose objects are never
///     repacked mid-operation;
///   * force durable loose-object writes (`core.fsync`) so an object written by
///     one commit is guaranteed readable by the next, even on CI filesystems
///     with weaker write-then-read coherency.
///
/// We deliberately do NOT pin `core.autocrlf` / `core.eol`: line-ending
/// behavior is left at git's platform default so the editor under test is
/// exercised the way a real user's git would behave. Tests that read files
/// back must therefore be line-ending-agnostic rather than assuming LF.
pub fn git_command(path: &Path) -> Command {
    let mut cmd = Command::new("git");
    cmd.current_dir(path)
        .env("GIT_CONFIG_NOSYSTEM", "1")
        .env("GIT_CONFIG_GLOBAL", path.join("nonexistent-gitconfig"))
        .env("GIT_TERMINAL_PROMPT", "0")
        .args(["-c", "user.name=Test User"])
        .args(["-c", "user.email=test@example.com"])
        .args(["-c", "commit.gpgsign=false"])
        .args(["-c", "tag.gpgsign=false"])
        .args(["-c", "gc.auto=0"])
        .args(["-c", "maintenance.auto=false"])
        .args(["-c", "core.fsync=loose-object,index,reference"])
        .args(["-c", "core.fsyncMethod=fsync"]);
    cmd
}

/// Run a repository-mutating git command, tolerating transient
/// `.git/index.lock` contention.
///
/// Why this is needed: the editor under test polls `git status --porcelain`
/// in the same repo (the `git_explorer` / `git_gutter` plugins refresh file
/// decorations on a timer). `git status` refreshes the on-disk index and, to
/// do so, briefly takes `.git/index.lock`. When a test's *own* external
/// `git add` / `git commit` — simulating a user running git in another
/// terminal — lands inside that window, git aborts with:
///
///   fatal: Unable to create '.../.git/index.lock': File exists.
///   Another git process seems to be running in this repository ...
///
/// That is not a real error: it is transient lock contention between two
/// legitimate git processes. A user would simply re-run the command, and so
/// do we. On *that specific* failure we wait (semantically, by watching the
/// lock file) for the competing process to release the lock, then retry — no
/// wall-clock timeout, matching CONTRIBUTING's "wait indefinitely / semantic
/// waiting" rule. Any *other* failure panics immediately, so a genuine git
/// error is never masked (narrow recovery path).
fn run_git_mutation(path: &Path, args: &[&str], what: &str) {
    let lock_path = path.join(".git").join("index.lock");
    loop {
        let output = git_command(path)
            .args(args)
            .output()
            .unwrap_or_else(|e| panic!("Failed to run git {what}: {e}"));

        if output.status.success() {
            return;
        }

        let stderr = String::from_utf8_lossy(&output.stderr);
        // Narrow: only the index.lock contention error is retryable.
        let lock_contention = stderr.contains("index.lock")
            && (stderr.contains("File exists") || stderr.contains("Another git process"));
        if !lock_contention {
            panic!("git {what} failed: {stderr}");
        }

        // Semantic wait: spin (yielding) until the competing git process
        // releases the lock, then retry. The poll-driven `git status` is
        // short-lived, so this resolves almost immediately.
        while lock_path.exists() {
            std::thread::yield_now();
        }
    }
}

/// A hermetic git repository for testing
pub struct GitTestRepo {
    /// Temporary directory containing the git repository
    _temp_dir: TempDir,
    /// Path to the git repository root
    pub path: PathBuf,
}

impl GitTestRepo {
    /// Create a new git test repository with test files
    pub fn new() -> Self {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        // Deliberately *not* canonicalized. On macOS a tempdir is
        // `/var/folders/...`, a symlink to `/private/var/...`, and the editor
        // stores the resolved form on the buffer while `repo.path` keeps the
        // unresolved one — which is a real hazard for anything comparing the
        // two, and why `tests/e2e/code_tour_dock.rs` canonicalizes its own
        // fixture. Resolving it *here* is not the fix, though: on Windows
        // `fs::canonicalize` returns an extended-length `\\?\D:\...` path,
        // which git and the plugins that shell out to it do not accept, and
        // that broke `plugins::package_manager` (which uses a repo path as a
        // git remote). Per-fixture canonicalization where a test actually
        // needs it stays correct; doing it for every git-backed test at once
        // does not.
        let path = temp_dir.path().to_path_buf();

        // Initialize git repository. The test's own git invocations are
        // isolated by `git_command` so the host's signing program / background
        // gc can't break commits, but we deliberately leave line-ending config
        // (core.autocrlf / core.eol) at git's defaults — the editor under test
        // must behave correctly for a normal user's git, whatever their
        // platform default is, rather than relying on the test pinning LF.
        let output = git_command(&path)
            .arg("init")
            .output()
            .expect("Failed to initialize git repository");

        if !output.status.success() {
            panic!(
                "git init failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }

        GitTestRepo {
            _temp_dir: temp_dir,
            path,
        }
    }

    /// Create a file with content
    pub fn create_file(&self, relative_path: &str, content: &str) -> PathBuf {
        let file_path = self.path.join(relative_path);

        // Create parent directories if needed
        if let Some(parent) = file_path.parent() {
            fs::create_dir_all(parent).expect("Failed to create parent directories");
        }

        fs::write(&file_path, content).expect("Failed to write file");
        file_path
    }

    /// Add files to git staging area
    pub fn git_add(&self, paths: &[&str]) {
        for path in paths {
            run_git_mutation(&self.path, &["add", path], "add");
        }
    }

    /// Add all files to git
    pub fn git_add_all(&self) {
        run_git_mutation(&self.path, &["add", "."], "add .");
    }

    /// Commit staged changes
    pub fn git_commit(&self, message: &str) {
        run_git_mutation(&self.path, &["commit", "-m", message], "commit");
    }

    /// Set up a typical project structure for testing
    pub fn setup_typical_project(&self) {
        // Create source files with searchable content
        self.create_file(
            "src/main.rs",
            r#"fn main() {
    println!("Hello, world!");
    let config = load_config();
    start_server(config);
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("Starting server...");
}
"#,
        );

        self.create_file(
            "src/lib.rs",
            r#"pub struct Config {
    pub port: u16,
    pub host: String,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            port: 8080,
            host: "localhost".to_string(),
        }
    }
}

pub fn process_request(data: &str) -> String {
    format!("Processed: {}", data)
}
"#,
        );

        self.create_file(
            "src/utils.rs",
            r#"pub fn format_output(msg: &str) -> String {
    format!("[INFO] {}", msg)
}

pub fn validate_config(config: &Config) -> bool {
    config.port > 0 && !config.host.is_empty()
}
"#,
        );

        self.create_file(
            "tests/integration.rs",
            r#"#[test]
fn test_config_default() {
    let config = Config::default();
    assert_eq!(config.port, 8080);
}

#[test]
fn test_process_request() {
    let result = process_request("test");
    assert_eq!(result, "Processed: test");
}
"#,
        );

        self.create_file(
            "Cargo.toml",
            r#"[package]
name = "test-project"
version = "0.1.0"
edition = "2021"

[dependencies]
serde = "1.0"
tokio = { version = "1.0", features = ["full"] }
"#,
        );

        self.create_file(
            "README.md",
            r#"# Test Project

A sample project for testing.

## Features

- Configuration management
- Request processing
- Server functionality
"#,
        );

        // Add and commit all files
        self.git_add_all();
        self.git_commit("Initial commit");
    }

    /// Set up a project with many files for scrolling tests
    pub fn setup_many_files(&self, count: usize) {
        for i in 0..count {
            let dir_num = i / 10;
            let file_name = format!("dir{dir_num}/file{i}.txt");
            let content = format!("This is file number {i}\nSearchable content here\nLine 3");
            self.create_file(&file_name, &content);
        }

        self.git_add_all();
        self.git_commit("Add many files");
    }

    /// Change the process's current directory to this repository, until the
    /// returned [`DirGuard`] drops.
    ///
    /// The guard also holds [`cwd_lock`], so only one test at a time is inside
    /// a `change_to_repo_dir`. That is what makes "the directory to go back
    /// to" a meaningful thing to record: the caller used to snapshot
    /// `current_dir()` unsynchronised, and with ~130 tests doing this in
    /// parallel a test routinely captured a *sibling's* repository as its
    /// "original", then restored the process into it on the way out. The
    /// checkout root was never restored after that, and every later test in
    /// the process resolved relative paths against a stray temp directory —
    /// one that its owner's `TempDir` had by then deleted.
    #[must_use = "the process cwd reverts as soon as the guard is dropped"]
    pub fn change_to_repo_dir(&self) -> DirGuard {
        let lock = cwd_lock().lock().unwrap_or_else(|e| e.into_inner());
        // With the lock held this really is "the directory nobody else is
        // moving". It can still be missing if a *previous* run of the suite
        // left the process somewhere deleted, so keep the fallback.
        let original_dir = std::env::current_dir().unwrap_or_else(|_| std::env::temp_dir());
        std::env::set_current_dir(&self.path).expect("Failed to change directory");
        DirGuard {
            original_dir,
            _lock: lock,
        }
    }

    /// Set up git plugins by copying them from the project's plugins directory
    /// This is needed for testing git functionality which has been moved to TypeScript plugins
    pub fn setup_git_plugins(&self) {
        let plugins_dir = self.path.join("plugins");
        fs::create_dir_all(&plugins_dir).expect("Failed to create plugins directory");
        copy_plugin_lib(&plugins_dir);
        copy_plugin(&plugins_dir, "git_grep");
        copy_plugin(&plugins_dir, "git_find_file");
    }

    /// Set up git log plugin by copying it from the project's plugins directory
    pub fn setup_git_log_plugin(&self) {
        let plugins_dir = self.path.join("plugins");
        fs::create_dir_all(&plugins_dir).expect("Failed to create plugins directory");
        copy_plugin_lib(&plugins_dir);
        copy_plugin(&plugins_dir, "git_log");
    }

    /// Set up git blame plugin by copying it from the project's plugins directory
    pub fn setup_git_blame_plugin(&self) {
        let plugins_dir = self.path.join("plugins");
        fs::create_dir_all(&plugins_dir).expect("Failed to create plugins directory");
        copy_plugin_lib(&plugins_dir);
        copy_plugin(&plugins_dir, "git_blame");
    }

    /// Set up test view marker plugin for debugging view transforms
    pub fn setup_test_view_marker_plugin(&self) {
        // Create plugins directory in the test repo
        let plugins_dir = self.path.join("plugins");
        fs::create_dir_all(&plugins_dir).expect("Failed to create plugins directory");
        copy_plugin_lib(&plugins_dir);

        // Get the project root
        let project_root = std::env::var("CARGO_MANIFEST_DIR")
            .map(PathBuf::from)
            .expect("CARGO_MANIFEST_DIR not set");

        // Copy test_view_marker.ts plugin from tests/plugins/
        let src = project_root.join("tests/plugins/test_view_marker.ts");
        let dst = plugins_dir.join("test_view_marker.ts");
        fs::copy(&src, &dst)
            .unwrap_or_else(|e| panic!("Failed to copy test_view_marker.ts from {:?}: {}", src, e));
    }

    /// Set up git gutter plugin for line indicator tests
    pub fn setup_git_gutter_plugin(&self) {
        let plugins_dir = self.path.join("plugins");
        fs::create_dir_all(&plugins_dir).expect("Failed to create plugins directory");
        copy_plugin_lib(&plugins_dir);
        copy_plugin(&plugins_dir, "git_gutter");
    }

    /// Configure local stand-ins for the `difft` external diff tool and the
    /// `delta` pager. The fake difft intentionally emits side-by-side output
    /// instead of a unified diff, matching the configuration from issue #2721
    /// without requiring either tool to be installed on the test machine.
    #[cfg(unix)]
    pub fn setup_external_diff_and_pager(&self) {
        use std::os::unix::fs::PermissionsExt;

        let difft = self.create_file(
            "difft",
            "#!/bin/sh\nprintf '%s\\n' 'src/main.rs --- Rust' '1 fn main() | 1 fn main()'\n",
        );
        let delta = self.create_file("delta", "#!/bin/sh\ncat\n");

        for tool in [&difft, &delta] {
            fs::set_permissions(tool, fs::Permissions::from_mode(0o755))
                .expect("Failed to make fake diff tool executable");
        }

        for (key, value) in [
            ("diff.external", difft.to_string_lossy()),
            ("core.pager", delta.to_string_lossy()),
        ] {
            let output = git_command(&self.path)
                .args(["config", "--local", key, value.as_ref()])
                .output()
                .expect("Failed to configure fake diff tool");
            assert!(
                output.status.success(),
                "git config {key} failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
    }

    /// Set up live diff plugin for live-diff e2e tests
    pub fn setup_live_diff_plugin(&self) {
        let plugins_dir = self.path.join("plugins");
        fs::create_dir_all(&plugins_dir).expect("Failed to create plugins directory");
        copy_plugin_lib(&plugins_dir);
        copy_plugin(&plugins_dir, "live_diff");
    }

    /// Set up git explorer plugin for file explorer decorations
    pub fn setup_git_explorer_plugin(&self) {
        let plugins_dir = self.path.join("plugins");
        fs::create_dir_all(&plugins_dir).expect("Failed to create plugins directory");
        copy_plugin(&plugins_dir, "git_explorer");
        copy_plugin_lib(&plugins_dir);
    }

    /// Buffer-modified indicators are now computed natively during rendering.
    /// Creates an empty plugins directory to prevent embedded plugins (git_gutter, etc.)
    /// from auto-loading and interfering with tests that only test native indicators.
    pub fn setup_buffer_modified_plugin(&self) {
        let plugins_dir = self.path.join("plugins");
        if !plugins_dir.exists() {
            fs::create_dir_all(&plugins_dir).expect("Failed to create plugins directory");
        }
    }

    /// Set up gutter plugins (git gutter; buffer-modified is native)
    pub fn setup_gutter_plugins(&self) {
        self.setup_git_gutter_plugin();
    }

    /// Modify a file without staging or committing (working copy change)
    pub fn modify_file(&self, relative_path: &str, content: &str) {
        let file_path = self.path.join(relative_path);
        fs::write(&file_path, content).expect("Failed to modify file");
    }

    /// Stage a file's changes
    pub fn stage_file(&self, relative_path: &str) {
        self.git_add(&[relative_path]);
    }

    /// Reset a file's index and working-tree state to `refspec`, the way an
    /// external tool would (`git checkout <ref> -- <path>` in another
    /// terminal while the editor has the file open).
    pub fn git_checkout_file(&self, refspec: &str, relative_path: &str) {
        run_git_mutation(
            &self.path,
            &["checkout", refspec, "--", relative_path],
            "checkout",
        );
    }

    /// The file's current on-disk modification time.
    pub fn mtime(&self, relative_path: &str) -> std::time::SystemTime {
        fs::metadata(self.path.join(relative_path))
            .and_then(|m| m.modified())
            .expect("file should exist and expose an mtime")
    }

    /// Block until `relative_path`'s on-disk mtime is strictly newer than
    /// `floor`, rewriting its (unchanged) bytes until the filesystem's clock
    /// ticks past its own granularity.
    ///
    /// Auto-revert notices an external write by comparing mtimes, so a test
    /// that rewrites a file the editor already opened has to guarantee the
    /// new mtime is distinguishable from the recorded one. On a filesystem
    /// with 1 s mtime granularity (HFS+, some CI volumes) a fast test does
    /// the whole open-then-rewrite sequence inside a single tick, and the
    /// reload never fires.
    ///
    /// The obvious fix — sleep past the granularity — is a fixed timer, and
    /// the one this replaced was worse than that: it used `harness.sleep`,
    /// which advances *logical* time only and so waited no real time at all,
    /// leaving the hazard it was written to prevent fully live. Rewriting
    /// until the observed mtime actually moves is semantic waiting on the
    /// real condition (CONTRIBUTING.md Testing §3): it costs nothing on a
    /// nanosecond-resolution filesystem and exactly one granularity tick on
    /// a coarse one, and it cannot silently not-work.
    pub fn touch_until_mtime_after(&self, relative_path: &str, floor: std::time::SystemTime) {
        let full = self.path.join(relative_path);
        let content = fs::read(&full).expect("file should exist");
        loop {
            if self.mtime(relative_path) > floor {
                return;
            }
            // Same bytes back: this is a pure mtime bump, not a content edit,
            // so it can't perturb what the test is actually asserting on.
            fs::write(&full, &content).expect("failed to rewrite file for mtime bump");
            std::thread::yield_now();
        }
    }
}

/// Serializes every `change_to_repo_dir`, so the process cwd has exactly one
/// owner at a time.
///
/// The cwd is process-global and there is no per-test version of it, so a lock
/// is the only way these tests can share it. It is held only for the body of a
/// test that needs `git` to run *in* its repository; the rest of the suite
/// keeps running in parallel.
pub fn cwd_lock() -> &'static std::sync::Mutex<()> {
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    LOCK.get_or_init(|| std::sync::Mutex::new(()))
}

/// Restores the directory the process was in before
/// [`GitTestRepo::change_to_repo_dir`], and releases the cwd lock.
pub struct DirGuard {
    original_dir: PathBuf,
    _lock: std::sync::MutexGuard<'static, ()>,
}

impl Drop for DirGuard {
    fn drop(&mut self) {
        let _ = std::env::set_current_dir(&self.original_dir);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Concurrent `change_to_repo_dir` calls must leave the process where they
    /// found it.
    ///
    /// Without [`cwd_lock`] each thread snapshots `current_dir()` while its
    /// siblings are mid-`set_current_dir`, so a thread routinely records a
    /// *sibling's* repository as the directory to go back to and restores the
    /// process into it. Nothing puts the checkout back after that, and every
    /// later test in the binary resolves relative paths against a temp
    /// directory its owner has since deleted — which is how the visual
    /// regression captures started failing on an unwritable
    /// `docs/visual-regression/screenshots`.
    #[test]
    fn concurrent_repo_dir_changes_restore_the_process_directory() {
        // Read the cwd through the same lock the guards take, so this never
        // observes a *legitimately* moved directory: a sibling test elsewhere
        // in the binary may be inside its own `change_to_repo_dir` right now.
        let observe = || {
            let _lock = cwd_lock().lock().unwrap_or_else(|e| e.into_inner());
            std::env::current_dir()
        };
        let before = observe().expect("a valid cwd to start from");
        // Several rounds: one interleaving is enough to strand the process, but
        // whether it happens in any single round is up to the scheduler.
        for _ in 0..4 {
            let repos: Vec<GitTestRepo> = (0..3).map(|_| GitTestRepo::new()).collect();
            std::thread::scope(|scope| {
                for repo in &repos {
                    scope.spawn(move || {
                        let _guard = repo.change_to_repo_dir();
                        // Widen the window a racing sibling would snapshot in.
                        for _ in 0..64 {
                            std::thread::yield_now();
                        }
                    });
                }
            });
            assert_eq!(
                observe().expect("cwd survives the guards"),
                before,
                "a repo-dir guard restored the process into someone else's directory"
            );
        }
    }
}
