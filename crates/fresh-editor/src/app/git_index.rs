//! Git-index discovery and watching for the file-tree change poller.
//!
//! The file explorer refreshes its git-status decorations when a repo's
//! `.git/index` changes. This module owns the logic that
//!   1. discovers every index to watch — one per repo, or several in a
//!      monorepo whose root is not itself a repo — on a background thread so
//!      the first poll never blocks the event loop, and
//!   2. folds the results back into the active window's explicit watch set.
//!
//! NOTE: a parallel BFS lives on the TypeScript side in
//! `lib/git_history.ts` (`discoverSubRepos`). Keep the two in sync.

use super::Editor;
use crate::model::filesystem::FileSystem;
use std::path::PathBuf;
use std::sync::Arc;

impl Editor {
    /// Fold in the result of a background git-index resolution if the thread
    /// has finished. Seeds the explicit watch set (`watched_git_indexes`) and
    /// records each index's mtime. Called at the top of the file-tree poll.
    pub(super) fn collect_git_index_resolution(&mut self) {
        if let Some(ref rx) = self.active_window_mut().pending_git_index_rx {
            match rx.try_recv() {
                Ok(seeded) => {
                    self.active_window_mut().pending_git_index_rx = None;
                    self.active_window_mut().git_index_resolved = true;
                    for (path, mtime) in seeded {
                        self.active_window_mut()
                            .watched_git_indexes
                            .push(path.clone());
                        if let Some(mtime) = mtime {
                            self.active_window_mut().dir_mod_times.insert(path, mtime);
                        }
                    }
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => {}
                Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                    self.active_window_mut().pending_git_index_rx = None;
                    self.active_window_mut().git_index_resolved = true;
                }
            }
        }
    }

    /// Kick off the one-shot git-index resolution on a background thread, if it
    /// hasn't run yet and isn't already in flight. Resolution spawns one `git`
    /// process per repo plus a directory scan, so it must not run inline on the
    /// event loop. `git_index_resolved` only flips true once results land (in
    /// `collect_git_index_resolution`), so the guard below prevents spawning a
    /// second thread while one is pending.
    pub(super) fn spawn_git_index_resolution(&mut self) {
        if self.active_window().git_index_resolved
            || self.active_window().pending_git_index_rx.is_some()
        {
            return;
        }
        let Some(rt) = self.tokio_runtime.clone() else {
            // No tokio runtime (minimal test setups) — nothing to resolve.
            self.active_window_mut().git_index_resolved = true;
            return;
        };
        let spawner = self.authority().process_spawner.clone();
        let fs = self.authority().filesystem.clone();
        let working_dir = self.working_dir().to_path_buf();
        let (tx, rx) = std::sync::mpsc::channel();
        std::thread::Builder::new()
            .name("resolve-git-indexes".to_string())
            .spawn(move || {
                let indexes = resolve_git_indexes_blocking(spawner, fs.clone(), working_dir, rt);
                // Pair each index with its current mtime so the main thread
                // just inserts without extra stat calls.
                let seeded: Vec<(PathBuf, Option<std::time::SystemTime>)> = indexes
                    .into_iter()
                    .map(|path| {
                        let mtime = fs.metadata(&path).ok().and_then(|m| m.modified);
                        (path, mtime)
                    })
                    .collect();
                // Receiver may have been dropped during shutdown.
                if tx.send(seeded).is_err() {}
            })
            .ok();
        self.active_window_mut().pending_git_index_rx = Some(rx);
    }
}

/// Resolve the paths to every `.git/index` reachable from `working_dir`:
/// the workspace root's own index when it is a repo, plus every nested
/// sub-repo's index found by BFS-scanning subdirectories up to 3 levels deep.
/// The two are additive so a monorepo whose root is *itself* a repo still has
/// its vendored sub-repos watched (#2592); a plain multi-repo workspace (root
/// not a repo) is covered by the same scan.
///
/// Uses the `ProcessSpawner` so it works transparently on both local and
/// remote (SSH) filesystems. Takes owned handles (no `&self`) so it can run on
/// a background thread.
fn resolve_git_indexes_blocking(
    spawner: Arc<dyn crate::services::remote::ProcessSpawner>,
    fs: Arc<dyn FileSystem + Send + Sync>,
    working_dir: PathBuf,
    rt: Arc<tokio::runtime::Runtime>,
) -> Vec<PathBuf> {
    let mut indexes: Vec<PathBuf> = Vec::new();

    // Watch the workspace root's own index when it is itself a repo.
    if let Some(index) = git_dir_index(&spawner, &working_dir, &rt) {
        indexes.push(index);
    }

    // Always also scan subdirectories for nested sub-repos' .git/index. A
    // monorepo whose root is *itself* a repo can still vendor independent git
    // repos, and their indexes must be watched too so the explorer refreshes
    // when they change (#2592) — mirroring the TypeScript `discoverSubRepos`
    // (lib/git_repo.ts), which now discovers sub-repos in both cases. When the
    // root is not a repo this is the sole discovery path (plain monorepo).
    // `MAX_DEPTH` levels below the working dir are scanned (level 1 = direct
    // children); this MUST match the levels the TypeScript walk scans — the two
    // are expected to discover the same repos.
    use std::collections::VecDeque;
    const MAX_DEPTH: u32 = 3;
    // Queue entries are (dir_to_scan, level_of_that_dir's_children).
    let mut queue: VecDeque<(PathBuf, u32)> = VecDeque::new();
    queue.push_back((working_dir, 1));

    while let Some((dir, level)) = queue.pop_front() {
        let entries = match fs.read_dir(&dir) {
            Ok(e) => e,
            Err(_) => continue,
        };

        for entry in entries {
            if !entry.is_dir() {
                continue;
            }
            if entry.name.starts_with('.') || entry.name == "node_modules" {
                continue;
            }

            let dot_git = entry.path.join(".git");
            if fs.exists(&dot_git) {
                if let Some(index) = git_dir_index(&spawner, &entry.path, &rt) {
                    if !indexes.contains(&index) {
                        indexes.push(index);
                    }
                }
            } else if level < MAX_DEPTH {
                queue.push_back((entry.path.clone(), level + 1));
            }
        }
    }

    indexes
}

/// Resolve the `.git/index` path for the repo rooted at (or containing) `dir`,
/// or `None` when `dir` is not inside a git repo. Shared by the workspace-root
/// probe and the nested sub-repo scan so both resolve the index the same way.
fn git_dir_index(
    spawner: &Arc<dyn crate::services::remote::ProcessSpawner>,
    dir: &std::path::Path,
    rt: &Arc<tokio::runtime::Runtime>,
) -> Option<PathBuf> {
    let cwd = dir.to_string_lossy().to_string();
    let result = rt.block_on(spawner.spawn(
        "git".to_string(),
        vec!["rev-parse".to_string(), "--git-dir".to_string()],
        Some(cwd),
    ));
    let output = result.ok()?;
    if output.exit_code != 0 {
        return None;
    }
    let git_dir = output.stdout.trim();
    let git_dir_path = if std::path::Path::new(git_dir).is_absolute() {
        PathBuf::from(git_dir)
    } else {
        dir.join(git_dir)
    };
    Some(git_dir_path.join("index"))
}
