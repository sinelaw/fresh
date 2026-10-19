//! Plugin-driven filesystem watching.
//!
//! Backs the `watchPath` / `unwatchPath` plugin API and the
//! `path_changed` plugin hook. One process-wide `notify::Watcher`
//! is shared across all plugin watchers; each `watchPath` call
//! registers a path with `notify` and stores a per-call handle in
//! [`FileWatcherManager`] so unwatching is a removal lookup
//! rather than tearing down and rebuilding the watcher.
//!
//! Events flow notify-thread → AsyncBridge → main loop →
//! `path_changed` hook. The path is passed verbatim from
//! `notify::Event::paths` (no canonicalisation, no debouncing —
//! plugins decide their dedup policy).
//!
//! **Why not per-plugin watchers?** notify's backends (inotify on
//! Linux, kqueue on BSD/macOS, ReadDirectoryChangesW on Windows)
//! all have per-process file-descriptor / handle limits. A single
//! shared `Watcher` reuses one fd per directory across plugins
//! that happen to watch the same path, which matters once
//! Orchestrator's collision radar is watching one path per worktree
//! across N sessions.
//!
//! **Why non-recursive watches poll on macOS.** notify's kqueue backend
//! needs one open fd per watched file, and when a watched directory gains
//! or loses an entry it re-adds the first entry it isn't yet watching
//! *recursively*, whatever mode the directory was registered with
//! (`notify` 8.2, `kqueue.rs`). A non-recursive watch on a project root
//! therefore opens all of `target/` or `.git` the first time something at
//! the root changes, one more top-level entry per change, and unwatching
//! the root never closes them — the daemon ran out of fds (#3523). It
//! also loses a watch on a single file once the file is replaced by a
//! rename, which is how git writes `.git/HEAD`. A `PollWatcher` holds no
//! fds between scans and stats by path, so on macOS every non-recursive
//! watch goes to one shared poller instead. Recursive watches stay on
//! kqueue: they open the whole tree by design and do not ratchet.

use crate::services::async_bridge::{AsyncBridge, AsyncMessage, PathChangeKind};
use notify::{
    event::{CreateKind, EventKind, ModifyKind, RemoveKind},
    PollWatcher, RecommendedWatcher, RecursiveMode, Watcher,
};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

/// How often the poller re-scans its watches where non-recursive
/// watches poll (macOS). Matches `auto_revert_poll_interval_ms`'s default.
const NON_RECURSIVE_POLL_INTERVAL: Duration = Duration::from_secs(2);

/// Which watcher a handle's path is registered with, so `unwatch`
/// removes it from the same one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Backend {
    Native,
    Poll,
}

/// Allocate a process-globally-unique watch handle. The notify-callback
/// lookup table ([`handle_map`]) is process-global, so handles must be
/// unique across *every* [`FileWatcherManager`] in the process — not just
/// within one. A per-manager counter (the old design) hands out `1, 2, …`
/// in each manager, so two managers (e.g. two editor instances in one
/// process, or two parallel tests) would collide on handle `1` and clobber
/// each other's entry in the shared map, silently dropping events.
fn alloc_global_handle() -> u64 {
    static NEXT: AtomicU64 = AtomicU64::new(1);
    NEXT.fetch_add(1, Ordering::Relaxed)
}

/// Manages plugin-registered file watchers. Created on demand the
/// first time a `WatchPath` arrives — the `notify::Watcher` is
/// non-zero-cost (spawns a backend thread on macOS / Windows) and
/// many editor instances never need it at all.
pub struct FileWatcherManager {
    /// The single shared notify `Watcher`. `None` until the first
    /// successful `watch` call wires up the AsyncBridge route.
    watcher: Option<RecommendedWatcher>,
    /// The single shared poller for non-recursive watches, where
    /// [`Self::poll_non_recursive`] routes them here. `None` until the
    /// first such watch.
    poller: Option<PollWatcher>,
    /// `Some(interval)` sends non-recursive watches to [`Self::poller`],
    /// scanning at `interval`; `None` keeps every watch on the native
    /// backend. See the module docs for why macOS polls.
    poll_non_recursive: Option<Duration>,
    /// `handle → (path, backend)`. Used by `unwatch` to find
    /// what `notify::Watcher::unwatch` should be called with,
    /// and on which watcher.
    handles: HashMap<u64, (PathBuf, Backend)>,
}

impl FileWatcherManager {
    pub fn new() -> Self {
        Self::with_non_recursive_polling(
            cfg!(target_os = "macos").then_some(NON_RECURSIVE_POLL_INTERVAL),
        )
    }

    /// A manager whose non-recursive watches poll at `interval` (or use
    /// the native backend when `None`) regardless of platform, so the
    /// macOS routing is testable everywhere.
    fn with_non_recursive_polling(interval: Option<Duration>) -> Self {
        Self {
            watcher: None,
            poller: None,
            poll_non_recursive: interval,
            handles: HashMap::new(),
        }
    }

    /// Register a watch. `bridge` is needed only on the first call
    /// to construct the `Watcher`; subsequent calls reuse the
    /// existing Watcher and ignore the parameter.
    ///
    /// Returns the allocated handle on success, or an error string
    /// on `notify` failures (path missing, permission, kernel
    /// limit). Errors are surfaced to the plugin via
    /// `WatchPathRegistered::result`.
    pub fn watch(
        &mut self,
        bridge: &AsyncBridge,
        path: &Path,
        recursive: bool,
    ) -> Result<u64, String> {
        let mode = if recursive {
            RecursiveMode::Recursive
        } else {
            RecursiveMode::NonRecursive
        };
        let watched = match self.poll_non_recursive {
            Some(interval) if !recursive => {
                // `PollWatcher::watch` accepts a missing path silently;
                // fail it here as the native backends do.
                std::fs::metadata(path)
                    .map_err(|e| format!("watchPath({}): {}", path.display(), e))?;
                if self.poller.is_none() {
                    self.poller = Some(build_poller(bridge.clone(), interval)?);
                }
                let poller = self
                    .poller
                    .as_mut()
                    .expect("just constructed above if missing");
                poller.watch(path, mode).map(|()| Backend::Poll)
            }
            _ => {
                if self.watcher.is_none() {
                    self.watcher = Some(build_watcher(bridge.clone())?);
                }
                let watcher = self
                    .watcher
                    .as_mut()
                    .expect("just constructed above if missing");
                watcher.watch(path, mode).map(|()| Backend::Native)
            }
        };
        let backend = watched.map_err(|e| format!("watchPath({}): {}", path.display(), e))?;
        let handle = alloc_global_handle();
        self.handles.insert(handle, (path.to_path_buf(), backend));
        // The notify event callback uses a shared `handles` map
        // (set up below in `build_watcher`) to look up which
        // handle owns each event. Update that here too — but the
        // callback uses a clone-on-write Arc<Mutex<>> that we
        // need to thread through. Pulled out into a closure below.
        register_handle(handle, path);
        Ok(handle)
    }

    /// Drop a registered watcher. Unknown handles are ignored.
    pub fn unwatch(&mut self, handle: u64) {
        if let Some((path, backend)) = self.handles.remove(&handle) {
            unregister_handle(handle);
            let unwatched = match backend {
                Backend::Native => self.watcher.as_mut().map(|w| w.unwatch(&path)),
                Backend::Poll => self.poller.as_mut().map(|w| w.unwatch(&path)),
            };
            if let Some(Err(e)) = unwatched {
                tracing::debug!(
                    "unwatchPath({}): notify returned {}; \
                     continuing — the editor's view is now consistent",
                    path.display(),
                    e
                );
            }
        }
    }
}

impl Default for FileWatcherManager {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------
// Notify event → AsyncMessage routing
//
// notify delivers events on its backend thread; we translate them
// into `AsyncMessage::PathChanged` and post via the AsyncBridge.
// The mapping is many-to-many: one event may carry multiple paths
// (rename old / new), and one path may match multiple registered
// watchers (a path watched directly + a parent watched
// recursively). Strategy:
//
// - For each `Event::paths`, find every registered handle whose
//   watch path is an ancestor (recursive) or equal (non-recursive)
//   to the event path.
// - Emit one `PathChanged` per (handle, path) pair.
//
// We store the handle map in a process-global `Arc<Mutex<>>`
// because `notify::Watcher`'s callback closure must be `'static`
// and the manager itself owns the handles HashMap. Sharing via a
// global is the simplest option that doesn't require restructuring
// FileWatcherManager into an `Arc<Mutex<>>` (which would force
// every editor caller through `lock()`).
// ---------------------------------------------------------------

/// Type alias kept short for readability. Stores `(path, recursive)`
/// keyed by handle — the source of truth for the notify callback's
/// path-prefix lookups.
type HandleMap = HashMap<u64, (PathBuf, RecursiveMode)>;

fn handle_map() -> &'static Mutex<HandleMap> {
    use std::sync::OnceLock;
    static MAP: OnceLock<Mutex<HandleMap>> = OnceLock::new();
    MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

fn register_handle(handle: u64, path: &Path) {
    if let Ok(mut map) = handle_map().lock() {
        // The recursive flag is recovered from the manager's own
        // map; here we store an arbitrary mode — the lookup uses
        // the manager's mode. (Could simplify by removing this
        // mode from the global map; left for future readers who
        // want the global-only fast-path.)
        map.insert(handle, (path.to_path_buf(), RecursiveMode::Recursive));
    }
}

fn unregister_handle(handle: u64) {
    if let Ok(mut map) = handle_map().lock() {
        map.remove(&handle);
    }
}

fn matches_handle(watch_path: &Path, recursive: RecursiveMode, event_path: &Path) -> bool {
    match recursive {
        RecursiveMode::Recursive => event_path.starts_with(watch_path),
        RecursiveMode::NonRecursive => {
            // notify reports the changed path verbatim. For
            // non-recursive watches we accept the watch path
            // itself OR its direct children — the user's mental
            // model of "watch this directory" includes its
            // immediate contents. Sub-children fall through.
            event_path == watch_path
                || event_path
                    .parent()
                    .map(|p| p == watch_path)
                    .unwrap_or(false)
        }
    }
}

fn classify_kind(kind: &EventKind) -> PathChangeKind {
    match kind {
        EventKind::Create(CreateKind::File)
        | EventKind::Create(CreateKind::Folder)
        | EventKind::Create(CreateKind::Any)
        | EventKind::Create(CreateKind::Other) => PathChangeKind::Create,
        EventKind::Remove(RemoveKind::File)
        | EventKind::Remove(RemoveKind::Folder)
        | EventKind::Remove(RemoveKind::Any)
        | EventKind::Remove(RemoveKind::Other) => PathChangeKind::Delete,
        EventKind::Modify(ModifyKind::Name(_)) => PathChangeKind::Rename,
        EventKind::Modify(_) => PathChangeKind::Modify,
        _ => PathChangeKind::Other,
    }
}

fn build_watcher(bridge: AsyncBridge) -> Result<RecommendedWatcher, String> {
    notify::recommended_watcher(event_handler(bridge))
        .map_err(|e| format!("notify::recommended_watcher: {}", e))
}

fn build_poller(bridge: AsyncBridge, interval: Duration) -> Result<PollWatcher, String> {
    PollWatcher::new(
        event_handler(bridge),
        notify::Config::default().with_poll_interval(interval),
    )
    .map_err(|e| format!("notify::PollWatcher: {}", e))
}

/// The callback both watchers share: route each event path to every
/// handle that covers it.
fn event_handler(bridge: AsyncBridge) -> impl Fn(notify::Result<notify::Event>) + Send + 'static {
    let bridge = Arc::new(bridge);
    move |res: notify::Result<notify::Event>| {
        let event = match res {
            Ok(e) => e,
            Err(e) => {
                tracing::debug!("notify error event: {}", e);
                return;
            }
        };
        let kind = classify_kind(&event.kind);
        let map = match handle_map().lock() {
            Ok(m) => m,
            Err(_) => return,
        };
        for path in event.paths.iter() {
            for (handle, (watch_path, mode)) in map.iter() {
                if matches_handle(watch_path, *mode, path) {
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = bridge.sender().send(AsyncMessage::PathChanged {
                        handle: *handle,
                        path: path.clone(),
                        kind,
                    });
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Recursive matches: any descendant of the watch path counts;
    /// non-recursive matches only the watch path itself or its
    /// direct children.
    #[test]
    fn matches_handle_respects_recursive_mode() {
        let root = Path::new("/repo");
        assert!(matches_handle(
            root,
            RecursiveMode::Recursive,
            Path::new("/repo/src/lib.rs")
        ));
        assert!(matches_handle(
            root,
            RecursiveMode::NonRecursive,
            Path::new("/repo/lib.rs")
        ));
        assert!(!matches_handle(
            root,
            RecursiveMode::NonRecursive,
            Path::new("/repo/src/lib.rs")
        ));
        assert!(!matches_handle(
            root,
            RecursiveMode::Recursive,
            Path::new("/other/file.rs")
        ));
    }

    /// Wait (semantically; nextest bounds it) for a `PathChanged` on
    /// `handle` that satisfies `pred`, draining everything else.
    fn wait_for_change(
        bridge: &AsyncBridge,
        handle: u64,
        pred: impl Fn(&Path, PathChangeKind) -> bool,
    ) {
        loop {
            for msg in bridge.try_recv_all() {
                if let AsyncMessage::PathChanged {
                    handle: h,
                    path,
                    kind,
                } = msg
                {
                    if h == handle && pred(&path, kind) {
                        return;
                    }
                }
            }
            std::thread::sleep(Duration::from_millis(20));
        }
    }

    /// Where non-recursive watches poll (macOS), one on a directory
    /// reports entries created in it, and a recursive watch still goes
    /// to the native backend.
    #[test]
    fn non_recursive_watch_polls_when_enabled() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().to_path_buf();
        let bridge = AsyncBridge::new();
        let mut manager =
            FileWatcherManager::with_non_recursive_polling(Some(Duration::from_millis(50)));

        let handle = manager.watch(&bridge, &root, false).unwrap();
        assert_eq!(manager.handles[&handle].1, Backend::Poll);
        let recursive = manager.watch(&bridge, &root, true).unwrap();
        assert_eq!(manager.handles[&recursive].1, Backend::Native);
        manager.unwatch(recursive);

        let marker = root.join(".envrc");
        std::fs::write(&marker, "x").unwrap();
        wait_for_change(&bridge, handle, |path, kind| {
            path == marker && matches!(kind, PathChangeKind::Create)
        });

        manager.unwatch(handle);
        assert!(manager.handles.is_empty());
    }

    /// `PollWatcher::watch` accepts a missing path; the manager still
    /// fails it, as the native backends do, so plugins see the error.
    #[test]
    fn polled_watch_on_missing_path_fails() {
        let dir = tempfile::tempdir().unwrap();
        let bridge = AsyncBridge::new();
        let mut manager =
            FileWatcherManager::with_non_recursive_polling(Some(Duration::from_millis(50)));
        assert!(manager
            .watch(&bridge, &dir.path().join("missing"), false)
            .is_err());
        assert!(manager.handles.is_empty());
    }

    /// Kind classification buckets every notify-supplied variant
    /// into one of the five exposed strings.
    #[test]
    fn kind_classification_covers_main_variants() {
        use notify::event::*;
        assert!(matches!(
            classify_kind(&EventKind::Create(CreateKind::File)),
            PathChangeKind::Create
        ));
        assert!(matches!(
            classify_kind(&EventKind::Remove(RemoveKind::File)),
            PathChangeKind::Delete
        ));
        assert!(matches!(
            classify_kind(&EventKind::Modify(ModifyKind::Data(DataChange::Content))),
            PathChangeKind::Modify
        ));
        assert!(matches!(
            classify_kind(&EventKind::Modify(ModifyKind::Name(RenameMode::Both))),
            PathChangeKind::Rename
        ));
    }
}
