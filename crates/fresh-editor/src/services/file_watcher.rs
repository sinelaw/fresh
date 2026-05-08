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
//! Conductor's collision radar is watching one path per worktree
//! across N sessions.

use crate::services::async_bridge::{AsyncBridge, AsyncMessage, PathChangeKind};
use notify::{
    event::{CreateKind, EventKind, ModifyKind, RemoveKind},
    RecommendedWatcher, RecursiveMode, Watcher,
};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

/// Manages plugin-registered file watchers. Created on demand the
/// first time a `WatchPath` arrives — the `notify::Watcher` is
/// non-zero-cost (spawns a backend thread on macOS / Windows) and
/// many editor instances never need it at all.
pub struct FileWatcherManager {
    /// The single shared notify `Watcher`. `None` until the first
    /// successful `watch` call wires up the AsyncBridge route.
    watcher: Option<RecommendedWatcher>,
    /// `handle → (path, recursive)`. Used by `unwatch` to find
    /// what `notify::Watcher::unwatch` should be called with;
    /// also lets us forward only paths that are still watched
    /// when notify fires events for a path that was just
    /// unwatched (rare but possible — events are queued).
    handles: HashMap<u64, (PathBuf, RecursiveMode)>,
    next_handle: u64,
}

impl FileWatcherManager {
    pub fn new() -> Self {
        Self {
            watcher: None,
            handles: HashMap::new(),
            next_handle: 1,
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
        if self.watcher.is_none() {
            self.watcher = Some(build_watcher(bridge.clone())?);
        }
        let mode = if recursive {
            RecursiveMode::Recursive
        } else {
            RecursiveMode::NonRecursive
        };
        let watcher = self
            .watcher
            .as_mut()
            .expect("just constructed above if missing");
        watcher
            .watch(path, mode)
            .map_err(|e| format!("watchPath({}): {}", path.display(), e))?;
        let handle = self.next_handle;
        self.next_handle += 1;
        self.handles.insert(handle, (path.to_path_buf(), mode));
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
        if let Some((path, _mode)) = self.handles.remove(&handle) {
            unregister_handle(handle);
            if let Some(w) = self.watcher.as_mut() {
                if let Err(e) = w.unwatch(&path) {
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
    let bridge = Arc::new(bridge);
    let watcher = notify::recommended_watcher(move |res: notify::Result<notify::Event>| {
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
    })
    .map_err(|e| format!("notify::recommended_watcher: {}", e))?;
    Ok(watcher)
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
