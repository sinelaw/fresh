//! Persistent list of recently opened project roots (working directories),
//! plus user-pinned entries. Backs the "Open Recent Project" picker (issue
//! #1895).
//!
//! ## Storage
//!
//! A single JSON file `recent_projects.json` under the platform data dir
//! (next to `workspaces/`). Like [`crate::workspace`] and the recovery store
//! this is *local* app state — never project/buffer content — so it uses
//! `std::fs` directly (not the remote-capable `FileSystem` trait) and is
//! written atomically (temp file + fsync + rename) so it is never left
//! half-written.
//!
//! ## Resilience
//!
//! Recent projects is a convenience, never load-bearing. A missing,
//! unreadable, malformed, or future-versioned file degrades to an empty list
//! rather than surfacing an error.

use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use serde::{Deserialize, Serialize};

/// Schema version for forward-compatible migrations.
pub const RECENT_PROJECTS_VERSION: u32 = 1;

/// Maximum number of *unpinned* recent entries kept. Pinned entries are always
/// retained regardless of this cap.
pub const MAX_RECENT: usize = 20;

/// A single recorded project root.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RecentProject {
    /// Absolute (canonicalized) path of the project root.
    pub path: PathBuf,
    /// Whether the user has pinned this project to the top of the list.
    #[serde(default)]
    pub pinned: bool,
    /// Unix seconds when this project was last opened. Used only for ordering.
    #[serde(default)]
    pub last_opened: u64,
}

/// The persisted recent/pinned-projects store.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RecentProjects {
    /// Schema version for future migrations.
    pub version: u32,
    /// All recorded projects, in no particular order. Use [`Self::ordered`] for
    /// display order.
    #[serde(default)]
    pub projects: Vec<RecentProject>,
}

impl Default for RecentProjects {
    fn default() -> Self {
        Self {
            version: RECENT_PROJECTS_VERSION,
            projects: Vec::new(),
        }
    }
}

/// Current Unix time in seconds (0 on the impossible pre-epoch clock).
pub fn now_secs() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

/// Path of the store file under `data_dir`.
pub fn store_path(data_dir: &Path) -> PathBuf {
    data_dir.join("recent_projects.json")
}

impl RecentProjects {
    /// Load the store from `data_dir`, degrading to an empty store on any
    /// problem (missing/unreadable/malformed/future-version).
    pub fn load(data_dir: &Path) -> Self {
        let path = store_path(data_dir);
        let content = match std::fs::read_to_string(&path) {
            Ok(c) => c,
            Err(_) => return Self::default(),
        };
        match serde_json::from_str::<RecentProjects>(&content) {
            // Refuse to interpret a file written by a newer, incompatible
            // version — better to start fresh than to silently drop fields on
            // the next save.
            Ok(rp) if rp.version <= RECENT_PROJECTS_VERSION => rp,
            _ => Self::default(),
        }
    }

    /// Persist the store to `data_dir` using an atomic write.
    pub fn save(&self, data_dir: &Path) -> std::io::Result<()> {
        let path = store_path(data_dir);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let content = serde_json::to_string_pretty(self)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;

        let temp_path = path.with_extension("json.tmp");
        {
            let mut file = std::fs::File::create(&temp_path)?;
            file.write_all(content.as_bytes())?;
            file.sync_all()?;
        }
        std::fs::rename(&temp_path, &path)?;
        Ok(())
    }

    /// Record that `path` was opened at `now` (Unix seconds). Updates the
    /// existing entry's timestamp (preserving its pinned flag) or inserts a new
    /// unpinned entry, then enforces the recency cap on unpinned entries.
    pub fn record_open(&mut self, path: &Path, now: u64) {
        if let Some(entry) = self.projects.iter_mut().find(|e| e.path == path) {
            entry.last_opened = now;
        } else {
            self.projects.push(RecentProject {
                path: path.to_path_buf(),
                pinned: false,
                last_opened: now,
            });
        }
        self.enforce_cap();
    }

    /// Toggle the pinned state of `path`, returning the new state. If `path` is
    /// not yet recorded it is inserted as pinned (records the moment of
    /// pinning).
    pub fn toggle_pinned(&mut self, path: &Path, now: u64) -> bool {
        if let Some(entry) = self.projects.iter_mut().find(|e| e.path == path) {
            entry.pinned = !entry.pinned;
            let pinned = entry.pinned;
            // Un-pinning may push the unpinned set over the cap.
            if !pinned {
                self.enforce_cap();
            }
            pinned
        } else {
            self.projects.push(RecentProject {
                path: path.to_path_buf(),
                pinned: true,
                last_opened: now,
            });
            true
        }
    }

    /// Whether `path` is currently recorded and pinned.
    pub fn is_pinned(&self, path: &Path) -> bool {
        self.projects.iter().any(|e| e.path == path && e.pinned)
    }

    /// Display order: pinned entries first, then unpinned, each most-recent
    /// first.
    pub fn ordered(&self) -> Vec<&RecentProject> {
        let mut pinned: Vec<&RecentProject> = self.projects.iter().filter(|e| e.pinned).collect();
        let mut recent: Vec<&RecentProject> = self.projects.iter().filter(|e| !e.pinned).collect();
        pinned.sort_by_key(|e| std::cmp::Reverse(e.last_opened));
        recent.sort_by_key(|e| std::cmp::Reverse(e.last_opened));
        pinned.into_iter().chain(recent).collect()
    }

    /// Drop the oldest unpinned entries until at most [`MAX_RECENT`] remain.
    /// Pinned entries are never counted or removed.
    fn enforce_cap(&mut self) {
        let mut unpinned: Vec<usize> = self
            .projects
            .iter()
            .enumerate()
            .filter(|(_, e)| !e.pinned)
            .map(|(i, _)| i)
            .collect();
        if unpinned.len() <= MAX_RECENT {
            return;
        }
        // Oldest first.
        unpinned.sort_by_key(|&i| self.projects[i].last_opened);
        let remove_count = unpinned.len() - MAX_RECENT;
        let mut to_remove: Vec<usize> = unpinned.into_iter().take(remove_count).collect();
        // Remove from the back so earlier indices stay valid.
        to_remove.sort_unstable_by(|a, b| b.cmp(a));
        for i in to_remove {
            self.projects.remove(i);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(s: &str) -> PathBuf {
        PathBuf::from(s)
    }

    #[test]
    fn record_open_inserts_unpinned() {
        let mut store = RecentProjects::default();
        store.record_open(&p("/a"), 100);
        assert_eq!(store.projects.len(), 1);
        assert_eq!(store.projects[0].path, p("/a"));
        assert!(!store.projects[0].pinned);
        assert_eq!(store.projects[0].last_opened, 100);
    }

    #[test]
    fn record_open_dedups_and_updates_timestamp() {
        let mut store = RecentProjects::default();
        store.record_open(&p("/a"), 100);
        store.record_open(&p("/a"), 200);
        assert_eq!(store.projects.len(), 1);
        assert_eq!(store.projects[0].last_opened, 200);
    }

    #[test]
    fn ordered_is_recency_then_pinned_first() {
        let mut store = RecentProjects::default();
        store.record_open(&p("/a"), 100);
        store.record_open(&p("/b"), 200);
        store.record_open(&p("/c"), 300);
        // Most recent first.
        let order: Vec<_> = store.ordered().iter().map(|e| e.path.clone()).collect();
        assert_eq!(order, vec![p("/c"), p("/b"), p("/a")]);

        // Pin the oldest — it should jump to the front despite being least recent.
        assert!(store.toggle_pinned(&p("/a"), 400));
        let order: Vec<_> = store.ordered().iter().map(|e| e.path.clone()).collect();
        assert_eq!(order, vec![p("/a"), p("/c"), p("/b")]);
    }

    #[test]
    fn toggle_pinned_round_trips() {
        let mut store = RecentProjects::default();
        store.record_open(&p("/a"), 100);
        assert!(!store.is_pinned(&p("/a")));
        assert!(store.toggle_pinned(&p("/a"), 100));
        assert!(store.is_pinned(&p("/a")));
        assert!(!store.toggle_pinned(&p("/a"), 100));
        assert!(!store.is_pinned(&p("/a")));
    }

    #[test]
    fn toggle_pinned_inserts_when_absent() {
        let mut store = RecentProjects::default();
        assert!(store.toggle_pinned(&p("/new"), 100));
        assert!(store.is_pinned(&p("/new")));
        assert_eq!(store.projects.len(), 1);
    }

    #[test]
    fn cap_evicts_oldest_unpinned_but_keeps_pinned() {
        let mut store = RecentProjects::default();
        // Pin one project with a very old timestamp.
        assert!(store.toggle_pinned(&p("/pinned"), 1));
        // Fill well past the cap with unpinned projects.
        for i in 0..(MAX_RECENT + 5) {
            store.record_open(&p(&format!("/u{i}")), 1000 + i as u64);
        }
        let unpinned = store.projects.iter().filter(|e| !e.pinned).count();
        assert_eq!(unpinned, MAX_RECENT);
        // The pinned, oldest entry survives.
        assert!(store.is_pinned(&p("/pinned")));
        // The very oldest unpinned ones were evicted.
        assert!(!store.projects.iter().any(|e| e.path == p("/u0")));
        // Newest unpinned survives.
        assert!(store.projects.iter().any(|e| e.path == p("/u23")));
    }

    #[test]
    fn save_then_load_round_trips() {
        let dir = tempfile::tempdir().unwrap();
        let mut store = RecentProjects::default();
        store.record_open(&p("/a"), 100);
        store.toggle_pinned(&p("/a"), 100);
        store.record_open(&p("/b"), 200);
        store.save(dir.path()).unwrap();

        let loaded = RecentProjects::load(dir.path());
        assert_eq!(loaded, store);
        assert!(loaded.is_pinned(&p("/a")));
    }

    #[test]
    fn load_missing_returns_empty() {
        let dir = tempfile::tempdir().unwrap();
        let loaded = RecentProjects::load(dir.path());
        assert!(loaded.projects.is_empty());
        assert_eq!(loaded.version, RECENT_PROJECTS_VERSION);
    }

    #[test]
    fn load_corrupt_returns_empty() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path()).unwrap();
        std::fs::write(store_path(dir.path()), b"not json{{{").unwrap();
        let loaded = RecentProjects::load(dir.path());
        assert!(loaded.projects.is_empty());
    }

    #[test]
    fn load_future_version_returns_empty() {
        let dir = tempfile::tempdir().unwrap();
        let future = serde_json::json!({
            "version": RECENT_PROJECTS_VERSION + 1,
            "projects": [{"path": "/a", "pinned": true, "last_opened": 1}],
        });
        std::fs::write(
            store_path(dir.path()),
            serde_json::to_string(&future).unwrap(),
        )
        .unwrap();
        let loaded = RecentProjects::load(dir.path());
        assert!(loaded.projects.is_empty());
    }
}
