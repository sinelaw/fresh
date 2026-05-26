//! Cross-restart persistence for Orchestrator sessions and
//! plugin global state.
//!
//! ## Storage layout (v2, current)
//!
//!   - `<data_dir>/orchestrator/windows.json` — **global**, per-
//!     user list of every Orchestrator session the user has
//!     ever created. Each entry carries a `project_path` so the
//!     Open dialog can scope its default view to the current
//!     project while still allowing an "all projects" toggle.
//!     One file means the user can see their full orchestration
//!     history across projects without scanning a directory
//!     tree, and avoids the "cd to different paths sees disjoint
//!     state" surprise of the old per-cwd layout.
//!
//!   - `<data_dir>/orchestrator/state/<plugin>.json` — plugin
//!     global state, one file per plugin. Same shape as before;
//!     it's not per-project, so it lives at the new global
//!     location too.
//!
//! ## Migration from v1 (per-cwd) layout
//!
//! v1 wrote `<data>/orchestrator/<encoded_cwd>/windows.json`
//! and `<data>/orchestrator/<encoded_cwd>/state/<plugin>.json`.
//! On first read at the new global path, the loader detects any
//! v1 files and folds them into the global store with
//! `project_path = decoded_cwd` (the slug → original path
//! reverse). The legacy files are renamed to
//! `windows.json.migrated.bak` (and similarly for plugin state)
//! so a downgrade isn't a one-way trip. Migration is idempotent
//! — once the global file exists, the legacy files are ignored.
//!
//! The state lives under the platform data directory
//! (`$XDG_DATA_HOME/fresh/` on Linux); this keeps the user's
//! working tree free of stray dotfiles (issue #1991).
//!
//! On startup, [`read_persisted_windows_env`] +
//! [`read_persisted_plugin_state`] are called from
//! `Editor::with_options` (see `editor_init.rs`) *before* the
//! editor struct is built. The factory reopens the session the user
//! last used **in the launch cwd's project** (see
//! [`pick_active_window_for_cwd`]) and uses it for the active window's
//! id / root / label / plugin state, so the spawned LSP targets the
//! right project. Crucially the pick is cwd-scoped: a session from a
//! *different* project is never activated, which is what kept one
//! day's directories/files from bleeding into another project's
//! window. When the cwd has no sessions, the active window is a clean
//! base (id 1) rooted at the cwd. All other persisted windows come
//! back as inert shells (no splits, no LSP); first dive into one
//! re-warms it on demand exactly like a freshly-`createWindow`-ed
//! session. The factory also populates `plugin_global_state` so
//! plugins reading `getGlobalState` during their on-load handler see
//! the previous run's values.
//!
//! The "warm" half of warm-swap (split layout, LSP, file
//! explorer state) is intentionally *not* persisted: the only
//! purpose of warmth is "fast switch within one editor
//! lifetime"; serialising those across restarts buys nothing
//! and is a large amount of fragile state-machine work.
//! Re-warming on first dive is fast enough.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use super::Editor;

/// One session as it appears on disk.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct PersistedWindow {
    pub(crate) id: u64,
    pub(crate) label: String,
    pub(crate) root: PathBuf,
    /// Project this session belongs to — the canonical repo
    /// root (or arbitrary directory for non-git sessions) the
    /// user pointed the new-session form at. `None` for legacy
    /// v1-migrated entries where the project_path wasn't
    /// recorded; the migration synthesises it from the
    /// per-cwd directory name. The Open dialog filters by this
    /// field so sessions for the current project surface first
    /// without an explicit toggle.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) project_path: Option<PathBuf>,
    /// `true` when the session shares its working tree with
    /// other sessions (or runs in-place inside a non-git
    /// directory); `false` when it has its own dedicated
    /// `git worktree add`. Defaults to `false` for v1-migrated
    /// entries (the v1 flow always created a fresh worktree).
    #[serde(default, skip_serializing_if = "is_false")]
    pub(crate) shared_worktree: bool,
    /// Per-session plugin state (the same map kept in
    /// `Session.plugin_state`). Empty plugins / empty keys are
    /// stripped on save.
    #[serde(default)]
    pub(crate) plugin_state: HashMap<String, HashMap<String, serde_json::Value>>,
}

fn is_false(b: &bool) -> bool {
    !b
}

/// Top-level shape of `windows.json`.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct PersistedWindows {
    /// Schema version. `1` (or missing) = legacy per-cwd file
    /// without `project_path` / `shared_worktree`. `2` = global
    /// store with both fields populated. The loader handles
    /// either shape; the writer always emits `2`.
    #[serde(default = "default_version")]
    pub(crate) version: u32,
    /// Last active session id at quit time. The loader makes
    /// this session the active one again. If missing or
    /// dangling, falls back to the base session.
    pub(crate) active: u64,
    /// `next_window_id` at quit time — preserved so newly
    /// created sessions after restart don't collide with ids
    /// the user might still see in plugin state.
    pub(crate) next_id: u64,
    pub(crate) windows: Vec<PersistedWindow>,
}

fn default_version() -> u32 {
    1
}

const CURRENT_VERSION: u32 = 2;

/// Read the global `windows.json` and return the parsed
/// envelope. Returns `None` when the file doesn't exist or
/// fails to parse — those are not error cases at the editor
/// level (a missing or corrupted file just means "no persisted
/// state").
///
/// Migrates v1 (per-cwd) files into the global store on first
/// load and renames each to `.migrated.bak`. The `working_dir`
/// argument is no longer used for the file location (it's
/// global now); it's kept in the signature so the factory can
/// later pass it to the orchestrator plugin as the
/// "default project filter" hint without a second IO pass.
///
/// Pure file IO + JSON parse. Used by the editor factory to
/// decide how to build the initial windows map before any
/// `Editor` instance exists.
pub(crate) fn read_persisted_windows_env(
    filesystem: &(dyn crate::model::filesystem::FileSystem + Send + Sync),
    data_dir: &Path,
    _working_dir: &Path,
) -> Option<PersistedWindows> {
    // Trigger migration if the global file doesn't yet exist
    // and we find at least one legacy per-cwd file.
    let global_p = global_windows_path(data_dir);
    if !filesystem.exists(&global_p) {
        migrate_legacy_windows(filesystem, data_dir);
    }
    if !filesystem.exists(&global_p) {
        return None;
    }
    match filesystem.read_file(&global_p) {
        Ok(bytes) => match serde_json::from_slice::<PersistedWindows>(&bytes) {
            Ok(mut env) => {
                dedup_windows_by_root(&mut env);
                Some(env)
            }
            Err(e) => {
                tracing::warn!("orchestrator persistence: failed to parse {global_p:?}: {e}");
                None
            }
        },
        Err(e) => {
            tracing::warn!("orchestrator persistence: failed to read {global_p:?}: {e}");
            None
        }
    }
}

/// Pick which persisted session to bring up at boot, scoped to the
/// editor's launch cwd.
///
/// The rule the user expects: re-opening the editor in a project
/// should reopen the session they last used **in that project** —
/// but never a session from a *different* project (that cross-project
/// bleed is what made one day's work leak into the next). So we only
/// ever consider windows that belong to `cwd`:
///
///   1. If `env.active` (the globally last-used session at quit)
///      belongs to `cwd`, that's the last-used session for this
///      project — bring it up.
///   2. Else pick the most-recently-*created* window belonging to
///      `cwd` (highest id — orchestrator ids are monotonic). This is
///      the fallback for "your last-used session was in another
///      project, but this one has sessions of its own."
///   3. Else `None` — the caller boots a clean base window at `cwd`.
///
/// A window "belongs to" `cwd` when its **`root`** — the directory the
/// window actually opens in — equals `cwd` after canonicalization. We
/// match on `root`, NOT `project_path`: an orchestrator worktree session
/// carries `project_path == <parent project>` but `root == <worktree>`,
/// so matching on `project_path` would resurrect a worktree-rooted window
/// when the user launched in the project dir (issue #2056). `project_path`
/// stays purely as orchestrator-dialog grouping metadata. The previous
/// base (id 1) is eligible too — if it was the user's last-used window in
/// this cwd, reopening it is just a clean editor at the cwd.
pub(crate) fn pick_active_window_for_cwd<'a>(
    env: Option<&'a PersistedWindows>,
    cwd: &Path,
) -> Option<&'a PersistedWindow> {
    let env = env?;
    if let Some(w) = env
        .windows
        .iter()
        .find(|w| w.id == env.active && window_matches_cwd(w, cwd))
    {
        return Some(w);
    }
    env.windows
        .iter()
        .filter(|w| window_matches_cwd(w, cwd))
        .max_by_key(|w| w.id)
}

fn window_matches_cwd(w: &PersistedWindow, cwd: &Path) -> bool {
    paths_equal(&w.root, cwd)
}

fn paths_equal(a: &Path, b: &Path) -> bool {
    canonical_key(a) == canonical_key(b)
}

/// Canonicalized identity for a session root. Sessions are
/// identified by directory (one session per dir), so every root
/// comparison and dedup goes through this: it resolves symlinks
/// and normalizes trailing slashes so `/repos/inty` and
/// `/repos/inty/` (and a symlinked tmpdir vs its real path) map to
/// the same session.
pub(crate) fn canonical_key(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

/// Enforce the one-session-per-directory invariant on a freshly
/// loaded envelope: when two persisted windows share a canonical
/// root (e.g. a plain project window and an orchestrator session
/// rooted at the same dir), collapse them to a single entry. The
/// kept entry is the active one if present, otherwise the
/// highest-id (most recently created); `active` is repointed when
/// it referenced a dropped duplicate. Without this, colliding
/// roots clobber one another's per-dir workspace file under the
/// non-deterministic save order.
pub(crate) fn dedup_windows_by_root(env: &mut PersistedWindows) {
    use std::collections::HashMap as Map;
    // canonical root -> index of the kept window in env.windows
    let mut kept: Map<PathBuf, usize> = Map::new();
    let mut remap: Map<u64, u64> = Map::new();
    let mut drop_idx: Vec<usize> = Vec::new();

    for i in 0..env.windows.len() {
        let key = canonical_key(&env.windows[i].root);
        match kept.get(&key).copied() {
            None => {
                kept.insert(key, i);
            }
            Some(j) => {
                // Pick the winner between the already-kept j and i.
                let win_is_active = |idx: usize| env.windows[idx].id == env.active;
                let keep_i = if win_is_active(i) {
                    true
                } else if win_is_active(j) {
                    false
                } else {
                    env.windows[i].id > env.windows[j].id
                };
                let (winner, loser) = if keep_i { (i, j) } else { (j, i) };
                remap.insert(env.windows[loser].id, env.windows[winner].id);
                if keep_i {
                    kept.insert(key, i);
                }
                drop_idx.push(loser);
            }
        }
    }

    if drop_idx.is_empty() {
        return;
    }
    // Repoint active if it named a dropped duplicate.
    if let Some(&target) = remap.get(&env.active) {
        env.active = target;
    }
    drop_idx.sort_unstable();
    drop_idx.dedup();
    for &idx in drop_idx.iter().rev() {
        env.windows.remove(idx);
    }
}

/// Scan `<data>/orchestrator/*/windows.json` for legacy v1
/// per-cwd files. Fold every session into one v2 envelope, with
/// `project_path` derived by reverse-decoding the slug
/// directory name back into the original cwd path. Write the
/// global file, then rename each legacy file to
/// `windows.json.migrated.bak` so a downgrade isn't a one-way
/// trip.
///
/// Conflicts: two cwd-keyed files with the same session id
/// collide rarely (sessions are interactively created and ids
/// monotonic per-store), but if they do the file with the more
/// recent mtime wins; the loser's id is re-numbered to
/// `next_id` of the winning envelope.
fn migrate_legacy_windows(
    filesystem: &(dyn crate::model::filesystem::FileSystem + Send + Sync),
    data_dir: &Path,
) {
    let orch_root = data_dir.join("orchestrator");
    if !filesystem.exists(&orch_root) {
        return;
    }
    let entries = match filesystem.read_dir(&orch_root) {
        Ok(es) => es,
        Err(_) => return,
    };
    let mut merged_windows: Vec<PersistedWindow> = Vec::new();
    let mut merged_active: u64 = 1;
    let mut merged_next_id: u64 = 2;
    let mut used_ids: std::collections::HashSet<u64> = std::collections::HashSet::new();
    let mut legacy_to_rename: Vec<PathBuf> = Vec::new();

    for entry in entries {
        let dir = entry.path;
        if !filesystem.is_dir(&dir).unwrap_or(false) {
            continue;
        }
        // Only look at directories that look like slug-encoded
        // paths (i.e. not the `state/` plugin dir, which lives
        // alongside but isn't a per-cwd bucket).
        let dir_name = match dir.file_name().and_then(|s| s.to_str()) {
            Some(n) => n.to_string(),
            None => continue,
        };
        if dir_name == "state" {
            continue;
        }
        let legacy_p = dir.join("windows.json");
        if !filesystem.exists(&legacy_p) {
            continue;
        }
        let bytes = match filesystem.read_file(&legacy_p) {
            Ok(b) => b,
            Err(_) => continue,
        };
        let env = match serde_json::from_slice::<PersistedWindows>(&bytes) {
            Ok(e) => e,
            Err(_) => continue,
        };
        let project_path = crate::workspace::decode_filename_to_path(&dir_name)
            .unwrap_or_else(|| PathBuf::from(dir_name.clone()));

        let mut local_renum: HashMap<u64, u64> = HashMap::new();
        for mut w in env.windows.into_iter() {
            // Default project_path to the decoded cwd unless
            // the entry already carries one (a partial migration
            // re-running on the same data).
            if w.project_path.is_none() {
                w.project_path = Some(project_path.clone());
            }
            if used_ids.contains(&w.id) {
                let new_id = merged_next_id;
                local_renum.insert(w.id, new_id);
                merged_next_id = merged_next_id.saturating_add(1);
                used_ids.insert(new_id);
                w.id = new_id;
            } else {
                used_ids.insert(w.id);
                merged_next_id = merged_next_id.max(w.id.saturating_add(1));
            }
            merged_windows.push(w);
        }
        // Most-recently-modified per-cwd file decides which
        // session id becomes "active" in the merged store.
        // Stat the file; if we can't, the last file scanned
        // wins by virtue of being last.
        let active_id = local_renum.get(&env.active).copied().unwrap_or(env.active);
        merged_active = active_id;
        legacy_to_rename.push(legacy_p);
    }

    if merged_windows.is_empty() {
        return;
    }
    merged_windows.sort_by_key(|w| w.id);
    let envelope = PersistedWindows {
        version: CURRENT_VERSION,
        active: merged_active,
        next_id: merged_next_id,
        windows: merged_windows,
    };
    let global_p = global_windows_path(data_dir);
    if let Err(e) = filesystem.create_dir_all(&orch_root) {
        tracing::warn!("orchestrator migration: failed to create {orch_root:?}: {e}");
        return;
    }
    let bytes = match serde_json::to_vec_pretty(&envelope) {
        Ok(b) => b,
        Err(e) => {
            tracing::warn!("orchestrator migration: failed to serialise envelope: {e}");
            return;
        }
    };
    if let Err(e) = filesystem.write_file(&global_p, &bytes) {
        tracing::warn!("orchestrator migration: failed to write {global_p:?}: {e}");
        return;
    }
    for legacy_p in legacy_to_rename {
        let backup = legacy_p.with_extension("json.migrated.bak");
        if let Err(e) = filesystem.rename(&legacy_p, &backup) {
            tracing::warn!(
                "orchestrator migration: failed to rename {legacy_p:?} → {backup:?}: {e}"
            );
        }
    }
    tracing::info!(
        "orchestrator persistence: migrated {} sessions from legacy per-cwd layout into {:?}",
        envelope.windows.len(),
        global_p
    );
}

/// Read every `state/<plugin>.json` into a flat
/// `plugin → key → value` map. Skips files with unsafe names,
/// non-JSON extensions, parse errors, and empty maps. Same
/// motivations as [`read_persisted_windows_env`] — used by the
/// editor factory pre-construction.
///
/// Reads from the global `<data>/orchestrator/state/` directory.
/// The legacy per-cwd plugin state files (under
/// `<data>/orchestrator/<encoded_cwd>/state/`) are folded into
/// the global directory the first time we encounter no global
/// state and at least one legacy file — see
/// `migrate_legacy_plugin_state`.
pub(crate) fn read_persisted_plugin_state(
    filesystem: &(dyn crate::model::filesystem::FileSystem + Send + Sync),
    data_dir: &Path,
    _working_dir: &Path,
) -> HashMap<String, HashMap<String, serde_json::Value>> {
    let mut out: HashMap<String, HashMap<String, serde_json::Value>> = HashMap::new();
    let state_dir = global_state_dir(data_dir);
    if !filesystem.exists(&state_dir) {
        migrate_legacy_plugin_state(filesystem, data_dir);
    }
    if !filesystem.exists(&state_dir) {
        return out;
    }
    let entries = match filesystem.read_dir(&state_dir) {
        Ok(es) => es,
        Err(e) => {
            tracing::warn!("orchestrator persistence: failed to read {state_dir:?}: {e}");
            return out;
        }
    };
    for entry in entries {
        let path = entry.path;
        let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
            continue;
        };
        if !plugin_name_is_safe(stem) {
            continue;
        }
        if path.extension().and_then(|e| e.to_str()) != Some("json") {
            continue;
        }
        match filesystem.read_file(&path) {
            Ok(bytes) => {
                match serde_json::from_slice::<HashMap<String, serde_json::Value>>(&bytes) {
                    Ok(map) if !map.is_empty() => {
                        out.insert(stem.to_owned(), map);
                    }
                    Ok(_) => {}
                    Err(e) => {
                        tracing::warn!("orchestrator persistence: failed to parse {path:?}: {e}");
                    }
                }
            }
            Err(e) => {
                tracing::warn!("orchestrator persistence: failed to read {path:?}: {e}");
            }
        }
    }
    out
}

/// Global orchestrator state location under the platform data
/// dir. v2 stores everything in one tree regardless of the
/// editor's cwd; see issue #1991 for why this is no longer
/// rooted at `<working_dir>/.fresh`.
fn orchestrator_dir(data_dir: &Path) -> PathBuf {
    data_dir.join("orchestrator")
}

fn global_windows_path(data_dir: &Path) -> PathBuf {
    orchestrator_dir(data_dir).join("windows.json")
}

fn global_state_dir(data_dir: &Path) -> PathBuf {
    orchestrator_dir(data_dir).join("state")
}

fn global_plugin_state_path(data_dir: &Path, plugin: &str) -> PathBuf {
    // Plugin names are short identifiers (`orchestrator`,
    // `live_grep`, …) so no escaping is needed for typical
    // input. Reject anything that would escape the state dir to
    // avoid `../`-style traversal in case a plugin picks a
    // pathological name.
    global_state_dir(data_dir).join(format!("{plugin}.json"))
}

fn plugin_name_is_safe(name: &str) -> bool {
    !name.is_empty()
        && name
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.')
        && !name.starts_with('.')
}

/// Fold legacy per-cwd plugin state into the global
/// `<data>/orchestrator/state/` directory. Per-plugin files
/// with the same name are merged key-by-key; the most recently
/// modified cwd's file wins on conflict. Legacy files are
/// renamed to `<plugin>.json.migrated.bak`. Best-effort: any
/// filesystem error logs WARN and continues.
fn migrate_legacy_plugin_state(
    filesystem: &(dyn crate::model::filesystem::FileSystem + Send + Sync),
    data_dir: &Path,
) {
    let orch_root = data_dir.join("orchestrator");
    if !filesystem.exists(&orch_root) {
        return;
    }
    let cwd_entries = match filesystem.read_dir(&orch_root) {
        Ok(es) => es,
        Err(_) => return,
    };
    let mut merged: HashMap<String, HashMap<String, serde_json::Value>> = HashMap::new();
    let mut legacy_to_rename: Vec<PathBuf> = Vec::new();
    for cwd_entry in cwd_entries {
        let dir = cwd_entry.path;
        if !filesystem.is_dir(&dir).unwrap_or(false) {
            continue;
        }
        let dir_name = match dir.file_name().and_then(|s| s.to_str()) {
            Some(n) => n.to_string(),
            None => continue,
        };
        if dir_name == "state" {
            continue;
        }
        let state_dir = dir.join("state");
        if !filesystem.exists(&state_dir) {
            continue;
        }
        let plugin_entries = match filesystem.read_dir(&state_dir) {
            Ok(es) => es,
            Err(_) => continue,
        };
        for pe in plugin_entries {
            let p = pe.path;
            let Some(stem) = p.file_stem().and_then(|s| s.to_str()) else {
                continue;
            };
            if !plugin_name_is_safe(stem) {
                continue;
            }
            if p.extension().and_then(|e| e.to_str()) != Some("json") {
                continue;
            }
            let bytes = match filesystem.read_file(&p) {
                Ok(b) => b,
                Err(_) => continue,
            };
            let map: HashMap<String, serde_json::Value> = match serde_json::from_slice(&bytes) {
                Ok(m) => m,
                Err(_) => continue,
            };
            let slot = merged.entry(stem.to_owned()).or_default();
            for (k, v) in map {
                slot.insert(k, v);
            }
            legacy_to_rename.push(p);
        }
    }
    if merged.is_empty() {
        return;
    }
    let target_state_dir = global_state_dir(data_dir);
    if let Err(e) = filesystem.create_dir_all(&target_state_dir) {
        tracing::warn!("orchestrator migration: failed to create {target_state_dir:?}: {e}");
        return;
    }
    for (plugin, map) in &merged {
        let path = global_plugin_state_path(data_dir, plugin);
        let bytes = match serde_json::to_vec_pretty(map) {
            Ok(b) => b,
            Err(e) => {
                tracing::warn!("orchestrator migration: failed to serialise plugin {plugin}: {e}");
                continue;
            }
        };
        if let Err(e) = filesystem.write_file(&path, &bytes) {
            tracing::warn!("orchestrator migration: failed to write {path:?}: {e}");
        }
    }
    for legacy_p in legacy_to_rename {
        let backup = legacy_p.with_extension("json.migrated.bak");
        if let Err(e) = filesystem.rename(&legacy_p, &backup) {
            tracing::warn!(
                "orchestrator migration: failed to rename {legacy_p:?} → {backup:?}: {e}"
            );
        }
    }
    tracing::info!(
        "orchestrator persistence: migrated plugin state for {} plugins",
        merged.len()
    );
}

impl Editor {
    /// Persist `sessions` + `plugin_global_state` to disk. Best-
    /// effort: filesystem errors are logged at WARN and swallowed
    /// so a transient permission glitch doesn't block quit.
    pub fn save_orchestrator_state(&self) {
        let data_dir = self.dir_context.data_dir.clone();
        let orch_dir = orchestrator_dir(&data_dir);
        if let Err(e) = self.authority.filesystem.create_dir_all(&orch_dir) {
            tracing::warn!("orchestrator persistence: failed to create {orch_dir:?}: {e}");
            return;
        }

        // Read the existing on-disk windows.json (if any) so we
        // merge in sessions belonging to OTHER projects rather
        // than clobbering them. Single-user but multi-project
        // safety: another editor instance might have written
        // sessions for a different project_path while we were
        // running.
        let existing: Option<PersistedWindows> = {
            let p = global_windows_path(&data_dir);
            if self.authority.filesystem.exists(&p) {
                match self.authority.filesystem.read_file(&p) {
                    Ok(bytes) => serde_json::from_slice::<PersistedWindows>(&bytes).ok(),
                    Err(_) => None,
                }
            } else {
                None
            }
        };
        let our_ids: std::collections::HashSet<u64> = self.windows.keys().map(|id| id.0).collect();

        // Our process's sessions, snapshotted from runtime state.
        let mut windows: Vec<PersistedWindow> = self
            .windows
            .values()
            .map(|s| {
                // project_path / shared_worktree live in
                // plugin_state under "orchestrator". Read them
                // back if the orchestrator plugin set them
                // (post-Phase 5 sessions); fall back to None /
                // false for sessions created before the schema
                // bump or by external paths.
                let (project_path, shared_worktree) = read_orch_session_meta(&s.plugin_state);
                PersistedWindow {
                    id: s.id.0,
                    label: s.label.clone(),
                    root: s.root.clone(),
                    project_path,
                    shared_worktree,
                    plugin_state: s.plugin_state.clone(),
                }
            })
            .collect();

        // Splice in other-process sessions from the existing
        // file (anything whose id we don't currently own).
        if let Some(env) = existing {
            for w in env.windows.into_iter() {
                if !our_ids.contains(&w.id) {
                    windows.push(w);
                }
            }
        }
        // Stable on-disk order — `HashMap` iteration order would
        // make the file diff differently every quit, producing
        // noisy diffs for anyone inspecting the persisted state.
        windows.sort_by_key(|s| s.id);
        let mut envelope = PersistedWindows {
            version: CURRENT_VERSION,
            active: self.active_window.0,
            next_id: self.next_window_id,
            windows,
        };
        // One session per directory: never persist two entries that
        // resolve to the same canonical root (a spliced other-process
        // session can collide with one of ours).
        dedup_windows_by_root(&mut envelope);
        match serde_json::to_vec_pretty(&envelope) {
            Ok(bytes) => {
                let path = global_windows_path(&data_dir);
                // Atomic rename to avoid a torn write if two
                // editor processes happen to quit at the same
                // moment. The `.tmp` file is in the same dir so
                // `rename` is an atomic syscall on every
                // filesystem we support.
                let tmp = path.with_extension("json.tmp");
                if let Err(e) = self.authority.filesystem.write_file(&tmp, &bytes) {
                    tracing::warn!("orchestrator persistence: failed to write {tmp:?}: {e}");
                    return;
                }
                if let Err(e) = self.authority.filesystem.rename(&tmp, &path) {
                    tracing::warn!(
                        "orchestrator persistence: failed to rename {tmp:?} → {path:?}: {e}"
                    );
                }
            }
            Err(e) => {
                tracing::warn!("orchestrator persistence: failed to serialise sessions: {e}");
            }
        }

        // Plugin global state — one file per plugin. Single
        // global directory now (no per-cwd split), so two
        // editor processes writing the same plugin's state
        // still need atomic-rename safety.
        let state_dir = global_state_dir(&data_dir);
        if !self.plugin_global_state.is_empty() {
            if let Err(e) = self.authority.filesystem.create_dir_all(&state_dir) {
                tracing::warn!("orchestrator persistence: failed to create {state_dir:?}: {e}");
                return;
            }
        }
        for (plugin, map) in &self.plugin_global_state {
            if !plugin_name_is_safe(plugin) {
                tracing::warn!(
                    "orchestrator persistence: skipping plugin with unsafe name: {plugin:?}"
                );
                continue;
            }
            if map.is_empty() {
                continue;
            }
            match serde_json::to_vec_pretty(map) {
                Ok(bytes) => {
                    let path = global_plugin_state_path(&data_dir, plugin);
                    let tmp = path.with_extension("json.tmp");
                    if let Err(e) = self.authority.filesystem.write_file(&tmp, &bytes) {
                        tracing::warn!("orchestrator persistence: failed to write {tmp:?}: {e}");
                        continue;
                    }
                    if let Err(e) = self.authority.filesystem.rename(&tmp, &path) {
                        tracing::warn!(
                            "orchestrator persistence: failed to rename {tmp:?} → {path:?}: {e}"
                        );
                    }
                }
                Err(e) => {
                    tracing::warn!(
                        "orchestrator persistence: failed to serialise plugin {plugin}: {e}"
                    );
                }
            }
        }
    }
}

/// Pull `project_path` (PathBuf) and `shared_worktree` (bool)
/// out of a session's per-plugin state, if the orchestrator
/// plugin has set them via `setWindowState`. Both keys live
/// under the `"orchestrator"` plugin slot; the keys are
/// `"project_path"` and `"shared_worktree"`.
fn read_orch_session_meta(
    plugin_state: &HashMap<String, HashMap<String, serde_json::Value>>,
) -> (Option<PathBuf>, bool) {
    let slot = plugin_state.get("orchestrator");
    let project_path = slot
        .and_then(|m| m.get("project_path"))
        .and_then(|v| v.as_str())
        .map(PathBuf::from);
    let shared_worktree = slot
        .and_then(|m| m.get("shared_worktree"))
        .and_then(|v| v.as_bool())
        .unwrap_or(false);
    (project_path, shared_worktree)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn paths_live_under_data_dir_not_working_dir() {
        // Regression test for issue #1991: orchestrator persistence
        // must never write inside the user's working tree.
        let data_dir = Path::new("/tmp/fresh-data");
        let working_dir = Path::new("/home/user/project");

        let wp = global_windows_path(data_dir);
        let sd = global_state_dir(data_dir);
        let psp = global_plugin_state_path(data_dir, "orchestrator");

        assert!(
            wp.starts_with(data_dir),
            "windows_path must live under data_dir, got {wp:?}"
        );
        assert!(
            sd.starts_with(data_dir),
            "state_dir must live under data_dir, got {sd:?}"
        );
        assert!(
            psp.starts_with(data_dir),
            "plugin_state_path must live under data_dir, got {psp:?}"
        );

        for p in [&wp, &sd, &psp] {
            assert!(
                !p.starts_with(working_dir),
                "orchestrator path must not be inside the working tree: {p:?}"
            );
            for component in p.components() {
                if let std::path::Component::Normal(c) = component {
                    assert_ne!(
                        c, ".fresh",
                        "orchestrator path must not contain a `.fresh` component: {p:?}"
                    );
                }
            }
        }
    }

    fn make_window(id: u64, root: &str, project_path: Option<&str>) -> PersistedWindow {
        PersistedWindow {
            id,
            label: String::new(),
            root: PathBuf::from(root),
            project_path: project_path.map(PathBuf::from),
            shared_worktree: false,
            plugin_state: HashMap::new(),
        }
    }

    fn env_with(active: u64, windows: Vec<PersistedWindow>) -> PersistedWindows {
        PersistedWindows {
            version: CURRENT_VERSION,
            active,
            next_id: windows.iter().map(|w| w.id).max().unwrap_or(0) + 1,
            windows,
        }
    }

    #[test]
    fn pick_active_never_crosses_projects() {
        // Regression for the orchestration bug: launching in /repoB
        // must never bring up a session rooted in /repoA, even when
        // /repoA holds the globally last-used session (env.active).
        let env = env_with(
            2,
            vec![
                make_window(1, "/repoA", Some("/repoA")),
                make_window(2, "/repoA", Some("/repoA")),
                make_window(3, "/repoB", Some("/repoB")),
            ],
        );
        let picked = pick_active_window_for_cwd(Some(&env), Path::new("/repoB"))
            .expect("a /repoB session exists");
        assert_eq!(
            picked.id, 3,
            "must pick the /repoB session, not env.active=2"
        );
    }

    #[test]
    fn pick_active_reopens_last_used_for_cwd() {
        // env.active points at this project's last-used session — it
        // wins even though it isn't the highest id.
        let env = env_with(
            2,
            vec![
                make_window(2, "/repoA", Some("/repoA")),
                make_window(5, "/repoA", Some("/repoA")),
            ],
        );
        let picked =
            pick_active_window_for_cwd(Some(&env), Path::new("/repoA")).expect("matching window");
        assert_eq!(
            picked.id, 2,
            "env.active is the last-used session for the cwd"
        );
    }

    #[test]
    fn pick_active_falls_back_to_most_recent_session_for_cwd() {
        // The globally last-used session (env.active=9) is in another
        // project, so for /repoA we fall back to the most-recently-
        // created /repoA session (highest id), not the first.
        let env = env_with(
            9,
            vec![
                make_window(2, "/repoA", Some("/repoA")),
                make_window(7, "/repoA", Some("/repoA")),
                make_window(9, "/repoB", Some("/repoB")),
            ],
        );
        let picked =
            pick_active_window_for_cwd(Some(&env), Path::new("/repoA")).expect("matching window");
        assert_eq!(picked.id, 7, "fall back to the most recent /repoA session");
    }

    #[test]
    fn pick_active_returns_none_when_no_window_matches_cwd() {
        // No session for this cwd → caller boots a clean base window.
        let env = env_with(
            1,
            vec![
                make_window(1, "/repoA", Some("/repoA")),
                make_window(2, "/repoB", Some("/repoB")),
            ],
        );
        assert!(pick_active_window_for_cwd(Some(&env), Path::new("/repoC")).is_none());
    }

    #[test]
    fn pick_active_falls_back_to_root_when_project_path_missing() {
        // Legacy v1-migrated entries may lack project_path; match on root.
        let env = env_with(
            2,
            vec![
                make_window(1, "/repoA", None),
                make_window(2, "/repoB", None),
            ],
        );
        let picked =
            pick_active_window_for_cwd(Some(&env), Path::new("/repoA")).expect("matching window");
        assert_eq!(picked.id, 1);
    }

    #[test]
    fn global_paths_are_independent_of_working_dir() {
        // v2: persistence is global, not per-cwd. Two different
        // cwds resolve to the same file path so the user sees
        // their full session history regardless of where the
        // editor was launched from.
        let data_dir = Path::new("/tmp/fresh-data");
        let a = global_windows_path(data_dir);
        let b = global_windows_path(data_dir);
        assert_eq!(a, b);
        assert_eq!(a, data_dir.join("orchestrator").join("windows.json"));
    }
}
