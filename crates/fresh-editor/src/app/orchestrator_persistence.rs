//! Cross-restart persistence for Orchestrator sessions and
//! plugin global state.
//!
//! ## The session registry is the directory set
//!
//! There is no central session-list file. A session *is* a
//! directory (one session per dir), and the registry is the
//! per-dir workspace cache:
//!
//!   - `<data_dir>/workspaces/<encoded-root>.json` — one file per
//!     directory ever opened. Each carries that window's identity
//!     (`label`, `session_plugin_state`) plus its buffer/split
//!     layout. [`discover_sessions`] scans this directory at boot,
//!     garbage-collects entries whose directory no longer exists,
//!     and returns one [`PersistedWindow`] per survivor (ids
//!     assigned by sorted canonical root for run-to-run stability).
//!
//!   - `<data_dir>/orchestrator/state/<plugin>.json` — editor-wide
//!     plugin global state, one file per plugin (not per-project).
//!
//! `PersistedWindow` / `PersistedWindows` are now in-memory shapes
//! produced by discovery (and still the parse target of a legacy
//! `windows.json` during migration), not an on-disk schema.
//!
//! ## Migration
//!
//! Older builds kept a central `<data_dir>/orchestrator/windows.json`
//! (and, before that, per-cwd `<data>/orchestrator/<encoded_cwd>/
//! windows.json`). On first read, [`migrate_legacy_windows`] folds any
//! per-cwd files into a single windows.json, then
//! [`migrate_windows_json_into_workspaces`] backfills its
//! `label` / per-session plugin state into the matching per-dir
//! workspace files and retires the file to `windows.json.retired.bak`.
//! After that the workspace cache is the sole registry.
//!
//! State lives under the platform data dir (`$XDG_DATA_HOME/fresh/`),
//! never the working tree (issue #1991).
//!
//! ## Startup
//!
//! [`read_persisted_windows_env`] + [`read_persisted_plugin_state`]
//! run from `editor_init` before the editor struct exists. The
//! foreground window is the one whose `root` matches the launch cwd
//! ([`pick_active_window_for_cwd`]) — authoritatively, regardless of
//! which session was last used; if none matches, a clean window is
//! booted at the cwd. Every other discovered session comes back as an
//! inert shell (no splits/LSP) restored lazily on first dive/preview.
//! The "warm" layout is intentionally not persisted across restarts —
//! re-warming on first dive is fast enough.

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
    /// How to rebuild/reconnect this session's backend on restore (read
    /// from the workspace file's `authority_spec`). `Local` for an ordinary
    /// host session. Threaded into the window at construction so an
    /// unmaterialized background session still knows its backend (and a
    /// later save doesn't clobber it back to local).
    #[serde(default, skip_serializing_if = "is_local_authority_spec")]
    pub(crate) authority_spec: crate::services::authority::SessionAuthoritySpec,
}

fn is_local_authority_spec(spec: &crate::services::authority::SessionAuthoritySpec) -> bool {
    matches!(
        spec,
        crate::services::authority::SessionAuthoritySpec::Local
    )
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
    // Legacy v1 (per-cwd) → windows.json, if any survive. windows.json
    // is itself legacy now; the next step folds it into the per-dir
    // workspace files and retires it.
    let global_p = global_windows_path(data_dir);
    if !filesystem.exists(&global_p) {
        migrate_legacy_windows(filesystem, data_dir);
    }
    migrate_windows_json_into_workspaces(filesystem, data_dir);

    // The per-dir workspace cache is the session registry now: one
    // session per directory, discovered from disk. GC dead entries and
    // build a window per survivor.
    let windows = discover_sessions(filesystem, data_dir);
    if windows.is_empty() {
        return None;
    }
    let next_id = windows.iter().map(|w| w.id).max().unwrap_or(0) + 1;
    // One session per directory is enforced upstream by the workspace
    // cache itself: `get_workspace_path` keys each file on the
    // canonical root, so discovery yields at most one window per
    // canonical dir. No post-hoc dedup is needed.
    //
    // `active` is decided downstream by the launch cwd
    // (`pick_active_window_for_cwd`); 0 means "no stored hint", so the
    // cwd-match branch governs which session is foregrounded.
    Some(PersistedWindows {
        version: CURRENT_VERSION,
        active: 0,
        next_id,
        windows,
    })
}

fn workspaces_dir(data_dir: &Path) -> PathBuf {
    data_dir.join("workspaces")
}

/// Per-dir workspace file path for `root` under `data_dir` — mirrors
/// `crate::workspace::get_workspace_path` but honours the passed data
/// dir rather than the process-global one.
fn workspace_file_for(data_dir: &Path, root: &Path) -> PathBuf {
    let filename = format!(
        "{}.json",
        crate::workspace::encode_path_for_filename(&canonical_key(root))
    );
    workspaces_dir(data_dir).join(filename)
}

fn basename_label(root: &Path) -> String {
    root.file_name()
        .and_then(|s| s.to_str())
        .map(|s| s.to_string())
        .unwrap_or_else(|| root.to_string_lossy().into_owned())
}

/// One session per existing directory: scan the workspace-file cache,
/// garbage-collect entries whose directory no longer exists, and return
/// one `PersistedWindow` per survivor. Ids are assigned by sorted
/// canonical root so they stay stable across runs for a stable dir set.
fn discover_sessions(
    filesystem: &(dyn crate::model::filesystem::FileSystem + Send + Sync),
    data_dir: &Path,
) -> Vec<PersistedWindow> {
    type SessionState = HashMap<String, HashMap<String, serde_json::Value>>;
    let dir = workspaces_dir(data_dir);
    tracing::debug!(dir = %dir.display(), "discover_sessions: read_dir");
    let entries = match filesystem.read_dir(&dir) {
        Ok(e) => e,
        Err(_) => return Vec::new(),
    };
    tracing::debug!(
        count = entries.len(),
        "discover_sessions: read_dir returned"
    );
    let mut found: Vec<(
        PathBuf,
        String,
        SessionState,
        crate::services::authority::SessionAuthoritySpec,
    )> = Vec::new();
    for entry in entries {
        let p = &entry.path;
        // Only real workspace files. A torn `*.json.tmp` write or a
        // `*.retired.bak` already fails the `.json` suffix test.
        if !entry.name.ends_with(".json") {
            continue;
        }
        tracing::debug!(path = %p.display(), "discover_sessions: read_file");
        let Ok(bytes) = filesystem.read_file(p) else {
            continue;
        };
        let Ok(val) = serde_json::from_slice::<serde_json::Value>(&bytes) else {
            continue;
        };
        let Some(root) = val.get("working_dir").and_then(|v| v.as_str()) else {
            continue;
        };
        let root = PathBuf::from(root);
        // The session's backend spec (how to reconnect on restore). Absent /
        // unparseable → `Local`, so a malformed entry degrades safely. Read
        // *before* the GC check: a remote session's `root` lives on the
        // remote host, so it can't be validated against the local filesystem.
        let authority_spec: crate::services::authority::SessionAuthoritySpec = val
            .get("authority_spec")
            .and_then(|v| serde_json::from_value(v.clone()).ok())
            .unwrap_or_default();
        // GC only local sessions, and only on a *definitive* answer that the
        // root is unusable: `NotFound` (the directory is gone) or `Ok(false)`
        // (the path was replaced by a non-dir). Drop the stale cache file then
        // — best-effort, a failed delete just leaves a harmless file to retry
        // next boot. Any *other* `Err` (permission, IO, an unreachable
        // remote/unmounted FS) is ambiguous but recoverable, so keep the file
        // rather than irreversibly losing the session.
        //
        // Remote sessions (SSH / kube) are *never* GC'd against the local
        // filesystem: their `root` is a path on the remote host that the local
        // `filesystem` here can't see, so `is_dir` would answer `Ok(false)`
        // and silently delete every remote session's workspace file on the
        // next boot — the session would vanish from the Orchestrator dock
        // after a restart. Whether the remote dir still exists is only knowable
        // after reconnecting, so we keep the entry and let restore decide.
        if !authority_spec.is_remote() {
            match filesystem.is_dir(&root) {
                Ok(true) => {}
                Ok(false) => {
                    let _ = filesystem.remove_file(p).ok();
                    continue;
                }
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                    let _ = filesystem.remove_file(p).ok();
                    continue;
                }
                Err(_) => continue,
            }
        }
        let label = val
            .get("label")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
            .unwrap_or_else(|| basename_label(&root));
        let plugin_state: SessionState = val
            .get("session_plugin_state")
            .and_then(|v| serde_json::from_value(v.clone()).ok())
            .unwrap_or_default();
        found.push((root, label, plugin_state, authority_spec));
    }
    found.sort_by(|a, b| canonical_key(&a.0).cmp(&canonical_key(&b.0)));
    found
        .into_iter()
        .enumerate()
        .map(|(i, (root, label, plugin_state, authority_spec))| {
            let (project_path, shared_worktree) = read_orch_session_meta(&plugin_state);
            PersistedWindow {
                id: (i as u64) + 1,
                label,
                root,
                project_path,
                shared_worktree,
                authority_spec,
                plugin_state,
            }
        })
        .collect()
}

/// Fold legacy `windows.json` session metadata (label + per-session
/// plugin state) into the per-dir workspace files, then retire the
/// file. After this the workspace cache is the sole registry. Only
/// existing workspace files are backfilled; entries with no workspace
/// file are dropped (they carried no buffer content to restore). No-op
/// once `windows.json` is gone.
fn migrate_windows_json_into_workspaces(
    filesystem: &(dyn crate::model::filesystem::FileSystem + Send + Sync),
    data_dir: &Path,
) {
    let global_p = global_windows_path(data_dir);
    if !filesystem.exists(&global_p) {
        return;
    }
    let Ok(bytes) = filesystem.read_file(&global_p) else {
        return;
    };
    let Ok(env) = serde_json::from_slice::<PersistedWindows>(&bytes) else {
        return; // leave an unparseable file in place rather than lose it
    };
    for w in &env.windows {
        let ws_path = workspace_file_for(data_dir, &w.root);
        if !filesystem.exists(&ws_path) {
            continue;
        }
        let Ok(wbytes) = filesystem.read_file(&ws_path) else {
            continue;
        };
        let Ok(mut val) = serde_json::from_slice::<serde_json::Value>(&wbytes) else {
            continue;
        };
        if let Some(obj) = val.as_object_mut() {
            obj.entry("label")
                .or_insert_with(|| serde_json::Value::String(w.label.clone()));
            if !obj.contains_key("session_plugin_state") && !w.plugin_state.is_empty() {
                if let Ok(ps) = serde_json::to_value(&w.plugin_state) {
                    obj.insert("session_plugin_state".into(), ps);
                }
            }
        }
        if let Ok(out) = serde_json::to_vec_pretty(&val) {
            // Best-effort backfill: on failure the workspace keeps its pre-migration content.
            let _ = filesystem.write_file(&ws_path, &out).ok();
        }
    }
    // Retire windows.json (keep a .bak so a downgrade isn't one-way).
    let bak = global_p.with_extension("json.retired.bak");
    if filesystem.rename(&global_p, &bak).is_err() {
        // Best-effort: if delete also fails the file stays and migration reruns (idempotent).
        let _ = filesystem.remove_file(&global_p).ok();
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
        if let Err(e) = self.authority().filesystem.create_dir_all(&orch_dir) {
            tracing::warn!("orchestrator persistence: failed to create {orch_dir:?}: {e}");
            return;
        }

        // Sessions are no longer written to a central windows.json:
        // each window's identity (label, per-session plugin_state) is
        // persisted in its own per-dir workspace file by
        // `save_all_windows_workspaces` (called just before this on
        // quit), and the session list is rediscovered from those files
        // at boot. Only editor-global plugin state is written here.

        // Plugin global state — one file per plugin. Single
        // global directory now (no per-cwd split), so two
        // editor processes writing the same plugin's state
        // still need atomic-rename safety.
        let state_dir = global_state_dir(&data_dir);
        if !self.plugin_global_state.is_empty() {
            if let Err(e) = self.authority().filesystem.create_dir_all(&state_dir) {
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
                    if let Err(e) = self.authority().filesystem.write_file(&tmp, &bytes) {
                        tracing::warn!("orchestrator persistence: failed to write {tmp:?}: {e}");
                        continue;
                    }
                    if let Err(e) = self.authority().filesystem.rename(&tmp, &path) {
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
            authority_spec: Default::default(),
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

    #[test]
    fn discover_gcs_missing_dirs_and_yields_one_session_per_existing_dir() {
        use crate::model::filesystem::StdFileSystem;
        let data = tempfile::tempdir().unwrap();
        let data_dir = data.path();
        let ws_dir = workspaces_dir(data_dir);
        std::fs::create_dir_all(&ws_dir).unwrap();

        // A workspace file for an existing dir...
        let live = tempfile::tempdir().unwrap();
        let live_root = live.path().canonicalize().unwrap();
        let live_file = ws_dir.join("live.json");
        std::fs::write(
            &live_file,
            serde_json::to_vec(&serde_json::json!({
                "working_dir": live_root, "label": "live-session",
            }))
            .unwrap(),
        )
        .unwrap();

        // ...and one for a directory that does not exist.
        let dead_file = ws_dir.join("dead.json");
        std::fs::write(
            &dead_file,
            serde_json::to_vec(&serde_json::json!({
                "working_dir": "/no/such/dir/anywhere", "label": "dead",
            }))
            .unwrap(),
        )
        .unwrap();

        let fs = StdFileSystem;
        let sessions = discover_sessions(&fs, data_dir);

        assert_eq!(sessions.len(), 1, "only the existing dir yields a session");
        assert_eq!(sessions[0].root, live_root);
        assert_eq!(sessions[0].label, "live-session");
        assert!(!dead_file.exists(), "the dead dir's cache file was GC'd");
        assert!(live_file.exists(), "the live cache file is kept");
    }

    #[test]
    fn discover_reads_authority_spec_so_remote_sessions_arent_lost() {
        // A session that was running on a remote backend persists an
        // `authority_spec` in its workspace file; discovery must surface it
        // (so restore can reconnect rather than degrade to local). A file
        // without the field reads back as `Local` — back-compat for sessions
        // written before per-session backends existed.
        use crate::model::filesystem::StdFileSystem;
        use crate::services::authority::{
            AuthorityPayload, FilesystemSpec, SessionAuthoritySpec, SpawnerSpec,
            TerminalWrapperSpec,
        };
        let data = tempfile::tempdir().unwrap();
        let data_dir = data.path();
        let ws_dir = workspaces_dir(data_dir);
        std::fs::create_dir_all(&ws_dir).unwrap();

        let remote_root = tempfile::tempdir().unwrap();
        let remote_root = remote_root.path().canonicalize().unwrap();
        let spec = SessionAuthoritySpec::Plugin(AuthorityPayload {
            filesystem: FilesystemSpec::Local,
            spawner: SpawnerSpec::DockerExec {
                container_id: "abc123".into(),
                user: Some("vscode".into()),
                workspace: Some("/workspaces/proj".into()),
                env: Vec::new(),
            },
            terminal_wrapper: TerminalWrapperSpec::HostShell,
            display_label: "Container:abc123".into(),
            path_translation: None,
        });
        std::fs::write(
            ws_dir.join("remote.json"),
            serde_json::to_vec(&serde_json::json!({
                "working_dir": remote_root,
                "label": "remote-session",
                "authority_spec": spec,
            }))
            .unwrap(),
        )
        .unwrap();

        // A plain local session with no `authority_spec` field at all.
        let local_root = tempfile::tempdir().unwrap();
        let local_root = local_root.path().canonicalize().unwrap();
        std::fs::write(
            ws_dir.join("local.json"),
            serde_json::to_vec(&serde_json::json!({
                "working_dir": local_root, "label": "local-session",
            }))
            .unwrap(),
        )
        .unwrap();

        let fs = StdFileSystem;
        let sessions = discover_sessions(&fs, data_dir);

        let remote = sessions
            .iter()
            .find(|s| s.label == "remote-session")
            .expect("remote session discovered");
        assert_eq!(
            remote.authority_spec, spec,
            "the remote backend spec round-trips through discovery"
        );
        let local = sessions
            .iter()
            .find(|s| s.label == "local-session")
            .expect("local session discovered");
        assert_eq!(
            local.authority_spec,
            SessionAuthoritySpec::Local,
            "a session with no persisted spec reads back as Local"
        );
    }

    #[test]
    fn discover_keeps_remote_session_whose_root_is_absent_locally() {
        // Regression: a running SSH session persists a `working_dir` that is a
        // path on the *remote* host — it does not (and need not) exist on the
        // local filesystem. Discovery runs the GC check against the local
        // filesystem, so before the fix `is_dir` answered `Ok(false)` and the
        // remote session's workspace file was deleted on the next boot,
        // dropping it from the Orchestrator dock. A remote session must survive
        // discovery even though its root is absent locally.
        use crate::model::filesystem::StdFileSystem;
        use crate::services::authority::{
            RemoteAgentSpec, RemoteTransportSpec, SessionAuthoritySpec,
        };
        let data = tempfile::tempdir().unwrap();
        let data_dir = data.path();
        let ws_dir = workspaces_dir(data_dir);
        std::fs::create_dir_all(&ws_dir).unwrap();

        // A path that does not exist on the local filesystem — it lives on the
        // remote host the SSH session is rooted at.
        let remote_only_root = "/home/remote-user/project-on-remote-host";
        assert!(
            !Path::new(remote_only_root).exists(),
            "test precondition: the remote root must not exist locally"
        );
        let spec = SessionAuthoritySpec::RemoteAgent(RemoteAgentSpec {
            transport: RemoteTransportSpec::Ssh {
                user: Some("remote-user".into()),
                host: "example.com".into(),
                port: None,
                identity_file: None,
                remote_path: Some(remote_only_root.into()),
                extra_args: Vec::new(),
            },
            base_env: Vec::new(),
            window: true,
            label: Some("ssh-session".into()),
            command: None,
        });
        std::fs::write(
            ws_dir.join("ssh.json"),
            serde_json::to_vec(&serde_json::json!({
                "working_dir": remote_only_root,
                "label": "ssh-session",
                "authority_spec": spec,
            }))
            .unwrap(),
        )
        .unwrap();

        let fs = StdFileSystem;
        let sessions = discover_sessions(&fs, data_dir);

        let ssh = sessions
            .iter()
            .find(|s| s.label == "ssh-session")
            .expect("the SSH session survives discovery despite a remote-only root");
        assert_eq!(ssh.authority_spec, spec);
        assert!(
            ws_dir.join("ssh.json").exists(),
            "the remote session's workspace file must not be GC'd"
        );
    }

    #[test]
    fn migrate_folds_windows_json_into_workspace_files_and_retires_it() {
        use crate::model::filesystem::StdFileSystem;
        let data = tempfile::tempdir().unwrap();
        let data_dir = data.path();
        let proj = tempfile::tempdir().unwrap();
        let proj_root = proj.path().canonicalize().unwrap();

        // An existing per-dir workspace file with no label yet.
        let ws_path = workspace_file_for(data_dir, &proj_root);
        std::fs::create_dir_all(ws_path.parent().unwrap()).unwrap();
        std::fs::write(
            &ws_path,
            serde_json::to_vec(&serde_json::json!({ "working_dir": proj_root })).unwrap(),
        )
        .unwrap();

        // A legacy windows.json naming that session with a label.
        let global_p = global_windows_path(data_dir);
        std::fs::create_dir_all(global_p.parent().unwrap()).unwrap();
        std::fs::write(
            &global_p,
            serde_json::to_vec(&serde_json::json!({
                "version": 2, "active": 1, "next_id": 2,
                "windows": [ { "id": 1, "label": "from-windows-json", "root": proj_root } ],
            }))
            .unwrap(),
        )
        .unwrap();

        let fs = StdFileSystem;
        migrate_windows_json_into_workspaces(&fs, data_dir);

        assert!(!global_p.exists(), "windows.json is retired");
        assert!(
            global_p.with_extension("json.retired.bak").exists(),
            "a .retired.bak is kept"
        );
        let val: serde_json::Value =
            serde_json::from_slice(&std::fs::read(&ws_path).unwrap()).unwrap();
        assert_eq!(
            val.get("label").and_then(|v| v.as_str()),
            Some("from-windows-json"),
            "the label was folded into the per-dir workspace file"
        );
    }
}
