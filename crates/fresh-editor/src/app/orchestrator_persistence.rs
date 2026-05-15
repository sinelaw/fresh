//! Cross-restart persistence for Orchestrator sessions and
//! plugin global state.
//!
//! On quit, `save_orchestrator_state` writes:
//!   - `<data_dir>/orchestrator/<encoded_working_dir>/windows.json`
//!     — list of sessions (id, label, root, per-session
//!     plugin_state) plus the last-active session id and the
//!     next id to allocate so id-based references on disk stay
//!     stable across restarts.
//!   - `<data_dir>/orchestrator/<encoded_working_dir>/state/<plugin>.json`
//!     — one file per plugin holding its
//!     `editor.setGlobalState(...)` map.
//!
//! The state lives under the platform data directory
//! (`$XDG_DATA_HOME/fresh/` on Linux), keyed by the working
//! directory using the same encoding as the `workspace` module.
//! This mirrors how other per-project editor state is stored
//! and keeps the user's working tree free of stray dotfiles
//! (issue #1991).
//!
//! On startup, [`read_persisted_windows_env`] +
//! [`read_persisted_plugin_state`] are called from
//! `Editor::with_options` (see `editor_init.rs`) *before* the
//! editor struct is built. The factory uses the parsed envelope
//! to pick the active window's id and root (so the spawned LSP
//! targets the right project), to attach the seed buffer +
//! split layout to the active window directly, and to populate
//! `plugin_global_state` so plugins reading `getGlobalState`
//! during their on-load handler see the previous run's values.
//! All non-active persisted windows come back as inert shells
//! (no splits, no LSP); first dive into one re-warms it on
//! demand exactly like a freshly-`createWindow`-ed session.
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
    /// Per-session plugin state (the same map kept in
    /// `Session.plugin_state`). Empty plugins / empty keys are
    /// stripped on save.
    #[serde(default)]
    pub(crate) plugin_state: HashMap<String, HashMap<String, serde_json::Value>>,
}

/// Top-level shape of `windows.json`.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct PersistedWindows {
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

/// Read `windows.json` for `working_dir` and return the parsed
/// envelope. Returns `None` when the file doesn't exist or fails
/// to parse — those are not error cases at the editor level (a
/// missing or corrupted file just means "no persisted state").
///
/// Pure file IO + JSON parse. Used by the editor factory to
/// decide how to build the initial windows map before any `Editor`
/// instance exists.
pub(crate) fn read_persisted_windows_env(
    filesystem: &(dyn crate::model::filesystem::FileSystem + Send + Sync),
    data_dir: &Path,
    working_dir: &Path,
) -> Option<PersistedWindows> {
    let windows_p = windows_path(data_dir, working_dir);
    if !filesystem.exists(&windows_p) {
        return None;
    }
    match filesystem.read_file(&windows_p) {
        Ok(bytes) => match serde_json::from_slice::<PersistedWindows>(&bytes) {
            Ok(env) => Some(env),
            Err(e) => {
                tracing::warn!("orchestrator persistence: failed to parse {windows_p:?}: {e}");
                None
            }
        },
        Err(e) => {
            tracing::warn!("orchestrator persistence: failed to read {windows_p:?}: {e}");
            None
        }
    }
}

/// Read every `state/<plugin>.json` for `working_dir` into a flat
/// `plugin → key → value` map. Skips files with unsafe names,
/// non-JSON extensions, parse errors, and empty maps. Same
/// motivations as [`read_persisted_windows_env`] — used by the
/// editor factory pre-construction.
pub(crate) fn read_persisted_plugin_state(
    filesystem: &(dyn crate::model::filesystem::FileSystem + Send + Sync),
    data_dir: &Path,
    working_dir: &Path,
) -> HashMap<String, HashMap<String, serde_json::Value>> {
    let mut out: HashMap<String, HashMap<String, serde_json::Value>> = HashMap::new();
    let state_dir = state_dir(data_dir, working_dir);
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

/// Per-working-dir orchestrator directory under the platform
/// data dir. The working dir is encoded with the same scheme
/// as `workspace::encode_path_for_filename`, so the per-project
/// state lives at a stable, collision-free location regardless
/// of where the user invokes the editor from. See issue #1991
/// for why this is no longer rooted at `<working_dir>/.fresh`.
fn orchestrator_dir(data_dir: &Path, working_dir: &Path) -> PathBuf {
    let encoded = crate::workspace::encode_path_for_filename(working_dir);
    data_dir.join("orchestrator").join(encoded)
}

fn windows_path(data_dir: &Path, working_dir: &Path) -> PathBuf {
    orchestrator_dir(data_dir, working_dir).join("windows.json")
}

fn state_dir(data_dir: &Path, working_dir: &Path) -> PathBuf {
    orchestrator_dir(data_dir, working_dir).join("state")
}

fn plugin_state_path(data_dir: &Path, working_dir: &Path, plugin: &str) -> PathBuf {
    // Plugin names are short identifiers (`orchestrator`,
    // `live_grep`, …) so no escaping is needed for typical
    // input. Reject anything that would escape the state dir to
    // avoid `../`-style traversal in case a plugin picks a
    // pathological name.
    state_dir(data_dir, working_dir).join(format!("{plugin}.json"))
}

fn plugin_name_is_safe(name: &str) -> bool {
    !name.is_empty()
        && name
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.')
        && !name.starts_with('.')
}

impl Editor {
    /// Persist `sessions` + `plugin_global_state` to disk. Best-
    /// effort: filesystem errors are logged at WARN and swallowed
    /// so a transient permission glitch doesn't block quit.
    pub fn save_orchestrator_state(&self) {
        let working_dir = self.working_dir().to_path_buf();
        let data_dir = self.dir_context.data_dir.clone();
        let orchestrator_dir = orchestrator_dir(&data_dir, &working_dir);
        if let Err(e) = self.authority.filesystem.create_dir_all(&orchestrator_dir) {
            tracing::warn!("orchestrator persistence: failed to create {orchestrator_dir:?}: {e}");
            return;
        }

        // Windows.
        let mut windows: Vec<PersistedWindow> = self
            .windows
            .values()
            .map(|s| PersistedWindow {
                id: s.id.0,
                label: s.label.clone(),
                root: s.root.clone(),
                plugin_state: s.plugin_state.clone(),
            })
            .collect();
        // Stable on-disk order — `HashMap` iteration order would
        // make the file diff differently every quit, producing
        // noisy diffs for anyone inspecting the persisted state.
        windows.sort_by_key(|s| s.id);
        let envelope = PersistedWindows {
            active: self.active_window.0,
            next_id: self.next_window_id,
            windows,
        };
        match serde_json::to_vec_pretty(&envelope) {
            Ok(bytes) => {
                let path = windows_path(&data_dir, &working_dir);
                if let Err(e) = self.authority.filesystem.write_file(&path, &bytes) {
                    tracing::warn!("orchestrator persistence: failed to write {path:?}: {e}");
                }
            }
            Err(e) => {
                tracing::warn!("orchestrator persistence: failed to serialise sessions: {e}");
            }
        }

        // Plugin global state — one file per plugin so concurrent
        // editors writing different plugins don't clobber each
        // other (a future feature; today single-process).
        let state_dir = state_dir(&data_dir, &working_dir);
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
                    let path = plugin_state_path(&data_dir, &working_dir, plugin);
                    if let Err(e) = self.authority.filesystem.write_file(&path, &bytes) {
                        tracing::warn!("orchestrator persistence: failed to write {path:?}: {e}");
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn paths_live_under_data_dir_not_working_dir() {
        // Regression test for issue #1991: orchestrator persistence
        // must never write inside the user's working tree.
        let data_dir = Path::new("/tmp/fresh-data");
        let working_dir = Path::new("/home/user/project");

        let wp = windows_path(data_dir, working_dir);
        let sd = state_dir(data_dir, working_dir);
        let psp = plugin_state_path(data_dir, working_dir, "orchestrator");

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

    #[test]
    fn paths_are_keyed_by_working_dir() {
        // Two distinct working dirs must map to two distinct
        // orchestrator subtrees, otherwise different projects
        // would clobber each other's state.
        let data_dir = Path::new("/tmp/fresh-data");
        let a = windows_path(data_dir, Path::new("/home/user/proj-a"));
        let b = windows_path(data_dir, Path::new("/home/user/proj-b"));
        assert_ne!(a, b);
        // Same working dir must map to same path (idempotent
        // encoding — relied on so a quit + restart sees the
        // same file).
        let a_again = windows_path(data_dir, Path::new("/home/user/proj-a"));
        assert_eq!(a, a_again);
    }
}
