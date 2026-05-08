//! Cross-restart persistence for Conductor sessions and
//! plugin global state.
//!
//! On quit, `save_conductor_state` writes:
//!   - `<working_dir>/.fresh/windows.json` — list of sessions
//!     (id, label, root, per-session plugin_state) plus the
//!     last-active session id and the next id to allocate so
//!     id-based references on disk stay stable across restarts.
//!   - `<working_dir>/.fresh/state/<plugin>.json` — one file per
//!     plugin holding its `editor.setGlobalState(...)` map.
//!
//! On startup, `load_conductor_state` (called from `editor_init`)
//! reads them back. Sessions are reconstituted as inert
//! shells — no warm split tree, no warm LSP — exactly like a
//! freshly-`createWindow`-ed session, so the user sees the same
//! list in `Conductor: Open` and can dive into any of them.
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

use fresh_core::WindowId;

use super::window::Window;
use super::Editor;

/// One session as it appears on disk.
#[derive(Serialize, Deserialize, Debug, Clone)]
struct PersistedWindow {
    id: u64,
    label: String,
    root: PathBuf,
    /// Per-session plugin state (the same map kept in
    /// `Session.plugin_state`). Empty plugins / empty keys are
    /// stripped on save.
    #[serde(default)]
    plugin_state: HashMap<String, HashMap<String, serde_json::Value>>,
}

/// Top-level shape of `.fresh/windows.json`.
#[derive(Serialize, Deserialize, Debug, Clone)]
struct PersistedWindows {
    /// Last active session id at quit time. The loader makes
    /// this session the active one again. If missing or
    /// dangling, falls back to the base session.
    active: u64,
    /// `next_window_id` at quit time — preserved so newly
    /// created sessions after restart don't collide with ids
    /// the user might still see in plugin state.
    next_id: u64,
    windows: Vec<PersistedWindow>,
}

fn windows_path(working_dir: &Path) -> PathBuf {
    working_dir.join(".fresh").join("windows.json")
}

fn state_dir(working_dir: &Path) -> PathBuf {
    working_dir.join(".fresh").join("state")
}

fn plugin_state_path(working_dir: &Path, plugin: &str) -> PathBuf {
    // Plugin names are short identifiers (`conductor`,
    // `live_grep`, …) so no escaping is needed for typical
    // input. Reject anything that would escape the state dir to
    // avoid `../`-style traversal in case a plugin picks a
    // pathological name.
    state_dir(working_dir).join(format!("{plugin}.json"))
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
    /// so a transient `.fresh/` permission glitch doesn't block
    /// quit.
    pub fn save_conductor_state(&self) {
        let working_dir = self.working_dir().to_path_buf();
        let fresh_dir = working_dir.join(".fresh");
        if let Err(e) = self.authority.filesystem.create_dir_all(&fresh_dir) {
            tracing::warn!("conductor persistence: failed to create {fresh_dir:?}: {e}");
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
        // make the file diff differently every quit, which is
        // ugly for users who keep `.fresh/` in version control.
        windows.sort_by_key(|s| s.id);
        let envelope = PersistedWindows {
            active: self.active_window.0,
            next_id: self.next_window_id,
            windows,
        };
        match serde_json::to_vec_pretty(&envelope) {
            Ok(bytes) => {
                let path = windows_path(&working_dir);
                if let Err(e) = self.authority.filesystem.write_file(&path, &bytes) {
                    tracing::warn!("conductor persistence: failed to write {path:?}: {e}");
                }
            }
            Err(e) => {
                tracing::warn!("conductor persistence: failed to serialise sessions: {e}");
            }
        }

        // Plugin global state — one file per plugin so concurrent
        // editors writing different plugins don't clobber each
        // other (a future feature; today single-process).
        let state_dir = state_dir(&working_dir);
        if !self.plugin_global_state.is_empty() {
            if let Err(e) = self.authority.filesystem.create_dir_all(&state_dir) {
                tracing::warn!("conductor persistence: failed to create {state_dir:?}: {e}");
                return;
            }
        }
        for (plugin, map) in &self.plugin_global_state {
            if !plugin_name_is_safe(plugin) {
                tracing::warn!(
                    "conductor persistence: skipping plugin with unsafe name: {plugin:?}"
                );
                continue;
            }
            if map.is_empty() {
                continue;
            }
            match serde_json::to_vec_pretty(map) {
                Ok(bytes) => {
                    let path = plugin_state_path(&working_dir, plugin);
                    if let Err(e) = self.authority.filesystem.write_file(&path, &bytes) {
                        tracing::warn!("conductor persistence: failed to write {path:?}: {e}");
                    }
                }
                Err(e) => {
                    tracing::warn!(
                        "conductor persistence: failed to serialise plugin {plugin}: {e}"
                    );
                }
            }
        }
    }

    /// Read `.fresh/windows.json` + `.fresh/state/*.json` and
    /// reconstitute `self.windows` + `self.plugin_global_state`.
    /// Idempotent: if no files exist, leaves the editor at the
    /// default single-base-session shape.
    ///
    /// Sessions are loaded as inert shells (empty buffer set,
    /// empty stashes); the first dive into a previously
    /// persisted session re-warms it on demand exactly like a
    /// freshly created session.
    pub fn load_conductor_state(&mut self) {
        let working_dir = self.working_dir().to_path_buf();

        // Sessions.
        let windows_p = windows_path(&working_dir);
        if self.authority.filesystem.exists(&windows_p) {
            match self.authority.filesystem.read_file(&windows_p) {
                Ok(bytes) => match serde_json::from_slice::<PersistedWindows>(&bytes) {
                    Ok(env) => self.apply_persisted_windows(env),
                    Err(e) => {
                        tracing::warn!("conductor persistence: failed to parse {windows_p:?}: {e}");
                    }
                },
                Err(e) => {
                    tracing::warn!("conductor persistence: failed to read {windows_p:?}: {e}");
                }
            }
        }

        // Plugin global state. Walks the state dir if present and
        // loads every `*.json` whose stem is a safe plugin name.
        let state_dir = state_dir(&working_dir);
        if !self.authority.filesystem.exists(&state_dir) {
            return;
        }
        let entries = match self.authority.filesystem.read_dir(&state_dir) {
            Ok(es) => es,
            Err(e) => {
                tracing::warn!("conductor persistence: failed to read {state_dir:?}: {e}");
                return;
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
            match self.authority.filesystem.read_file(&path) {
                Ok(bytes) => {
                    match serde_json::from_slice::<HashMap<String, serde_json::Value>>(&bytes) {
                        Ok(map) if !map.is_empty() => {
                            self.plugin_global_state.insert(stem.to_owned(), map);
                        }
                        Ok(_) => {}
                        Err(e) => {
                            tracing::warn!("conductor persistence: failed to parse {path:?}: {e}");
                        }
                    }
                }
                Err(e) => {
                    tracing::warn!("conductor persistence: failed to read {path:?}: {e}");
                }
            }
        }
    }

    fn apply_persisted_windows(&mut self, env: PersistedWindows) {
        // Drop the synthetic default base session — we'll recreate
        // it from disk so its id matches what plugin state may
        // reference. If the persisted set didn't include the
        // current active session's id we still keep the active
        // one (so the user has somewhere to be).
        let current_active = self.active_window;
        let preserve_active = !env.windows.iter().any(|s| s.id == current_active.0);

        if !preserve_active {
            // Wipe the seeded default session so we can replace it
            // with the persisted version that has the same id.
            self.windows.remove(&current_active);
        }

        for ps in env.windows {
            let id = WindowId(ps.id);
            let mut s = Window::new(id, ps.label, ps.root);
            s.plugin_state = ps.plugin_state;
            self.windows.insert(id, s);
        }

        // Allocate next from max(persisted next_id, max
        // existing+1) to avoid collisions with the synthetic
        // session above.
        let max_existing = self.windows.keys().map(|k| k.0).max().unwrap_or(0);
        self.next_window_id = env.next_id.max(max_existing + 1);

        // Restore the active id if it's still resolvable.
        if self.windows.contains_key(&WindowId(env.active)) {
            self.active_window = WindowId(env.active);
        }
    }
}
