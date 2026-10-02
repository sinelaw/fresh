//! The left dock column (orchestrator sessions panel).
//!
//! The dock's *content* is a plugin's; the column is the editor's. Whether
//! the slot is open and how wide is decided here at construction, from the
//! plugin's manifest, what the user left, and the launch mode; kept in
//! `Editor::{dock_reserved, dock_width, dock_width_rule}`; and remembered
//! across launches in `<data>/chrome.json`.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use super::Editor;
use crate::model::filesystem::FileSystem;
use crate::services::plugins::manifest::PluginManifest;

/// What `chrome.json` remembers about the dock. Either may be unknown.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct DockChromeState {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) open: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) width: Option<u16>,
}

/// The file: editor-global, shared by every editor this data directory serves.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
struct ChromeState {
    #[serde(default)]
    dock: DockChromeState,
}

/// The narrowest a drag (or a plugin's `dock_width`) may make the dock.
/// Below `DOCK_MIN` on purpose: that floor is where the dock stops being
/// opened by default, not where the user stops being allowed to squeeze it.
const DOCK_DRAG_MIN: u16 = 10;

fn chrome_state_path(data_dir: &Path) -> PathBuf {
    data_dir.join("chrome.json")
}

/// Read the remembered chrome. A missing or unreadable file is a first launch.
pub(crate) fn read_dock_chrome_state(fs: &dyn FileSystem, data_dir: &Path) -> DockChromeState {
    let path = chrome_state_path(data_dir);
    match fs.read_file(&path) {
        Ok(bytes) => match serde_json::from_slice::<ChromeState>(&bytes) {
            Ok(state) => state.dock,
            Err(e) => {
                tracing::warn!("chrome state: ignoring unparseable {path:?}: {e}");
                DockChromeState::default()
            }
        },
        Err(_) => DockChromeState::default(),
    }
}

/// What the plugin's `open_setting` asks for. It replaced a boolean whose
/// `true` meant "allow", not "open".
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DockOpenPolicy {
    /// Closed, ignoring what was remembered.
    Never,
    /// Open, ignoring what was remembered.
    Always,
    /// What was remembered; on a first launch, the launch mode, then the
    /// manifest's `open`.
    Auto,
}

impl DockOpenPolicy {
    fn as_str(self) -> &'static str {
        match self {
            Self::Never => "never",
            Self::Always => "always",
            Self::Auto => "auto",
        }
    }

    /// Booleans are the pre-#3442 spelling and are always accepted, so
    /// nothing depends on the rewrite below having run.
    ///
    /// Anything unrecognised is `Auto`, which is also what the Settings UI
    /// falls back to showing, so the two agree.
    fn read(value: Option<&serde_json::Value>) -> Self {
        match value {
            Some(serde_json::Value::String(s)) => match s.as_str() {
                "never" => Self::Never,
                "always" => Self::Always,
                _ => Self::Auto,
            },
            Some(serde_json::Value::Bool(false)) => Self::Never,
            Some(serde_json::Value::Bool(true)) => Self::Auto,
            _ => Self::Auto,
        }
    }

    fn is_legacy(value: Option<&serde_json::Value>) -> bool {
        matches!(value, Some(serde_json::Value::Bool(_)))
    }
}

impl Editor {
    /// Decide the dock's startup chrome, before the first frame. No dock
    /// declared, no column.
    ///
    /// The width rule and a remembered width are adopted even when the slot
    /// stays closed, so a dock toggled open later comes up the right size.
    ///
    /// Orchestrator mode is the default *under* the policy, not an override
    /// over it: overriding left `autoOpenDock: false` with no effect in the
    /// launch mode that is the default since 0.5.2 (#3442).
    pub(crate) fn apply_startup_dock_chrome(
        &mut self,
        manifests: &HashMap<String, PluginManifest>,
        orchestrator_mode: bool,
    ) {
        let remembered =
            read_dock_chrome_state(&*self.local_filesystem, &self.dir_context.data_dir);
        self.dock_width = remembered.width;

        // One slot, so one declaration: the lowest name wins, deterministically.
        let docks = || {
            manifests
                .iter()
                .filter_map(|(name, m)| m.chrome.dock.as_ref().map(|d| (name, d)))
        };
        let Some((name, decl)) = docks().min_by_key(|(name, _)| *name) else {
            return;
        };
        let claimants = docks().count();
        if claimants > 1 {
            tracing::warn!("plugin manifest: {claimants} plugins declare the dock; {name} wins");
        }
        self.dock_width_rule = decl.width;

        let setting = decl.open_setting.clone();
        let stored = setting.as_deref().and_then(|key| {
            self.config
                .plugins
                .get(name.as_str())
                .and_then(|c| c.settings.get(key))
        });
        let policy = DockOpenPolicy::read(stored);
        let legacy = DockOpenPolicy::is_legacy(stored);
        self.dock_reserved = match policy {
            DockOpenPolicy::Never => false,
            DockOpenPolicy::Always => true,
            DockOpenPolicy::Auto => remembered.open.unwrap_or(orchestrator_mode || decl.open),
        };
        tracing::debug!(
            plugin = %name,
            reserved = self.dock_reserved,
            width = ?self.dock_width,
            ?policy,
            "startup dock chrome"
        );

        if legacy {
            let plugin = name.clone();
            if let Some(key) = setting {
                self.rewrite_legacy_dock_open_setting(&plugin, &key, policy);
            }
        }
    }

    /// Rewrite a pre-#3442 boolean as the mode it meant, in memory and on
    /// disk.
    ///
    /// Needed because the plugin's field registration only fills in a value
    /// that is *absent* (`handle_add_plugin_config_field` uses `or_insert`):
    /// a boolean would survive under the enum's schema, and the Settings UI
    /// would show the enum default while startup honoured the boolean.
    ///
    /// Best-effort — `DockOpenPolicy::read` still accepts booleans, so a
    /// failed write just retries next launch.
    fn rewrite_legacy_dock_open_setting(
        &mut self,
        plugin: &str,
        setting: &str,
        policy: DockOpenPolicy,
    ) {
        let value = serde_json::Value::String(policy.as_str().to_string());
        let cfg = std::sync::Arc::make_mut(&mut self.config);
        if let Some(entry) = cfg.plugins.get_mut(plugin) {
            if let serde_json::Value::Object(map) = &mut entry.settings {
                map.insert(setting.to_string(), value.clone());
            }
        }
        tracing::info!(
            plugin,
            setting,
            mode = policy.as_str(),
            "migrating a boolean dock open_setting to its mode"
        );
        self.persist_config_pointer(&format!("/plugins/{plugin}/settings/{setting}"), value);
    }

    /// Hand back a column held open at startup that nothing mounted into;
    /// returns whether there was one. Called when the `ready` hook's
    /// sentinel lands (every mount it sent is ahead of it in the channel),
    /// or by a test harness that runs no startup hooks. Reflows the chrome;
    /// the caller decides whether a full repaint is needed too. Nothing is
    /// remembered: the column going away is not the user's doing.
    pub fn release_startup_dock_reservation(&mut self) -> bool {
        let released = std::mem::take(&mut self.dock_reserved) && self.dock.is_none();
        if released {
            self.relayout();
        }
        released
    }

    /// Whether the slot is held open for a panel that has not arrived.
    pub(crate) fn dock_slot_reserved(&self) -> bool {
        self.dock_reserved && self.dock.is_none()
    }

    /// The width the dock asks for on a frame `frame_width` wide: the
    /// explicit width if there is one, else the rule. The one derivation —
    /// do not recompute it elsewhere. Whether a column is carved at all is
    /// `frame::dock_width`'s call.
    pub(crate) fn requested_dock_width(&self, frame_width: u16) -> u16 {
        self.dock_width
            .unwrap_or_else(|| self.dock_width_rule.width(frame_width))
    }

    /// Clamp an explicit width so the editor keeps its `EDITOR_MIN` columns.
    /// Shared by the drag and the `dock_width` op.
    pub(crate) fn clamp_dock_width(&self, cols: u16) -> u16 {
        let max = self
            .terminal_width
            .saturating_sub(crate::view::shell::frame::EDITOR_MIN)
            .max(DOCK_DRAG_MIN);
        cols.clamp(DOCK_DRAG_MIN, max)
    }

    /// Remember whether the dock is open. Called at quit, not on every mount
    /// and unmount: the plugin closes and reopens the dock on its own (a
    /// dive into a worktree), and writing those would remember a transient
    /// as the user's decision. A column merely held open at quit says
    /// nothing and leaves the file as it was.
    pub fn save_dock_chrome(&self) {
        let open = (!self.dock_slot_reserved()).then_some(self.dock.is_some());
        self.persist_dock_chrome(open);
    }

    /// Remember the dock's explicit width now (a drag's end, or a plugin's
    /// `dock_width` op). A no-op with no dock mounted.
    pub(crate) fn persist_dock_width(&self) {
        if self.dock.is_some() {
            self.persist_dock_chrome(None);
        }
    }

    /// Write `chrome.json`: the width as it stands, and `open` when given.
    /// Read-merge-write, atomic (tmp + rename), best-effort.
    fn persist_dock_chrome(&self, open: Option<bool>) {
        let fs = &*self.local_filesystem;
        let path = chrome_state_path(&self.dir_context.data_dir);
        let mut dock = read_dock_chrome_state(fs, &self.dir_context.data_dir);
        dock.width = self.dock_width;
        if let Some(open) = open {
            dock.open = Some(open);
        }
        let bytes = match serde_json::to_vec_pretty(&ChromeState { dock }) {
            Ok(b) => b,
            Err(e) => {
                tracing::warn!("chrome state: failed to serialise: {e}");
                return;
            }
        };
        if let Some(dir) = path.parent() {
            if let Err(e) = fs.create_dir_all(dir) {
                tracing::warn!("chrome state: failed to create {dir:?}: {e}");
                return;
            }
        }
        let tmp = path.with_extension("json.tmp");
        if let Err(e) = fs.write_file(&tmp, &bytes) {
            tracing::warn!("chrome state: failed to write {tmp:?}: {e}");
            return;
        }
        if let Err(e) = fs.rename(&tmp, &path) {
            tracing::warn!("chrome state: failed to rename {tmp:?} → {path:?}: {e}");
        }
    }

    /// The dock column's width as the host would carve it now, mounted or
    /// not; `0` when the terminal is too narrow for one. What the plugin
    /// lays its content out to.
    #[cfg(feature = "plugins")]
    pub(crate) fn dock_cols_if_open(&self) -> u16 {
        let requested = self.requested_dock_width(self.terminal_width);
        crate::view::shell::frame::dock_width(Some(requested), self.terminal_width).unwrap_or(0)
    }

    /// Dock resize drag (`PointerGrab::DockResize`): the pointer column is
    /// the new width, clamped. Disk waits for the release
    /// (`persist_dock_width`).
    pub(crate) fn handle_dock_resize_drag(&mut self, col: u16) {
        let new_w = self.clamp_dock_width(col.saturating_add(1));
        if self.dock.is_none() || self.dock_width == Some(new_w) {
            return;
        }
        self.dock_width = Some(new_w);
        self.relayout();
    }
}
