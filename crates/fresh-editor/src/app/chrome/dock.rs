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

impl Editor {
    /// Decide the dock's startup chrome, before the first frame. In order of
    /// authority: Orchestrator mode always opens it; the plugin's own switch
    /// (`open_setting`, set to `false`) keeps it closed; otherwise it comes
    /// back the way the user left it, or as the manifest says on a first
    /// launch. No declaration, no dock. The width rule and a remembered
    /// width are adopted regardless, so a dock toggled open later is right.
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

        let switched_off = decl.open_setting.as_deref().is_some_and(|setting| {
            self.config
                .plugins
                .get(name.as_str())
                .and_then(|c| c.settings.get(setting))
                .and_then(|v| v.as_bool())
                == Some(false)
        });
        self.dock_reserved =
            orchestrator_mode || (!switched_off && remembered.open.unwrap_or(decl.open));
        tracing::debug!(
            plugin = %name,
            reserved = self.dock_reserved,
            width = ?self.dock_width,
            "startup dock chrome"
        );
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

    /// Whether the slot is held open for a panel that has not arrived. The
    /// flag's one reader: a slot with a panel in it is never reserved.
    pub(crate) fn dock_slot_reserved(&self) -> bool {
        self.dock_reserved && self.dock.is_none()
    }

    /// The width the dock asks for on a frame `frame_width` wide: the
    /// explicit width if there is one, else the rule. The one derivation,
    /// read by the layout and by what the plugin is told. Whether a column
    /// is carved at all is `frame::dock_width`'s call.
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
