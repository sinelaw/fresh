//! The left dock column (orchestrator sessions panel).
//!
//! The dock's *content* is a plugin's; the column is the editor's. Whether
//! the slot is open and how wide is decided here at construction, from the
//! plugin's manifest, the plugin's own `open_setting`, and the launch mode;
//! kept in `Editor::{dock_reserved, dock_width, dock_width_rule}`.
//!
//! Openness is remembered in one place only: the boolean the manifest names
//! (`plugins.<name>.settings.<key>` — the orchestrator's `autoOpenDock`),
//! which is also what the Settings UI edits. The plugin writes it when the
//! user opens or closes the dock, so a dock the user closed is still closed
//! on the next launch and the Settings value says so (issue #3442).
//!
//! The setting holds the user's own decisions, not a mirror of the slot: a
//! plugin may open or close its dock for an event of its own (the
//! orchestrator attaching to a discovered worktree, or surfacing a recovered
//! workspace) without touching the preference, so within a session the column
//! can differ from the setting. `chrome.json` remembers the dragged width and
//! nothing else, plus the pre-#3442 `open` it still reads once on upgrade.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use super::Editor;
use crate::model::filesystem::FileSystem;
use crate::services::plugins::manifest::PluginManifest;

/// What `chrome.json` remembers about the dock: the width a drag left, which
/// may be unknown. Whether the dock is open is the plugin's `open_setting`,
/// not this file — see the module docs.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
struct DockChromeState {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    width: Option<u16>,
    /// Where openness lived before #3442. Read as a one-time fallback so an
    /// upgrading user who had closed the dock does not get it back, and never
    /// written: the first width write drops it and `open_setting` carries it
    /// from then on.
    #[serde(default, rename = "open", skip_serializing_if = "Option::is_none")]
    legacy_open: Option<bool>,
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
fn read_dock_chrome_state(fs: &dyn FileSystem, data_dir: &Path) -> DockChromeState {
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
    /// Decide the dock's startup chrome, before the first frame. The boolean
    /// the manifest names (`open_setting`) decides: it is both what the user
    /// left behind and what the Settings UI edits, so it is the only thing
    /// consulted once it exists. With nothing set — a first launch — the
    /// launch mode decides (a bare `fresh` is the orchestrator, whose dock is
    /// the point of it), falling back to the manifest's own `open`. No
    /// declaration, no dock. The width rule and a remembered width are
    /// adopted regardless, so a dock toggled open later is right.
    ///
    /// Orchestrator mode used to *override* the setting rather than default
    /// it, which left an `autoOpenDock: false` with no effect at all in the
    /// mode that is the default since 0.5.2 (issue #3442).
    ///
    /// Between the two sits `chrome.json`'s pre-#3442 `open`, read once so an
    /// upgrading user who had closed the dock is not handed it back.
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

        let configured = decl.open_setting.as_deref().and_then(|setting| {
            self.config
                .plugins
                .get(name.as_str())
                .and_then(|c| c.settings.get(setting))
                .and_then(|v| v.as_bool())
        });
        self.dock_reserved = configured
            .or(remembered.legacy_open)
            .unwrap_or(orchestrator_mode || decl.open);
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

    /// Remember the dock's explicit width now (a drag's end, or a plugin's
    /// `dock_width` op). A no-op with no dock mounted.
    ///
    /// Openness is not written here, or at quit: the plugin records it in its
    /// `open_setting` as the user expresses it. Inferring it from the slot at
    /// quit could not tell the user's decision from a transient — the plugin
    /// closes and reopens the dock on its own (a dive into a worktree) — and
    /// would overwrite a value the user had just set in the Settings UI.
    pub(crate) fn persist_dock_width(&self) {
        if self.dock.is_none() {
            return;
        }
        let fs = &*self.local_filesystem;
        let path = chrome_state_path(&self.dir_context.data_dir);
        let dock = DockChromeState {
            width: self.dock_width,
            legacy_open: None,
        };
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
