//! The left dock column (orchestrator sessions panel).
//!
//! The dock's *content* is a plugin's; the column is the editor's. What the
//! editor owns about it — whether the slot is open and how wide — is decided
//! here at construction, from the plugin's manifest, what the user left, and
//! the launch mode; kept in `Editor::{dock_reserved, dock_width,
//! dock_width_rule}`; and remembered across launches in `<data>/chrome.json`.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use super::Editor;
use crate::model::filesystem::FileSystem;
use crate::services::plugins::manifest::PluginManifest;

/// What `chrome.json` remembers about the dock: whether the user left it
/// open, and the width they dragged it to. Either may be unknown — a
/// launch that never touched the dock writes nothing.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct DockChromeState {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) open: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) width: Option<u16>,
}

/// The file. Editor-global like the dock itself, and shared by every editor
/// this data directory serves, the way plugin global state is.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
struct ChromeState {
    #[serde(default)]
    dock: DockChromeState,
}

/// The narrowest a drag (or a plugin's `dock_width`) may make the dock —
/// under `DOCK_MIN` deliberately: that floor is where the dock stops being
/// *opened* by default, not where the user stops being allowed to squeeze
/// one they have. The other end is the editor's own minimum, `EDITOR_MIN`.
const DOCK_DRAG_MIN: u16 = 10;

fn chrome_state_path(data_dir: &Path) -> PathBuf {
    data_dir.join("chrome.json")
}

/// Read the remembered chrome, or nothing: a missing file is the first
/// launch, and an unreadable one is treated the same way rather than
/// refusing to start.
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
    /// Decide the dock's startup chrome, before the first frame.
    ///
    /// Three facts, in order of authority:
    ///
    /// 1. **The launch.** A bare `fresh` in Orchestrator mode is a request
    ///    for the workspace switcher; the dock opens whatever else is true.
    /// 2. **The plugin's own switch.** A declaration may name a boolean in
    ///    the plugin's settings (`open_setting`, the orchestrator's
    ///    `autoOpenDock`); set to `false` by the user, the dock never opens
    ///    itself.
    /// 3. **What the user left.** Otherwise the dock comes back the way it
    ///    was closed — open or not, and at the dragged width — and on a
    ///    first launch the way the manifest says it opens.
    ///
    /// No declaration, no dock: nothing is held open for a plugin that never
    /// said it would fill it. The width rule and a remembered width are
    /// adopted even then, so a dock the user later toggles opens right.
    pub(crate) fn apply_startup_dock_chrome(
        &mut self,
        manifests: &HashMap<String, PluginManifest>,
        orchestrator_mode: bool,
    ) {
        let remembered =
            read_dock_chrome_state(&*self.local_filesystem, &self.dir_context.data_dir);
        self.dock_width = remembered.width;

        // One slot, so one declaration; by name, so two plugins claiming it
        // resolve the same way on every launch.
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

    /// Hand back a column held open at startup that nothing mounted into.
    /// Returns whether there was one to hand back.
    ///
    /// The startup window is over: in production the `ready` hook's
    /// `HookCompleted` sentinel says so (every command its handlers sent is
    /// ahead of it in the channel, so a dock that was ever going to mount
    /// has), and a test harness that runs no startup hooks says so itself
    /// (`HarnessOptions::with_startup_chrome`), or the column would sit
    /// empty for the whole test. The chrome reflows to the reclaimed width
    /// through the one layout funnel; whether the strip also needs a full
    /// repaint is the caller's to say, because only the caller knows if a
    /// frame was ever painted with the column in it — the sentinel arrives
    /// after several, the harness releases before any. Nothing is
    /// remembered: the column going away is not the user's doing. A no-op
    /// once a dock is mounted, which is the mount having cleared the
    /// reservation already.
    pub fn release_startup_dock_reservation(&mut self) -> bool {
        let released = std::mem::take(&mut self.dock_reserved) && self.dock.is_none();
        if released {
            self.relayout();
        }
        released
    }

    /// Whether the dock slot is held open for a panel that has not arrived
    /// (`dock_reserved`), as the one place that says so. A slot with a
    /// panel in it is not reserved whatever the flag says — the mount and
    /// the slot-move op both clear it, but a reader that keeps asking the
    /// flag alone is one missed transition from laying out a column twice.
    pub(crate) fn dock_slot_reserved(&self) -> bool {
        self.dock_reserved && self.dock.is_none()
    }

    /// The width the dock column asks for on a frame `frame_width` wide: the
    /// explicit width if there is one, else the rule. **The one derivation**
    /// — the layout (`compute_dock_split`) and what the plugin is told
    /// (`dock_cols_if_open`) both read it, so they cannot disagree by a
    /// frame. Whether a column is carved at all, and how the request is
    /// clamped against the frame, is `frame::dock_width`'s.
    pub(crate) fn requested_dock_width(&self, frame_width: u16) -> u16 {
        self.dock_width
            .unwrap_or_else(|| self.dock_width_rule.width(frame_width))
    }

    /// An explicit width, kept inside what the terminal can give: no
    /// narrower than `DOCK_DRAG_MIN`, and never so wide the editor loses its
    /// `EDITOR_MIN` columns. The drag and the `dock_width` op share it, so
    /// there is one rule about how narrow or wide a user (or a plugin) may
    /// make the dock, not two spellings of it.
    pub(crate) fn clamp_dock_width(&self, cols: u16) -> u16 {
        let max = self
            .terminal_width
            .saturating_sub(crate::view::shell::frame::EDITOR_MIN)
            .max(DOCK_DRAG_MIN);
        cols.clamp(DOCK_DRAG_MIN, max)
    }

    /// Remember whether the dock is open — at quit, which is the one moment
    /// "the way you left it" is literally true.
    ///
    /// Not on every mount and unmount: the orchestrator plugin closes and
    /// reopens the dock on its own (a dive into a worktree closes the picker
    /// that was holding the dock behind it and brings the dock back when the
    /// attach lands), and a write on each of those would remember a plugin's
    /// transient as the user's decision — quit inside the window and the
    /// next launch has no dock. The slot at quit is what the user actually
    /// left. A column still merely held open at quit (the plugin never
    /// mounted) says nothing about the user, and leaves the file as it was.
    pub fn save_dock_chrome(&self) {
        let open = (!self.dock_slot_reserved()).then_some(self.dock.is_some());
        self.persist_dock_chrome(open);
    }

    /// Remember the dock's explicit width now — the end of a drag, or a
    /// plugin's `dock_width` op. Unlike `open`, a width is never transient,
    /// so it is written as it changes rather than at quit. A no-op with no
    /// dock mounted: a press on a reserved column's grip changes nothing.
    pub(crate) fn persist_dock_width(&self) {
        if self.dock.is_some() {
            self.persist_dock_chrome(None);
        }
    }

    /// Write `chrome.json`: the explicit width as it stands, and `open` when
    /// the caller has an answer. Read-merge-write, so a width write leaves
    /// `open` as the last quit recorded it and vice versa. Atomic (tmp +
    /// rename) and best-effort, like plugin state.
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

    /// The dock column's width as the host would carve it now — with a
    /// panel in the slot, held open for one, or neither: what a dock
    /// *would* get. `0` when the terminal is too narrow for one at all.
    /// What the plugin lays its content out to, before its mount has
    /// been processed as much as after.
    pub(crate) fn dock_cols_if_open(&self) -> u16 {
        let requested = self.requested_dock_width(self.terminal_width);
        crate::view::shell::frame::dock_width(Some(requested), self.terminal_width).unwrap_or(0)
    }
}

/// Behavior owned by this component — the drag half of the
/// width-resize grab; the press half arms it in `on_pointer`, and the
/// release finalizer in `shell_host`'s `GripRelease` arm persists the width.
impl Editor {
    /// Dock resize drag (`PointerGrab::DockResize`, armed by the grip's own
    /// press — see `view::shell::dock`): track the pointer column as the new
    /// dock width (the right border follows the cursor), clamped so it
    /// can't swallow the chrome.
    pub(crate) fn handle_dock_resize_drag(&mut self, col: u16) {
        let new_w = self.clamp_dock_width(col.saturating_add(1));
        if self.dock.is_none() || self.dock_width == Some(new_w) {
            return;
        }
        // The explicit width is the one fact the layout reads, so setting it
        // is the whole of the drag; `relayout` re-derives the column from
        // it. Disk waits for the release (`persist_dock_width`).
        self.dock_width = Some(new_w);
        // The dock got wider/narrower: reflow the chrome (terminals,
        // viewports, panels) to the new dock width via the funnel.
        self.relayout();
    }
}
