//! Chrome seam (PoC): a semantic snapshot of the editor's non-text "chrome".
//!
//! This is the proof-of-concept seam from `docs/internal/NON_TERMINAL_UI_RESEARCH.md`.
//! It lets the editor describe its **chrome** — menu bar, the split layout tree,
//! the per-pane tab list, the active overlay/popup stack, and the status line —
//! *semantically*, independently of the cell grid it renders into today.
//!
//! The point of the seam (mirroring Neovim's `ext_*` model):
//!   - the **terminal** frontend ignores the snapshot and keeps painting cells
//!     (zero behaviour change — proves non-disruption), while
//!   - a **GUI/web** frontend can consume the snapshot and render the same chrome
//!     with native widgets / proportional fonts (native tabs, draggable split
//!     dividers, native popups), without the text body changing.
//!
//! The snapshot is `Serialize` so it can also be dumped (for tests / a remote
//! transport) — but per the xi-editor lesson, the *intended* path is in-process:
//! the GUI reads this struct directly, with no serialization on the hot path.
//!
//! This is deliberately a read-only projection of existing editor state; it adds
//! no new source of truth. Faithful pixel rendering of each element is the
//! follow-on work (verified visually with `fresh --gui`).

use serde::Serialize;

use crate::app::Editor;
use crate::view::split::SplitNode;

/// A complete semantic description of the editor chrome for one frame.
#[derive(Debug, Clone, Serialize)]
pub struct ChromeSnapshot {
    /// Top-level menu titles, in order (e.g. "File", "Edit", "View", …).
    pub menubar: Vec<String>,
    /// The active window's split layout tree. `SplitNode` is already
    /// serializable; a GUI lays the panes out in pixels from this, a terminal
    /// from cells. Maps to Neovim `ext_multigrid`.
    pub split_layout: SplitNode,
    /// One entry per pane/tab target discovered by walking `split_layout`,
    /// in tree order. Maps to Neovim `ext_tabline`.
    pub tabs: Vec<TabEntry>,
    /// The overlay/popup stack, top-first (e.g. ["Menu", "Popup"]), excluding
    /// the always-present editor base layer. Maps to `ext_popupmenu` /
    /// `ext_messages` / native modal dialogs.
    pub overlays: Vec<String>,
    /// Number of internal split containers (dividers) in the tree — a quick
    /// scalar a GUI uses to know how many draggable dividers to render.
    pub divider_count: usize,
    /// The active buffer id, so a layout pass can mark the focused pane without
    /// re-querying the editor.
    pub active_buffer: usize,
}

/// One tab/pane entry, derived from a leaf (or grouped node) of the split tree.
#[derive(Debug, Clone, Serialize)]
pub struct TabEntry {
    /// `"leaf"` (a buffer pane) or `"group"` (a grouped subtree shown as a tab).
    pub kind: &'static str,
    /// Human label: the group name, or the buffer id for a leaf (names are
    /// enriched in the follow-on; the id is enough to prove capture here).
    pub label: String,
    /// The buffer shown in this leaf, if it is a leaf (`None` for groups).
    pub buffer_id: Option<usize>,
    /// True if this pane holds the active buffer.
    pub active: bool,
}

impl Editor {
    /// Build a [`ChromeSnapshot`] from current editor state.
    ///
    /// Pure read-only projection — safe to call every frame. The terminal
    /// frontend never calls this; a GUI frontend calls it instead of (or
    /// alongside) `render()` to draw chrome natively.
    pub fn chrome_snapshot(&self) -> ChromeSnapshot {
        let root = self.split_manager().root();
        let active_buffer = self.active_buffer();

        let mut tabs = Vec::new();
        let mut divider_count = 0usize;
        collect(root, active_buffer, &mut tabs, &mut divider_count);

        let menubar = self
            .expanded_menu_definitions()
            .iter()
            .map(|m| m.label.clone())
            .collect();

        // Overlay stack, top-first, dropping the always-present `Editor` base.
        let overlays = self
            .overlay_layers()
            .iter()
            .map(|layer| format!("{:?}", layer.kind))
            .filter(|k| k != "Editor")
            .collect();

        ChromeSnapshot {
            menubar,
            split_layout: root.clone(),
            tabs,
            overlays,
            divider_count,
            active_buffer: active_buffer.0,
        }
    }
}

/// Walk the split tree in order, collecting tab/pane entries and counting
/// dividers (internal `Split` containers).
fn collect(
    node: &SplitNode,
    active_buffer: fresh_core::BufferId,
    tabs: &mut Vec<TabEntry>,
    dividers: &mut usize,
) {
    match node {
        SplitNode::Leaf { buffer_id, .. } => {
            tabs.push(TabEntry {
                kind: "leaf",
                label: format!("buffer#{}", buffer_id.0),
                buffer_id: Some(buffer_id.0),
                active: *buffer_id == active_buffer,
            });
        }
        SplitNode::Split { first, second, .. } => {
            // Each internal node is one draggable divider for a GUI to render
            // (parity with Neovim ext_multigrid window placement).
            *dividers += 1;
            collect(first, active_buffer, tabs, dividers);
            collect(second, active_buffer, tabs, dividers);
        }
        SplitNode::Grouped { name, layout, .. } => {
            tabs.push(TabEntry {
                kind: "group",
                label: name.clone(),
                buffer_id: None,
                active: false,
            });
            collect(layout, active_buffer, tabs, dividers);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use crate::config_io::DirectoryContext;
    use crate::model::filesystem::{FileSystem, NoopFileSystem};
    use crate::view::color_support::ColorCapability;
    use std::sync::Arc;

    fn test_editor() -> Editor {
        let dir_context = DirectoryContext::from_system().expect("dir context");
        let fs: Arc<dyn FileSystem + Send + Sync> = Arc::new(NoopFileSystem);
        Editor::new(
            Config::default(),
            80,
            24,
            dir_context,
            ColorCapability::TrueColor,
            fs,
        )
        .expect("editor")
    }

    #[test]
    fn snapshot_captures_menubar_and_single_pane() {
        let editor = test_editor();
        let snap = editor.chrome_snapshot();

        // A fresh editor has a populated menu bar and exactly one pane.
        assert!(
            !snap.menubar.is_empty(),
            "menubar should be captured for a GUI to render natively"
        );
        assert_eq!(snap.divider_count, 0, "no splits yet");
        assert_eq!(snap.tabs.len(), 1, "one buffer pane");
        assert_eq!(snap.tabs[0].kind, "leaf");
        assert!(snap.tabs[0].active, "the only pane holds the active buffer");
    }

    #[test]
    fn snapshot_is_serializable_to_json() {
        let editor = test_editor();
        let snap = editor.chrome_snapshot();
        let json = serde_json::to_string(&snap).expect("snapshot must serialize");
        // The full chrome is captured semantically (this is what a remote/web
        // frontend would receive, or a debug dump prints).
        assert!(json.contains("menubar"));
        assert!(json.contains("split_layout"));
        assert!(json.contains("tabs"));
        assert!(json.contains("overlays"));
    }

    #[test]
    fn snapshot_reflects_overlay_stack() {
        let editor = test_editor();
        let snap = editor.chrome_snapshot();
        // With no popup open, the overlay stack (minus the editor base) is empty;
        // when a GUI sees a non-empty list it renders native popups/dialogs.
        assert!(
            snap.overlays.iter().all(|k| k != "Editor"),
            "the editor base layer is excluded from the chrome overlay list"
        );
    }
}
