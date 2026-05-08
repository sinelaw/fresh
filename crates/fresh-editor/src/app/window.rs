//! Editor `Window` — a project-rooted unit of editor state.
//!
//! A `Window` bundles the state that is logically scoped to one
//! project root: the file tree, ignore matcher, LSP client set,
//! file watchers, split layout, and buffer membership. Switching the
//! active window re-targets the entire editor UI (file explorer,
//! quick-open, LSP roots) without recreating buffers, terminals, or
//! plugin state — those live on the `Editor` and survive switches.
//!
//! See `docs/internal/conductor-sessions-design.md` for the full
//! design rationale.
//!
//! ## Naming
//!
//! Internally we call these "windows" (modelled on VS Code windows)
//! to disambiguate from Fresh's pre-existing workspace-recovery and
//! config-layer "session" concepts. Conductor presents windows as
//! "agent sessions" in its UX, since the parallel-agents domain
//! language is what users see — but the editor types are `Window`,
//! `WindowId`, etc.
//!
//! ## Migration status
//!
//! Step 0a (cached_layout split) and Step 0b (warm-swap stashes →
//! live `Window` fields) are shipped: every per-subsystem state
//! field that used to warm-swap on `setActiveWindow`
//! — `panel_ids`, `file_mod_times`, `file_explorer`, `lsp`, and
//! the `splits` pair — now lives directly on `Window`.
//! `set_active_window` is a pointer write (plus first-dive seed
//! allocation for windows that have never been activated).
//!
//! Still on `Editor` (move in Step 0c–0f): `buffers`,
//! `terminal_manager` + `terminal_buffers` + `terminal_backing_files`,
//! `event_logs`, `position_history`, `bookmarks`. Once those land,
//! `closeWindow` becomes a single `Window::drop` and the
//! `attach_buffer_to_active_window` /
//! `detach_buffer_from_all_windows` shims go away.

use crate::app::types::WindowLayoutCache;
use crate::model::event::LeafId;
use crate::services::lsp::manager::LspManager;
use crate::view::file_tree::FileTreeView;
use crate::view::split::{SplitManager, SplitViewState};
use fresh_core::{BufferId, WindowId};
use std::collections::HashMap;
use std::path::PathBuf;

/// A project-rooted unit of editor state.
///
/// After Step 0b every per-subsystem field listed below is owned
/// outright by the window — there are no warm-swap stashes.
/// `setActiveWindow` is a pointer write; reads of the active
/// window's state route through Editor accessors
/// (`active_layout()`, `split_manager()`, `file_explorer()`, `lsp()`,
/// `panel_ids()`, `file_mod_times()`, …). Cross-window access goes
/// through `Editor.windows.get(&id)` directly.
pub struct Window {
    /// Stable identifier. The base window is always `WindowId(1)`.
    pub id: WindowId,

    /// User-visible label. Defaults to the basename of `root` (or
    /// "main" when the root is the original process cwd). Not
    /// required to be unique.
    pub label: String,

    /// Canonical absolute path of the project root. Read-only after
    /// construction; closing a window and creating a new one is the
    /// way to "rename" the root.
    pub root: PathBuf,

    /// File-explorer view (expansion, scroll, selection). `None`
    /// means "never opened" — the caller rebuilds at `root` on first
    /// toggle. Each window has its own view; switching windows shows
    /// the new window's tree (or none, if it hasn't been opened yet).
    pub file_explorer: Option<FileTreeView>,

    /// Split-tree layout (split tree + per-leaf view state — scroll,
    /// cursor positions, focused buffer in each leaf). `None` means
    /// "this window has never been activated and so has no layout
    /// yet"; the dive code creates a fresh layout rooted at a new
    /// empty unnamed buffer for that window. The base window has
    /// this populated at editor init.
    pub splits: Option<(SplitManager, HashMap<LeafId, SplitViewState>)>,

    /// Polling-based mtime cache for auto-revert. Auto-revert only
    /// fires for the active window's files; inactive windows' mtimes
    /// stay frozen at dive-out time and resync on dive-back —
    /// matching the user's mental model that a dormant window "is
    /// paused".
    pub file_mod_times: HashMap<PathBuf, std::time::SystemTime>,

    /// LSP manager (running language servers, configs, per-language
    /// root URIs). Each window owns its own LSP set, rooted at its
    /// project root; inactive windows' servers remain running in the
    /// background — that's the warm-LSP property the design's
    /// trade-off discussion calls out as a memory cost worth paying
    /// so dive-back is instant.
    ///
    /// `None` means "this window has never spawned any LSP"; the
    /// next LSP feature trigger will lazily create one.
    pub lsp: Option<LspManager>,

    /// Utility-dock panel-id → buffer-id occupancy. Each window
    /// gets its own dock — when one window has the search panel
    /// claimed and the user dives elsewhere, the new window starts
    /// with an empty dock and rebuilds on demand.
    pub panel_ids: HashMap<String, BufferId>,

    /// Buffers attached to this window. Each window owns the
    /// `EditorState` for its buffers outright; closing the window
    /// drops them. Opening the same file in two windows produces
    /// two independent buffers.
    pub buffers: HashMap<BufferId, crate::state::EditorState>,

    /// Plugin-managed per-window state. Outer key is plugin name,
    /// inner is the plugin-defined key. Read via
    /// `editor.getWindowState(key)` and written via
    /// `editor.setWindowState(key, value)`. Persisted to
    /// `.fresh/windows.json` so it survives editor restarts.
    pub plugin_state: HashMap<String, HashMap<String, serde_json::Value>>,

    /// Window-scoped layout hit-test cache: split-leaf rects, tab
    /// rects, the file-explorer rect, separators, scrollbars, and
    /// per-leaf `view_line_mappings` that mouse positioning and
    /// visual-line motion read. Repopulated by the renderer on every
    /// frame; stale until the next render after a window switch (the
    /// post-switch render fills it in before any input handling).
    /// Editor-chrome rects (status bar, menu, popups, prompt overlay)
    /// live on `Editor::chrome_layout` instead.
    pub(crate) layout_cache: WindowLayoutCache,
}

impl Window {
    /// Mutable handle to this window's split tree (or `None` when
    /// the layout hasn't been seeded yet). Useful at sites where
    /// the caller already has a `&mut Window` from a direct
    /// `self.windows.get_mut(&id)` and wants the split layout
    /// without going back through Editor's accessor.
    pub fn split_manager_mut(&mut self) -> Option<&mut SplitManager> {
        self.splits.as_mut().map(|(mgr, _)| mgr)
    }

    /// Mutable handle to this window's per-leaf view state map.
    pub fn split_view_states_mut(&mut self) -> Option<&mut HashMap<LeafId, SplitViewState>> {
        self.splits.as_mut().map(|(_, vs)| vs)
    }

    /// Both halves of the split layout at once. Returns `None` if
    /// the layout hasn't been seeded yet.
    pub fn splits_mut(
        &mut self,
    ) -> Option<(&mut SplitManager, &mut HashMap<LeafId, SplitViewState>)> {
        self.splits.as_mut().map(|(m, vs)| (m, vs))
    }

    /// Construct a window.
    ///
    /// `root` is taken as-is (the caller is responsible for
    /// canonicalisation). `label` defaults to the basename of
    /// `root` when empty.
    pub fn new(id: WindowId, label: impl Into<String>, root: PathBuf) -> Self {
        let mut label = label.into();
        if label.is_empty() {
            label = root
                .file_name()
                .and_then(|n| n.to_str())
                .map(str::to_owned)
                .unwrap_or_else(|| "main".to_owned());
        }
        Self {
            id,
            label,
            root,
            file_explorer: None,
            file_mod_times: HashMap::new(),
            plugin_state: HashMap::new(),
            lsp: None,
            panel_ids: HashMap::new(),
            splits: None,
            buffers: HashMap::new(),
            layout_cache: WindowLayoutCache::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// An empty label is replaced with the basename of `root`. This
    /// matches the design's "label defaults to the branch name" rule
    /// for windows Conductor creates over git worktrees, where the
    /// worktree directory name is the branch.
    #[test]
    fn empty_label_defaults_to_root_basename() {
        let s = Window::new(WindowId(1), "", PathBuf::from("/tmp/feat-auth"));
        assert_eq!(s.label, "feat-auth");
    }

    /// A non-empty label is preserved verbatim — Conductor renames
    /// (`r` action) write straight to this field.
    #[test]
    fn explicit_label_is_kept() {
        let s = Window::new(
            WindowId(2),
            "auth-with-uuid",
            PathBuf::from("/tmp/feat-auth"),
        );
        assert_eq!(s.label, "auth-with-uuid");
    }

    /// A root with no basename (e.g. `/`) and an empty label fall
    /// back to "main" rather than panicking. The base window at
    /// startup may hit this on some unusual cwds.
    #[test]
    fn empty_label_with_rootless_path_falls_back_to_main() {
        let s = Window::new(WindowId(1), "", PathBuf::from("/"));
        assert_eq!(s.label, "main");
    }
}
