//! Editor `Session` — a project-rooted unit of editor state.
//!
//! A `Session` bundles the state that is logically scoped to one
//! project root: the file tree, ignore matcher, LSP client set,
//! file watchers, split layout, and buffer membership. Switching the
//! active session re-targets the entire editor UI (file explorer,
//! quick-open, LSP roots) without recreating buffers, terminals, or
//! plugin state — those live on the `Editor` and survive switches.
//!
//! See `docs/internal/conductor-sessions-design.md` for the full
//! design rationale.
//!
//! ## Migration status
//!
//! This module is the *foothold* for the Session abstraction. In its
//! initial form (Step 1 of the migration sequence) the struct holds
//! only `id`, `label`, and `root`. The editor still owns the file
//! tree, LSP set, watchers, and split layout directly. Subsequent
//! commits move those into Session one subsystem at a time, each
//! step preserving today's single-root behaviour.

use crate::model::event::LeafId;
use crate::services::lsp::manager::LspManager;
use crate::view::file_tree::FileTreeView;
use crate::view::split::{SplitManager, SplitViewState};
use fresh_core::{BufferId, SessionId};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

/// A project-rooted unit of editor state.
///
/// "Stash" fields hold the inactive session's snapshot of state
/// that for the *active* session lives directly on `Editor`. This
/// is the warm-switching pattern: `setActiveSession` swaps the
/// active session's live state out into the outgoing session's
/// stash, then pulls the incoming session's stash into the active
/// slots — O(1) and lossless. New code that reads any of these
/// stash fields directly is a bug; only the swap path on
/// `setActiveSession` should touch them.
pub struct Session {
    /// Stable identifier. The base session is always `SessionId(1)`.
    pub id: SessionId,

    /// User-visible label. Defaults to the basename of `root` (or
    /// "main" when the root is the original process cwd). Not
    /// required to be unique.
    pub label: String,

    /// Canonical absolute path of the project root. Read-only after
    /// construction; closing a session and creating a new one is the
    /// way to "rename" the root.
    pub root: PathBuf,

    /// **Stash.** File-explorer view (expansion, scroll, selection)
    /// when this session is *inactive*. The active session's view
    /// lives on `Editor.file_explorer`; on switch we move both at
    /// once. `None` means "never opened" — the caller rebuilds at
    /// `root` on first toggle.
    pub file_explorer_stash: Option<FileTreeView>,

    /// **Stash.** Split-tree layout (split tree + per-leaf view
    /// state — scroll, cursor positions, focused buffer in each
    /// leaf) when this session is *inactive*. The active session's
    /// layout lives on `Editor.split_manager` and
    /// `Editor.split_view_states`; on dive we move all of it.
    ///
    /// `None` means "this session has never been activated and so
    /// has no layout yet"; the dive code creates a fresh layout
    /// rooted at a new empty unnamed buffer for that session.
    pub splits_stash: Option<(SplitManager, HashMap<LeafId, SplitViewState>)>,

    /// **Stash.** Polling-based mtime cache for auto-revert when
    /// this session is *inactive*. Auto-revert only fires for
    /// the active session's files; an inactive session's mtimes
    /// stay frozen at dive-out time and resync on dive-back —
    /// matching the user's mental model that a dormant session
    /// "is paused".
    pub file_mod_times_stash: HashMap<PathBuf, std::time::SystemTime>,

    /// **Stash.** LSP manager (running language servers, configs,
    /// per-language root URIs) when this session is *inactive*.
    /// The active session's LSP lives on `Editor.lsp`; on dive we
    /// swap. Inactive-session LSPs remain running in the
    /// background — that's the warm-LSP property the design's
    /// trade-off discussion calls out as a memory cost worth
    /// paying so dive-back is instant.
    ///
    /// `None` means "this session has never spawned any LSP";
    /// the next LSP feature trigger will lazily create one.
    pub lsp_stash: Option<LspManager>,

    /// **Stash.** Utility-dock panel-id → buffer-id occupancy when
    /// this session is *inactive*. The active session's
    /// occupancy lives on `Editor.panel_ids`. Each session gets
    /// its own dock — when one session has the search panel
    /// claimed and the user dives elsewhere, the new session
    /// starts with an empty dock and rebuilds on demand.
    pub panel_ids_stash: HashMap<String, BufferId>,

    /// Buffers attached to this session (membership only — the
    /// buffer storage stays on `Editor`, see "Why buffer storage
    /// stays Editor-global" in the design doc). Used by
    /// `closeSession` to drop session-private buffers and by
    /// future per-session quick-open scoping.
    pub buffers: HashSet<BufferId>,

    /// Plugin-managed per-session state. Outer key is plugin name,
    /// inner is the plugin-defined key. Read via
    /// `editor.getSessionState(key)` and written via
    /// `editor.setSessionState(key, value)`. Survives editor
    /// restarts only when the cross-restart persistence step
    /// flushes it; for now lives in memory only.
    pub plugin_state: HashMap<String, HashMap<String, serde_json::Value>>,
}

impl Session {
    /// Construct a session.
    ///
    /// `root` is taken as-is (the caller is responsible for
    /// canonicalisation). `label` defaults to the basename of
    /// `root` when empty.
    pub fn new(id: SessionId, label: impl Into<String>, root: PathBuf) -> Self {
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
            file_explorer_stash: None,
            file_mod_times_stash: HashMap::new(),
            plugin_state: HashMap::new(),
            lsp_stash: None,
            panel_ids_stash: HashMap::new(),
            splits_stash: None,
            buffers: HashSet::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// An empty label is replaced with the basename of `root`. This
    /// matches the design's "label defaults to the branch name" rule
    /// for sessions Conductor creates over git worktrees, where the
    /// worktree directory name is the branch.
    #[test]
    fn empty_label_defaults_to_root_basename() {
        let s = Session::new(SessionId(1), "", PathBuf::from("/tmp/feat-auth"));
        assert_eq!(s.label, "feat-auth");
    }

    /// A non-empty label is preserved verbatim — Conductor renames
    /// (`r` action) write straight to this field.
    #[test]
    fn explicit_label_is_kept() {
        let s = Session::new(
            SessionId(2),
            "auth-with-uuid",
            PathBuf::from("/tmp/feat-auth"),
        );
        assert_eq!(s.label, "auth-with-uuid");
    }

    /// A root with no basename (e.g. `/`) and an empty label fall
    /// back to "main" rather than panicking. The base session at
    /// startup may hit this on some unusual cwds.
    #[test]
    fn empty_label_with_rootless_path_falls_back_to_main() {
        let s = Session::new(SessionId(1), "", PathBuf::from("/"));
        assert_eq!(s.label, "main");
    }
}
