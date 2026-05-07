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

use fresh_core::SessionId;
use std::path::PathBuf;

/// A project-rooted unit of editor state.
///
/// Initial fields are intentionally minimal — see module docs.
#[derive(Debug, Clone)]
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
        Self { id, label, root }
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
