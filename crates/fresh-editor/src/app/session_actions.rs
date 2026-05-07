//! Editor methods for session lifecycle (create, switch, close).
//!
//! Sessions are introduced in
//! `docs/internal/conductor-sessions-design.md`. The MVP build of
//! these actions implements the **plugin-visible behaviour** —
//! sessions can be created, switched, and closed, and the
//! corresponding hooks fire — but does not yet move per-subsystem
//! state (file tree, LSP, ignore matcher) into `Session`. As a
//! result, `set_active_session` here updates `working_dir` and
//! discards the cached file explorer (so it rebuilds on next open),
//! but warm-LSP swap and warm-file-tree swap are deferred to the
//! per-subsystem migration commits.
//!
//! Plugins that listen for `active_session_changed` see the same
//! sequence regardless of whether the swap is warm or cold; the
//! difference is performance only.

use crate::app::session::Session;
use crate::services::plugins::hooks::HookArgs;
use fresh_core::SessionId;
use std::path::PathBuf;

impl crate::app::Editor {
    /// Allocate a session id, insert a new `Session`, fire
    /// `session_created`. Does not switch active.
    ///
    /// Caller is responsible for ensuring `root` is absolute. The
    /// `PluginCommand::CreateSession` dispatcher rejects relative
    /// paths before reaching here.
    pub fn create_session_at(&mut self, root: PathBuf, label: String) -> SessionId {
        let id = SessionId(self.next_session_id);
        self.next_session_id += 1;

        let session = Session::new(id, label, root.clone());
        let resolved_label = session.label.clone();
        self.sessions.insert(id, session);

        self.plugin_manager.run_hook(
            "session_created",
            HookArgs::SessionCreated {
                id: id.0,
                label: resolved_label,
                root: root.to_string_lossy().into_owned(),
            },
        );

        id
    }

    /// Switch the active session to `id`.
    ///
    /// Atomic swap: per-session live state (currently the file
    /// explorer view) is moved out of `Editor` into the outgoing
    /// session's stash and the incoming session's stash is moved
    /// onto `Editor`. The dive is now warm — switching back
    /// preserves the previous file-tree expansion / scroll /
    /// selection rather than rebuilding from scratch.
    ///
    /// No-op when `id` is already active. Logs and returns when
    /// `id` is unknown — the design treats unknown ids as a plugin
    /// bug (caller verifies with `listSessions`), not a recoverable
    /// error worth surfacing through the channel.
    pub fn set_active_session(&mut self, id: SessionId) {
        if self.active_session == id {
            return;
        }
        if !self.sessions.contains_key(&id) {
            tracing::warn!("set_active_session: unknown session id {id}; active session unchanged");
            return;
        }

        let previous_id = self.active_session;

        // Snapshot the new root before mutating fields that borrow
        // self.sessions.
        let new_root = self.sessions[&id].root.clone();

        // Stash the outgoing session's live file explorer.
        let outgoing_explorer = self.file_explorer.take();
        if let Some(outgoing) = self.sessions.get_mut(&previous_id) {
            outgoing.file_explorer_stash = outgoing_explorer;
        }

        self.active_session = id;
        self.working_dir = new_root;

        // Restore the incoming session's stashed view, if any. A
        // never-activated session has `None` here, in which case
        // the next toggle rebuilds at the new root — same fallback
        // as the cold-swap MVP.
        if let Some(incoming) = self.sessions.get_mut(&id) {
            self.file_explorer = incoming.file_explorer_stash.take();
        }

        self.plugin_manager.run_hook(
            "active_session_changed",
            HookArgs::ActiveSessionChanged {
                previous_id: Some(previous_id.0),
                active_id: id.0,
            },
        );
    }

    /// Close a session and drop its `Session` entry. Refuses to
    /// close the currently active session — the caller must switch
    /// to a different session first. Refuses to close the base
    /// session (`SessionId(1)`) — that's the editor's anchor.
    ///
    /// Returns `true` on success, `false` on rejection.
    pub fn close_session(&mut self, id: SessionId) -> bool {
        if id == SessionId(1) {
            tracing::warn!("close_session: refusing to close the base session (id 1)");
            return false;
        }
        if id == self.active_session {
            tracing::warn!(
                "close_session: refusing to close the active session (id {id}); \
                 switch first via setActiveSession"
            );
            return false;
        }
        if self.sessions.remove(&id).is_none() {
            tracing::warn!("close_session: unknown session id {id}");
            return false;
        }

        self.plugin_manager
            .run_hook("session_closed", HookArgs::SessionClosed { id: id.0 });

        true
    }
}
