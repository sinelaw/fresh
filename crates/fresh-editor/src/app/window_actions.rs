//! Editor methods for session lifecycle (create, switch, close).
//!
//! Sessions are introduced in
//! `docs/internal/conductor-sessions-design.md`. The MVP build of
//! these actions implements the **plugin-visible behaviour** —
//! sessions can be created, switched, and closed, and the
//! corresponding hooks fire — but does not yet move per-subsystem
//! state (file tree, LSP, ignore matcher) into `Session`. As a
//! result, `set_active_window` here updates `working_dir` and
//! discards the cached file explorer (so it rebuilds on next open),
//! but warm-LSP swap and warm-file-tree swap are deferred to the
//! per-subsystem migration commits.
//!
//! Plugins that listen for `active_session_changed` see the same
//! sequence regardless of whether the swap is warm or cold; the
//! difference is performance only.

use crate::app::window::Window;
use crate::services::plugins::hooks::HookArgs;
use crate::view::split::{SplitManager, SplitViewState};
use fresh_core::{BufferId, WindowId};
use std::collections::HashMap;
use std::path::PathBuf;

impl crate::app::Editor {
    /// Allocate a session id, insert a new `Session`, fire
    /// `session_created`. Does not switch active.
    ///
    /// Caller is responsible for ensuring `root` is absolute. The
    /// `PluginCommand::CreateWindow` dispatcher rejects relative
    /// paths before reaching here.
    pub fn create_window_at(&mut self, root: PathBuf, label: String) -> WindowId {
        let id = WindowId(self.next_window_id);
        self.next_window_id += 1;

        let session = Window::new(id, label, root.clone());
        let resolved_label = session.label.clone();
        self.windows.insert(id, session);

        self.plugin_manager.run_hook(
            "window_created",
            HookArgs::WindowCreated {
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
    /// bug (caller verifies with `listWindows`), not a recoverable
    /// error worth surfacing through the channel.
    pub fn set_active_window(&mut self, id: WindowId) {
        if self.active_window == id {
            return;
        }
        if !self.windows.contains_key(&id) {
            tracing::warn!("set_active_window: unknown session id {id}; active session unchanged");
            return;
        }

        let previous_id = self.active_window;

        // Snapshot the new root before mutating fields that borrow
        // self.windows.
        let new_root = self.windows[&id].root.clone();

        let needs_fresh_layout = self
            .windows
            .get(&id)
            .is_some_and(|s| s.splits_stash.is_none());

        // For a never-activated incoming session, allocate a
        // fresh seed buffer + a SplitManager rooted at it
        // BEFORE we touch `Editor.split_manager`. We deliberately
        // build the buffer state directly (not via `new_buffer`)
        // so the outgoing session's split manager — still
        // installed in `self.split_manager` — is not mutated.
        // After the swap below, the active session is the
        // incoming one and the seed buffer attaches to it.
        let fresh_layout = if needs_fresh_layout {
            let buf = BufferId(self.next_buffer_id);
            self.next_buffer_id += 1;
            let mut state = crate::state::EditorState::new(
                self.terminal_width,
                self.terminal_height,
                self.config.editor.large_file_threshold_bytes as usize,
                std::sync::Arc::clone(&self.authority.filesystem),
            );
            state
                .margins
                .configure_for_line_numbers(self.config.editor.line_numbers);
            state
                .buffer
                .set_default_line_ending(self.config.editor.default_line_ending.to_line_ending());
            self.buffers.insert(buf, state);
            // Skip `attach_buffer_to_active_window` — at this
            // point `active_window` is still the outgoing one.
            // We attach to the incoming session below, after the
            // active pointer moves.
            self.event_logs
                .insert(buf, crate::model::event::EventLog::new());
            self.buffer_metadata
                .insert(buf, crate::app::types::BufferMetadata::new());
            let manager = SplitManager::new(buf);
            let active_leaf = manager.active_split();
            let mut view_states = HashMap::new();
            view_states.insert(
                active_leaf,
                SplitViewState::with_buffer(self.terminal_width, self.terminal_height, buf),
            );
            Some((buf, manager, view_states))
        } else {
            None
        };

        // Stash the outgoing session's live state.
        let outgoing_explorer = self.file_explorer.take();
        let outgoing_panel_ids = std::mem::take(&mut self.panel_ids);
        let outgoing_lsp = self.lsp.take();
        let outgoing_mtimes = std::mem::take(&mut self.file_mod_times);
        let outgoing_splits = std::mem::replace(
            &mut self.split_manager,
            SplitManager::new(BufferId(usize::MAX)),
        );
        let outgoing_view_states = std::mem::take(&mut self.split_view_states);
        if let Some(outgoing) = self.windows.get_mut(&previous_id) {
            outgoing.file_explorer_stash = outgoing_explorer;
            outgoing.panel_ids_stash = outgoing_panel_ids;
            outgoing.lsp_stash = outgoing_lsp;
            outgoing.file_mod_times_stash = outgoing_mtimes;
            outgoing.splits_stash = Some((outgoing_splits, outgoing_view_states));
        }

        self.active_window = id;
        self.working_dir = new_root;

        // Restore the incoming session's stashed state. Buffers,
        // file explorer, LSP set, mtime cache.
        if let Some(incoming) = self.windows.get_mut(&id) {
            self.file_explorer = incoming.file_explorer_stash.take();
            self.panel_ids = std::mem::take(&mut incoming.panel_ids_stash);
            self.lsp = incoming.lsp_stash.take();
            self.file_mod_times = std::mem::take(&mut incoming.file_mod_times_stash);
            if let Some((mgr, vs)) = incoming.splits_stash.take() {
                self.split_manager = mgr;
                self.split_view_states = vs;
            }
        }

        // For a never-activated incoming session, install the
        // freshly-built layout and attach the seed buffer to the
        // (now-active) incoming session.
        if let Some((buf, mgr, vs)) = fresh_layout {
            self.split_manager = mgr;
            self.split_view_states = vs;
            if let Some(s) = self.windows.get_mut(&id) {
                s.buffers.insert(buf);
            }
        }

        self.plugin_manager.run_hook(
            "active_window_changed",
            HookArgs::ActiveWindowChanged {
                previous_id: Some(previous_id.0),
                active_id: id.0,
            },
        );
    }

    /// Eagerly initialise an inactive session's per-session
    /// state without diving. Useful for plugins (Conductor) that
    /// want to pay the warm-up cost (file-tree walk, ignore
    /// matcher, etc.) ahead of the user's first dive.
    ///
    /// In the current build this is a placeholder — file
    /// explorer rebuilds and LSP boot still happen on first dive.
    /// The API exists so callers don't have to be rewritten when
    /// eager warm-up wires up later.
    pub fn prewarm_window(&mut self, id: WindowId) {
        if id == self.active_window {
            return;
        }
        if !self.windows.contains_key(&id) {
            tracing::warn!("prewarm_window: unknown session id {id}");
        }
        // Placeholder for eager warm-up of file_explorer / LSP.
    }

    /// Attach a buffer to the active session's membership set.
    /// Called from every `Editor.buffers.insert` site so the
    /// `Session.buffers` field stays in sync. Idempotent.
    pub(crate) fn attach_buffer_to_active_window(&mut self, buffer_id: fresh_core::BufferId) {
        let id = self.active_window;
        if let Some(s) = self.windows.get_mut(&id) {
            s.buffers.insert(buffer_id);
        }
    }

    /// Detach a buffer from every session's membership set.
    /// Called from buffer-close sites. Cheap when the buffer was
    /// only attached to one session (the common case).
    pub(crate) fn detach_buffer_from_all_windows(&mut self, buffer_id: fresh_core::BufferId) {
        for s in self.windows.values_mut() {
            s.buffers.remove(&buffer_id);
        }
    }

    /// Close a session and drop its `Session` entry. Refuses to
    /// close the currently active session — the caller must switch
    /// to a different session first. Refuses to close the base
    /// session (`WindowId(1)`) — that's the editor's anchor.
    ///
    /// Returns `true` on success, `false` on rejection.
    pub fn close_window(&mut self, id: WindowId) -> bool {
        if id == WindowId(1) {
            tracing::warn!("close_window: refusing to close the base session (id 1)");
            return false;
        }
        if id == self.active_window {
            tracing::warn!(
                "close_window: refusing to close the active session (id {id}); \
                 switch first via setActiveWindow"
            );
            return false;
        }
        if self.windows.remove(&id).is_none() {
            tracing::warn!("close_window: unknown session id {id}");
            return false;
        }

        self.plugin_manager
            .run_hook("window_closed", HookArgs::WindowClosed { id: id.0 });

        true
    }
}
