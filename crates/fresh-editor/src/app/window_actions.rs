//! Editor methods for window lifecycle (create, switch, close).
//!
//! Windows are introduced in
//! `docs/internal/orchestrator-sessions-design.md`. After Step 0b each
//! window owns its file tree, file mod-times, LSP set, panel-id
//! map, and split layout outright. `set_active_window` is therefore
//! a pointer write (plus seed-buffer allocation when diving into a
//! never-activated window) — there are no warm-swap stashes left to
//! shuffle. Plugins that listen for `active_window_changed` see the
//! same hook sequence as before.

use crate::app::window::Window;
use crate::app::window_resources::WindowResources;
use crate::services::plugins::hooks::HookArgs;
use crate::view::split::{SplitManager, SplitViewState};
use fresh_core::WindowId;
use std::collections::HashMap;
use std::path::PathBuf;

impl crate::app::Editor {
    /// Snapshot the editor-global resources every new `Window` needs.
    /// All fields are cheap clones (`Arc` increments or `Clone`-by-value
    /// where the inner type already holds `Arc`s, like `Authority`).
    /// Called by `create_window_at` and by the first-dive seed path in
    /// `set_active_window`; also by `editor_init` for the base window.
    pub(crate) fn window_resources(&self) -> WindowResources {
        WindowResources {
            config: std::sync::Arc::clone(&self.config),
            grammar_registry: std::sync::Arc::clone(&self.grammar_registry),
            theme_registry: std::sync::Arc::clone(&self.theme_registry),
            theme_cache: std::sync::Arc::clone(&self.theme_cache),
            keybindings: std::sync::Arc::clone(&self.keybindings),
            command_registry: std::sync::Arc::clone(&self.command_registry),
            fs_manager: std::sync::Arc::clone(&self.fs_manager),
            local_filesystem: std::sync::Arc::clone(&self.local_filesystem),
            buffer_id_alloc: self.buffer_id_alloc.clone(),
            authority: self.authority.clone(),
            time_source: std::sync::Arc::clone(&self.time_source),
            dir_context: self.dir_context.clone(),
            tokio_runtime: self.tokio_runtime.clone(),
            async_bridge: self.async_bridge.clone(),
            plugin_manager: std::sync::Arc::clone(&self.plugin_manager),
            theme: std::sync::Arc::clone(&self.theme),
            event_broadcaster: self.event_broadcaster.clone(),
            recovery_service: std::sync::Arc::clone(&self.recovery_service),
        }
    }

    /// Allocate a session id, insert a new `Session`, fire
    /// `session_created`. Does not switch active.
    ///
    /// Caller is responsible for ensuring `root` is absolute. The
    /// `PluginCommand::CreateWindow` dispatcher rejects relative
    /// paths before reaching here.
    ///
    /// Seeds the new window with an empty scratch buffer + a
    /// minimal split layout up front (same shape as the
    /// first-dive seed path), so the window is renderable
    /// immediately. Without this, never-dived windows have
    /// `splits == None` and any cross-window render (e.g. the
    /// Orchestrator preview pane's `WindowEmbed`) draws blank.
    pub fn create_window_at(&mut self, root: PathBuf, label: String) -> WindowId {
        let id = WindowId(self.next_window_id);
        self.next_window_id += 1;

        let resources = self.window_resources();
        let mut session = Window::new(id, label, root.clone(), resources);
        session.terminal_width = self.terminal_width;
        session.terminal_height = self.terminal_height;
        let resolved_label = session.label.clone();
        self.windows.insert(id, session);

        // Same seed shape that `set_active_window` builds on
        // first dive — installed eagerly so the window is
        // immediately renderable from any code path that walks
        // the windows map (preview rendering, embedded session
        // panes, etc.).
        if let Some((buf, state, metadata, event_log, mgr, vs)) =
            self.build_fresh_layout_if_needed(id)
        {
            if let Some(s) = self.windows.get_mut(&id) {
                s.buffers.set_splits((mgr, vs));
                s.buffers.insert(buf, state);
                s.buffer_metadata.insert(buf, metadata);
                s.event_logs.insert(buf, event_log);
            }
        }

        self.plugin_manager.read().unwrap().run_hook(
            "window_created",
            HookArgs::WindowCreated {
                id: id.0,
                label: resolved_label,
                root: root.to_string_lossy().into_owned(),
            },
        );

        id
    }

    /// Atomic "create a new window seeded with an agent terminal"
    /// entry point. Used by Orchestrator's new-session flow.
    ///
    /// Unlike `create_window_at`, this path deliberately does NOT
    /// seed an empty `[No Name]` buffer up front — the terminal
    /// becomes the window's seed via `create_plugin_terminal`'s
    /// no-active-split branch, so the new window is born with a
    /// single tab (the terminal) instead of `[No Name] | <agent>`.
    ///
    /// The eager-seed invariant `create_window_at` upholds
    /// ("window is renderable immediately after returning") still
    /// holds here: the call to `create_plugin_terminal` runs
    /// synchronously on the same thread before this function
    /// yields, installing the terminal-rooted split layout before
    /// any other code can observe the window. The `window_created`
    /// hook is intentionally fired *after* the terminal is wired
    /// up so plugin handlers see the new window in its final
    /// shape, not the half-built intermediate state.
    ///
    /// `root` must be absolute; the plugin-command dispatcher
    /// validates this before reaching here.
    pub fn create_window_with_terminal(
        &mut self,
        root: PathBuf,
        label: String,
        cwd: Option<PathBuf>,
        command: Option<Vec<String>>,
        title: Option<String>,
    ) -> Result<(WindowId, fresh_core::TerminalId, fresh_core::BufferId), String> {
        let id = WindowId(self.next_window_id);
        self.next_window_id += 1;

        let resources = self.window_resources();
        let mut session = Window::new(id, label, root.clone(), resources);
        session.terminal_width = self.terminal_width;
        session.terminal_height = self.terminal_height;
        let resolved_label = session.label.clone();
        self.windows.insert(id, session);

        // Dive into the new window before spawning the terminal
        // so `Window::create_plugin_terminal` operates on a window
        // with `splits.is_none()` — that's the "no active_split"
        // branch which seeds the layout rooted at the terminal
        // buffer. We bypass `set_active_window`'s
        // `build_fresh_layout_if_needed` call (which would install
        // a `[No Name]` seed) by writing the active-window pointer
        // directly.
        let previous_id = self.active_window;
        self.active_window = id;

        let spawn_result = {
            let target = self
                .windows
                .get_mut(&id)
                .expect("just-inserted window must be present");
            target.create_plugin_terminal(
                cwd.or_else(|| Some(root.clone())),
                None, // no split direction — let the no-layout branch seed
                None,
                true,  // focus — newly spawned terminal is the seed
                false, // ephemeral by default; orchestrator owns persistence
                command,
                title.filter(|t| !t.is_empty()),
            )
        };

        let (terminal_id, buffer_id, _split_id) = match spawn_result {
            Ok(triple) => triple,
            Err(e) => {
                // Roll back: tear down the half-built window and
                // restore the previous active pointer so the user
                // isn't stranded on an empty window when the PTY
                // spawn fails (missing binary, permission denied,
                // out of PTYs, ...).
                self.windows.remove(&id);
                self.active_window = previous_id;
                return Err(e);
            }
        };

        // Register the leader pid with the new window's
        // process_groups so window-level signal operations reach
        // the spawned group. Mirrors `create_plugin_terminal`'s
        // registration in the active-target path of
        // `handle_create_terminal`, but kept here because we
        // bypass that dispatcher.
        if let Some(pid) = self
            .windows
            .get(&id)
            .and_then(|w| w.terminal_manager.get(terminal_id))
            .and_then(|h| h.pid())
        {
            let pg_label = format!("terminal #{}", terminal_id.0);
            if let Some(win) = self.windows.get_mut(&id) {
                win.process_groups.register(pid, pg_label);
            }
        }

        // Resize the newly-active window's PTYs (mirrors
        // `set_active_window`'s post-dive resize so the seeded
        // terminal renders into the right cell rect on its first
        // frame).
        if let Some(win) = self.windows.get_mut(&id) {
            win.resize_visible_terminals();
        }

        // Plugin lifecycle: fire `window_created` first, then
        // `active_window_changed`. Order mirrors the
        // `create_window_at` + `set_active_window` sequence the
        // orchestrator previously chained — plugin handlers that
        // care about either event see the same payload order.
        self.plugin_manager.read().unwrap().run_hook(
            "window_created",
            HookArgs::WindowCreated {
                id: id.0,
                label: resolved_label,
                root: root.to_string_lossy().into_owned(),
            },
        );
        if previous_id != id {
            self.plugin_manager.read().unwrap().run_hook(
                "active_window_changed",
                HookArgs::ActiveWindowChanged {
                    previous_id: Some(previous_id.0),
                    active_id: id.0,
                },
            );
        }
        #[cfg(feature = "plugins")]
        self.update_plugin_state_snapshot();
        #[cfg(feature = "plugins")]
        self.plugin_manager.read().unwrap().run_hook(
            "buffer_activated",
            crate::services::plugins::hooks::HookArgs::BufferActivated { buffer_id },
        );

        Ok((id, terminal_id, buffer_id))
    }

    /// Switch the active window to `id`.
    ///
    /// Pointer write: every per-window field
    /// (panel_ids / file_mod_times / file_explorer / lsp / splits)
    /// already lives on `Window`, so flipping `active_window` is the
    /// whole switch. Diving into a never-activated window seeds it
    /// with a fresh empty buffer + SplitManager so the renderer
    /// finds a populated `splits` field.
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
            tracing::warn!("set_active_window: unknown window id {id}; active window unchanged");
            return;
        }

        let previous_id = self.active_window;

        // For a never-activated incoming window, allocate a fresh
        // seed buffer + SplitManager rooted at it. The state is
        // installed into the incoming window's `buffers` map after
        // the active pointer moves.
        let fresh_layout = self.build_fresh_layout_if_needed(id);

        // Pointer write — that's the whole switch. `working_dir()`
        // derives from the active window's root, so moving the pointer
        // is all it takes (no separate working_dir to sync).
        self.active_window = id;

        // For a never-activated incoming window, install the freshly
        // built layout into the window's `splits` field and attach
        // the seed buffer.
        if let Some((buf, state, metadata, event_log, mgr, vs)) = fresh_layout {
            if let Some(s) = self.windows.get_mut(&id) {
                s.buffers.set_splits((mgr, vs));
                s.buffers.insert(buf, state);
                s.buffer_metadata.insert(buf, metadata);
                s.event_logs.insert(buf, event_log);
            }
        }

        // Refresh the plugin state snapshot so `getCwd()` (and every
        // other snapshot field) reflects the window we just switched
        // to *before* the `active_window_changed` hook runs. Without
        // this, plugins that read `editor.getCwd()` — Live Grep, file
        // finders, etc. — keep targeting the previous window's project
        // after a dive, surfacing the wrong project's files.
        #[cfg(feature = "plugins")]
        self.update_plugin_state_snapshot();

        self.plugin_manager.read().unwrap().run_hook(
            "active_window_changed",
            HookArgs::ActiveWindowChanged {
                previous_id: Some(previous_id.0),
                active_id: id.0,
            },
        );

        // Resize the newly-active window's visible terminal PTYs to
        // match their dive-view split rects. Without this, a session
        // that was just previewed in the orchestrator picker
        // (`render_session_preview_into_rect` resizes PTYs to the
        // embed rect — typically ~half the terminal's height) keeps
        // drawing at that smaller size after the dive, leaving the
        // bottom of the dive view blank until something else triggers
        // a resize. Same applies for the inverse: dive away while a
        // session has a small split, dive back when the window is
        // bigger — the terminal needs the new dimensions.
        if let Some(win) = self.windows.get_mut(&id) {
            win.resize_visible_terminals();
        }
    }

    /// Cycle to the next open window in the workspace.
    ///
    /// Windows are ordered by their numeric `WindowId` (which is
    /// monotonically assigned by `create_window_at`), so "next"
    /// reads in creation order with wrap-around. No-op when only
    /// one window is open (issue #2031).
    pub fn next_window(&mut self) {
        self.cycle_active_window(1);
    }

    /// Cycle to the previous open window. See [`Self::next_window`]
    /// for ordering.
    pub fn prev_window(&mut self) {
        self.cycle_active_window(-1);
    }

    /// Step `delta` positions through the open windows (positive =
    /// forward, negative = backward), wrapping around at the ends.
    /// Centralises the cycle logic shared by `next_window` and
    /// `prev_window` so both directions stay in sync if the
    /// underlying ordering changes (e.g. user-controlled reorder).
    fn cycle_active_window(&mut self, delta: isize) {
        let mut ids: Vec<WindowId> = self.windows.keys().copied().collect();
        if ids.len() <= 1 {
            return;
        }
        ids.sort_by_key(|id| id.0);
        let current_pos = match ids.iter().position(|id| *id == self.active_window) {
            Some(pos) => pos as isize,
            None => 0,
        };
        let len = ids.len() as isize;
        let next_pos = (((current_pos + delta) % len) + len) % len;
        let next_id = ids[next_pos as usize];
        self.set_active_window(next_id);
    }

    /// Build a fresh seed buffer + split layout for `id` if that
    /// window is missing either a split tree or any buffer to back
    /// it. Returns `None` when the window is unknown or already
    /// populated. The caller is responsible for installing the
    /// returned tuple into the window's fields.
    ///
    /// Both branches (no splits, or splits but empty buffer map)
    /// are pathological: render walks the active buffer and would
    /// panic at `expect("active buffer must be present")` when the
    /// split manager points at a buffer id that isn't in
    /// `window.buffers`.
    ///
    /// Factored out of `set_active_window` so other call sites that
    /// need to populate an inert window shell can share the same
    /// seed-construction logic.
    pub(crate) fn build_fresh_layout_if_needed(
        &mut self,
        id: WindowId,
    ) -> Option<(
        fresh_core::BufferId,
        crate::state::EditorState,
        crate::app::types::BufferMetadata,
        crate::model::event::EventLog,
        SplitManager,
        HashMap<crate::model::event::LeafId, SplitViewState>,
    )> {
        if !self
            .windows
            .get(&id)
            .is_some_and(|s| s.buffers.splits().is_none() || s.buffers.len() == 0)
        {
            return None;
        }
        let buf = self.alloc_buffer_id();
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
        let metadata = crate::app::types::BufferMetadata::new();
        let event_log = crate::model::event::EventLog::new();
        let manager = SplitManager::new(buf);
        let active_leaf = manager.active_split();
        let mut view_states = HashMap::new();
        view_states.insert(
            active_leaf,
            SplitViewState::with_buffer(self.terminal_width, self.terminal_height, buf),
        );
        Some((buf, state, metadata, event_log, manager, view_states))
    }

    /// Eagerly initialise an inactive session's per-session
    /// state without diving. Useful for plugins (Orchestrator) that
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

    /// Remove a buffer from whichever window holds it. Returns the
    /// removed `EditorState` if the buffer was found. Step 0c: each
    /// buffer lives in exactly one window, so this is at most one
    /// successful removal.
    pub(crate) fn detach_buffer_from_all_windows(
        &mut self,
        buffer_id: fresh_core::BufferId,
    ) -> Option<crate::state::EditorState> {
        for w in self.windows.values_mut() {
            if let Some(state) = w.buffers.remove(&buffer_id) {
                return Some(state);
            }
        }
        None
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
            .read()
            .unwrap()
            .run_hook("window_closed", HookArgs::WindowClosed { id: id.0 });

        true
    }
}
