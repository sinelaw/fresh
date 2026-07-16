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

/// Seed state for a freshly-built window layout: the base buffer plus the
/// editor/metadata/event-log/split scaffolding returned by
/// [`Editor::build_fresh_layout_if_needed`].
type FreshLayoutSeed = (
    fresh_core::BufferId,
    crate::state::EditorState,
    crate::app::types::BufferMetadata,
    crate::model::event::EventLog,
    SplitManager,
    HashMap<crate::model::event::LeafId, SplitViewState>,
);

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
            // Derive the window's fs_manager from the *same* authority we hand
            // it below, so directory listings (the file explorer) ride the
            // window's filesystem — local or remote — instead of a stale,
            // boot-time local one. A born-attached SSH/k8s window otherwise
            // showed the local machine in the explorer while its terminal ran
            // remote, because the cached fs_manager never tracked the authority.
            // Default to a host fs_manager; callers that build a window with a
            // non-local authority re-derive it from that authority's
            // filesystem. The window's `authority` is set on the `Window`
            // itself (not here in the `Clone`-fanned resources).
            fs_manager: std::sync::Arc::new(crate::services::fs::FsManager::new(
                std::sync::Arc::new(crate::model::filesystem::StdFileSystem),
            )),
            local_filesystem: std::sync::Arc::clone(&self.local_filesystem),
            buffer_id_alloc: self.buffer_id_alloc.clone(),
            time_source: std::sync::Arc::clone(&self.time_source),
            dir_context: self.dir_context.clone(),
            tokio_runtime: self.tokio_runtime.clone(),
            async_bridge: self.async_bridge.clone(),
            plugin_manager: std::rc::Rc::clone(&self.plugin_manager),
            theme: std::sync::Arc::clone(&self.theme),
            event_broadcaster: self.event_broadcaster.clone(),
            recovery_service: std::sync::Arc::clone(&self.recovery_service),
            mouse_capture: std::sync::Arc::clone(&self.mouse_capture),
        }
    }

    /// Allocate a session id, insert a new `Session`, fire
    /// `session_created`. Does not switch active.
    ///
    /// Caller is responsible for ensuring `root` is absolute. The
    /// `PluginCommand::CreateWindow` dispatcher rejects relative
    /// paths before reaching here.
    ///
    /// Find an existing window whose root resolves to the same
    /// canonical directory, if any. Backs the one-session-per-dir
    /// invariant: opening a directory that already has a window
    /// reuses it rather than creating a duplicate.
    pub(crate) fn find_window_by_root(&self, root: &std::path::Path) -> Option<WindowId> {
        let key = crate::app::orchestrator_persistence::canonical_key(root);
        self.windows
            .iter()
            .find(|(_, w)| crate::app::orchestrator_persistence::canonical_key(&w.root) == key)
            .map(|(id, _)| *id)
    }

    /// Open the window for `root`, creating it if absent. Enforces
    /// one-session-per-directory: if a window already exists at the
    /// same canonical root it is returned as-is and `label` is
    /// ignored (the existing window keeps its label) — no duplicate
    /// is created.
    ///
    /// Seeds a freshly created window with an empty scratch buffer +
    /// a minimal split layout up front (same shape as the first-dive
    /// seed path), so the window is renderable immediately. Without
    /// this, never-dived windows have `splits == None` and any
    /// cross-window render (e.g. the Orchestrator preview pane's
    /// `WindowEmbed`) draws blank.
    pub fn create_window_at(&mut self, root: PathBuf, label: String) -> WindowId {
        // One session per directory: reuse an existing window at this
        // root instead of spawning a colliding duplicate.
        if let Some(existing) = self.find_window_by_root(&root) {
            return existing;
        }
        // A new window for `root` is its own local session with its **own**
        // per-session trust scoped to that root — not a clone of the active
        // session's authority/trust (which would leak a trust decision across
        // projects). Its `fs_manager` rides the same (host) filesystem.
        let local_authority = self.local_session_authority(&root);
        self.create_window_with_authority(root, label, local_authority)
    }

    /// Create a new window rooted at `root` under an explicit `authority`,
    /// seeded with an empty scratch buffer + minimal split layout (so it is
    /// renderable immediately) and announced via `window_created`.
    ///
    /// Unlike [`Self::create_window_at`] this does **not** dedup by root or
    /// mint a fresh local authority — the caller supplies the backend. That
    /// lets Switch Project (`change_working_dir`) carry a remote window's
    /// already-connected authority onto the new project so the new root opens
    /// on the same container / SSH host, while local callers pass a fresh
    /// [`Self::local_session_authority`].
    pub(crate) fn create_window_with_authority(
        &mut self,
        root: PathBuf,
        label: String,
        authority: crate::services::authority::Authority,
    ) -> WindowId {
        let id = WindowId(self.next_window_id);
        self.next_window_id += 1;

        let mut resources = self.window_resources();
        resources.fs_manager = std::sync::Arc::new(crate::services::fs::FsManager::new(
            std::sync::Arc::clone(&authority.filesystem),
        ));
        let mut session = Window::new(id, label, root.clone(), authority, resources);
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

    /// A fresh per-session execution scope (trust + env) for `root` — the one
    /// blessed factory. Each call mints handles owned by exactly one session,
    /// so a trust decision or env activation in one window can never leak into
    /// another. Every per-session window construction goes through this.
    pub(crate) fn session_scope_for(
        &self,
        root: &std::path::Path,
    ) -> crate::services::authority::SessionScope {
        crate::services::authority::SessionScope::for_root(
            root,
            &self.dir_context.project_state_dir(root),
        )
    }

    /// A fresh local authority for a brand-new session rooted at `root`, with
    /// its **own** per-session trust + env (not clones of the active
    /// session's) and a host filesystem. The canonical backend for the
    /// Orchestrator's "New Session (Local)" flow: callers pass this to
    /// [`Self::create_window_with_terminal`] so a new session for a different
    /// project is born under its own local backend, trust, and env.
    pub fn local_session_authority(
        &self,
        root: &std::path::Path,
    ) -> crate::services::authority::Authority {
        crate::services::authority::Authority::local_scoped(self.session_scope_for(root))
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
    ///
    /// `authority` is the backend the new session is born under — passed
    /// explicitly so this primitive never guesses. The Orchestrator's "New
    /// Session (Local)" flow hands it [`Self::local_session_authority`] (a
    /// fresh local backend sharing the editor's trust + env handles), so a
    /// new session for a *different* project does not inherit the active
    /// window's container/SSH/k8s backend just because that window was
    /// focused when "+ New" was clicked. The born-attached remote-session
    /// path (`create_remote_session_window`) passes its already-connected
    /// backend so the new window's filesystem, LSP spawner, and terminal all
    /// act remotely from birth.
    ///
    /// The editor-wide authority cache is re-pointed at the new active
    /// window via [`Self::adopt_active_window_authority`] before returning,
    /// so the status bar, quick-open, and the 100+ `self.authority` call
    /// sites reflect the session the user just landed on rather than the one
    /// they left.
    ///
    /// `resume` is the agent-resume argv to re-run instead of `command` if
    /// this session is restored (Orchestrator agent-resume).
    #[allow(clippy::too_many_arguments)]
    pub fn create_window_with_terminal(
        &mut self,
        root: PathBuf,
        label: String,
        cwd: Option<PathBuf>,
        command: Option<Vec<String>>,
        title: Option<String>,
        window_authority: crate::services::authority::Authority,
        resume: Option<Vec<String>>,
    ) -> Result<(WindowId, fresh_core::TerminalId, fresh_core::BufferId), String> {
        let id = WindowId(self.next_window_id);
        self.next_window_id += 1;

        // The backend the editor was acting through before this new
        // session — captured so `adopt_active_window_authority` can tell
        // whether the active authority actually changed and skip the
        // hook/snapshot churn when it didn't.
        let previous_authority_label = self.authority().display_label.clone();

        let mut resources = self.window_resources();
        // Re-derive the window's `fs_manager` from *its* backend's filesystem
        // so the file explorer rides this session's backend, then build the
        // window owning `window_authority` outright.
        resources.fs_manager = std::sync::Arc::new(crate::services::fs::FsManager::new(
            std::sync::Arc::clone(&window_authority.filesystem),
        ));
        let mut session = Window::new(id, label, root.clone(), window_authority, resources);
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

        // Run the workspace-trust decision for the project this session opens,
        // exactly as the CLI / session-server startup paths do — the
        // orchestrator's "New Session" path bypasses those, so without this a
        // never-decided project opened through the dock stays Restricted and
        // its env manager (venv / direnv / mise) never activates, leaving the
        // terminal below on the *system* toolchain even though a direct
        // `fresh <dir>` would have auto-trusted and activated it. `authority()`
        // already follows `active_window` (set just above), so this scopes to
        // the new window's trust + root. Local only: remote (born-attached
        // SSH/k8s) sessions manage trust through their own connect flow and
        // their markers don't live on the host filesystem.
        if self
            .authority()
            .filesystem
            .remote_connection_info()
            .is_none()
        {
            // Activating a new Orchestrator session on a running editor:
            // secondary is Cancel, not Quit — dismissing the prompt must leave
            // the new session Restricted, never tear down the editor and the
            // other open sessions.
            self.maybe_prompt_workspace_trust(true);
        }

        // The argv to re-run if this session is restored. `None` (plain
        // shell) is recorded as an empty vec: a present entry — even empty —
        // marks this as a restorable *session* terminal (re-spawn it on
        // restore), distinct from a throwaway ephemeral build/exec shell.
        let restore_command = command.clone().unwrap_or_default();
        let spawn_result = {
            let target = self
                .windows
                .get_mut(&id)
                .expect("just-inserted window must be present");
            target.create_plugin_terminal(crate::app::terminal::PluginTerminalSpec {
                cwd: cwd.or_else(|| Some(root.clone())),
                direction: None, // no split direction — let the no-layout branch seed
                ratio: None,
                focus: true,       // newly spawned terminal is the seed
                persistent: false, // ephemeral by default; orchestrator owns persistence
                command,
                title: title.filter(|t| !t.is_empty()),
            })
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

        // Mark the freshly-spawned agent terminal restorable so workspace
        // capture persists it (with its command) and a later launch
        // re-runs it, instead of the session coming back as a blank pane.
        // An explicit `resume` argv (agent-resume) supersedes the launch
        // command on restore — see `restore_terminal_from_workspace`.
        if let Some(target) = self.windows.get_mut(&id) {
            target
                .terminal_commands
                .insert(terminal_id, restore_command);
            if let Some(resume_argv) = resume.filter(|a| !a.is_empty()) {
                target
                    .terminal_resume_commands
                    .insert(terminal_id, resume_argv);
            }
        }

        // The switch has now committed (the spawn succeeded and the active
        // pointer stays on the new window). This path wrote `active_window`
        // directly above, bypassing `set_active_window` — so mirror its
        // guard here, or a panel-scoped mode set on the window we switched
        // away from (e.g. the New-Session form's `orchestrator-new-form`,
        // still mounted during a born-attached SSH/K8s attach) is left
        // stranded and silently swallows all of that window's buffer input.
        // See #2237 / #2234 item 4.
        self.clear_panel_scoped_mode_on_switch_away(previous_id);

        // Adopt the new active window's authority into the editor-wide
        // caches (`self.authority`, quick-open, the `authority_changed`
        // hook). This path writes `active_window` directly and bypasses
        // `set_active_window`, so without this the status bar + the 100+
        // `self.authority` call sites keep reporting the *previous*
        // window's backend — e.g. a new local session created from a
        // devcontainer window would still show `Container:…` and route
        // file ops through the container. The window's own
        // `resources.authority` was already set above (local by default,
        // or the explicit remote backend for born-attached sessions).
        self.adopt_active_window_authority(&previous_authority_label);

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

        // Size the newly-created window's PTYs (mirrors
        // `set_active_window`'s post-dive resize so the seeded terminal
        // renders into the right cell rect on its first frame). Route
        // through the funnel rather than `win.resize_visible_terminals()`
        // directly: a brand-new window's `dock_cols` cache is still 0, and
        // `relayout` pushes the current editor-global dock width into every
        // window before sizing, so the seeded terminal accounts for a dock
        // that's already showing.
        self.relayout();

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

    /// Clear a floating-panel-scoped editor mode on the window we are
    /// switching *away* from.
    ///
    /// A plugin-defined editor mode (`editor.setEditorMode`) tied to a mounted
    /// floating widget panel — the Orchestrator picker (`orchestrator-open`) or
    /// new-session form (`orchestrator-new-form`) — is transient UI state that
    /// belongs to the *panel*, not to the window it was opened over.
    /// `setEditorMode` writes to whatever window is active when the plugin
    /// calls it, so a plugin that switches the active window while its panel is
    /// still mounted (the orchestrator "dive": `setActiveWindow(target)` first,
    /// then `closeOpenDialog()` / `closeForm()` which runs
    /// `setEditorMode(null)`) lands the clear on the *incoming* window and
    /// leaves the *outgoing* one stuck in the panel's mode. That stuck mode
    /// stays masked while the window sits in terminal mode, then silently
    /// swallows every printable key the moment the user leaves terminal mode
    /// (e.g. opens a file via quick-open) — the buffer ignores all keyboard
    /// input until the user switches sessions.
    ///
    /// Both window-switch paths must call this before moving the active
    /// pointer: the ordinary `set_active_window` dive *and* the born-attached
    /// remote session creation (`create_window_with_terminal`), which writes
    /// the active pointer directly and so never reaches `set_active_window`'s
    /// own guard. See #2237 / #2234 item 4.
    ///
    /// vi-mode and other persistent per-window modes are unaffected: they never
    /// have a floating panel mounted during a window switch.
    fn clear_panel_scoped_mode_on_switch_away(&mut self, previous_id: WindowId) {
        if self.floating_widget_panel.is_some() {
            if let Some(win) = self.windows.get_mut(&previous_id) {
                win.editor_mode = None;
            }
        }
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

        // Checkpoint the outgoing window's workspace before we leave it, so a
        // later kill can't lose its layout. Switching away is the natural
        // save point — the window is fully materialized and its state is now
        // final until the user returns to it. (No-op for an unmaterialized
        // seed or a window with no splits — see `checkpoint_window_workspace`.)
        self.checkpoint_window_workspace(previous_id);

        // Capture the outgoing backend label so we can tell, after the
        // switch, whether the active *authority* actually changed (most
        // window switches are between same-authority local sessions, where
        // it doesn't). Only then do we re-point editor-wide caches + fire
        // the `authority_changed` hook.
        let previous_authority_label = self.authority().display_label.clone();

        // Clear any panel-scoped editor mode on the window we're leaving so
        // it can never outlive the switch (see
        // `clear_panel_scoped_mode_on_switch_away`).
        self.clear_panel_scoped_mode_on_switch_away(previous_id);

        // Lazy materialization: if this window's saved workspace hasn't
        // been restored yet, restore it now (before seeding) so the
        // dive lands on real content rather than an empty buffer.
        self.materialize_window(id);

        // For a never-activated incoming window, allocate a fresh
        // seed buffer + SplitManager rooted at it. The state is
        // installed into the incoming window's `buffers` map after
        // the active pointer moves. After a successful materialize the
        // window already has splits, so this is a no-op.
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

        // Authority follows the active window. Each `Window` owns its
        // `resources.authority`; the editor-wide `self.authority` cache (read
        // by the 100+ filesystem/spawn/terminal call sites) must now reflect
        // the window we just switched to, or a per-session remote/cloud
        // backend would silently keep acting through the previous window's
        // authority. This is the switch-time counterpart to
        // `set_session_authority` (which mirrors on swap of the *active*
        // window) — see `AUTHORITY_DESIGN.md` §"Evolution: per-session
        // authority". Cheap for the common case: same-authority local windows
        // share `Arc`s and the label is unchanged, so the hook below is
        // skipped.
        self.adopt_active_window_authority(&previous_authority_label);

        // If we just switched to a remote session that came back from disk
        // dormant (backend spec known, live authority still the local
        // placeholder), start reconnecting its backend now — the per-window
        // activation the per-session design calls for. SSH/k8s reconnect from
        // core; the agent terminals re-run in the live backend once it lands.
        #[cfg(feature = "plugins")]
        self.reconnect_dormant_session_if_needed(id);

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

        // Bring `terminal_mode` in line with the incoming window's active
        // buffer, exactly as the tab-switch path (`set_active_buffer`) does.
        // A window whose active buffer is a *restored* terminal comes back
        // with that buffer marked `Live` (see
        // `restore_terminals_from_workspace`) but its window-level
        // `terminal_mode` flag defaulted to `false` and the buffer left
        // read-only — the window switch never touched either. Without this
        // sync the first dive into such a session after an editor restart
        // lands on the read-only scrollback view instead of the live
        // terminal, and the user has to type (or wait for new output) to
        // wake it. Diving is a focus change just like a tab switch, so it
        // must route through the same single mode authority. A terminal the
        // user had explicitly dropped to Scrollback stays read-only (its
        // remembered mode isn't `Live`), so this only revives genuinely-live
        // terminals.
        self.sync_terminal_mode_to_active_buffer();

        // Reflow the newly-active window's visible terminal PTYs to
        // match their dive-view split rects. Without this, a session
        // that was just previewed in the orchestrator picker
        // (`render_session_preview_into_rect` resizes PTYs to the
        // embed rect — typically ~half the terminal's height) keeps
        // drawing at that smaller size after the dive, leaving the
        // bottom of the dive view blank until something else triggers
        // a resize. Same applies for the inverse: dive away while a
        // session has a small split, dive back when the window is
        // bigger — the terminal needs the new dimensions. Route through
        // the funnel so the dive-target window also picks up the current
        // editor-global dock width (its `dock_cols` cache may be stale).
        self.relayout();
    }

    /// Switch the active window and play a directional wipe over the
    /// editor content as the incoming window appears. The editor
    /// content geometry is layout-driven (identical for any session),
    /// so the outgoing window's last content rect is the right area to
    /// animate. `capture_before_all` snapshots the previous frame (the
    /// outgoing window) and `SlideIn` slides the new content in over it.
    pub fn set_active_window_animated(&mut self, id: WindowId, from_edge: &str) {
        let animate = self.active_window != id
            && self.windows.contains_key(&id)
            && self.config().editor.animations;
        // Wipe the ENTIRE window — menu bar, explorer, tabs, splits, and
        // status bar — i.e. everything to the right of the dock. That's
        // the chrome area from the dock split, not just the buffer's
        // content rect. The dock column itself stays put.
        let full = ratatui::layout::Rect {
            x: 0,
            y: 0,
            width: self.terminal_width,
            height: self.terminal_height,
        };
        let (_dock, area) = self.compute_dock_split(full);
        self.set_active_window(id);
        if !animate {
            return;
        }
        if area.width == 0 || area.height == 0 {
            return;
        }
        use crate::view::animation::{AnimationKind, Edge};
        let from = match from_edge {
            "top" => Edge::Top,
            "bottom" => Edge::Bottom,
            "left" => Edge::Left,
            "right" => Edge::Right,
            _ => Edge::Bottom,
        };
        self.active_window_mut().animations.start(
            area,
            AnimationKind::SlideIn {
                from,
                duration: std::time::Duration::from_millis(180),
                delay: std::time::Duration::ZERO,
            },
        );
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
        // A plugin (the orchestrator dock) may constrain cycling to a
        // specific ordered subset — the windows currently visible in its
        // session list — so Next/Prev Window walks exactly that list rather
        // than every open window. Ids no longer open are dropped, preserving
        // the given order. An empty result (or no override) falls back to the
        // default: every window, ordered by id.
        let override_ids: Option<Vec<WindowId>> = self
            .window_cycle_order
            .as_ref()
            .map(|order| {
                order
                    .iter()
                    .copied()
                    .filter(|id| self.windows.contains_key(id))
                    .collect::<Vec<_>>()
            })
            .filter(|kept| !kept.is_empty());
        let ids: Vec<WindowId> = match override_ids {
            Some(kept) => kept,
            None => {
                let mut all: Vec<WindowId> = self.windows.keys().copied().collect();
                all.sort_by_key(|id| id.0);
                all
            }
        };
        if ids.len() <= 1 {
            return;
        }
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
    pub(crate) fn build_fresh_layout_if_needed(&mut self, id: WindowId) -> Option<FreshLayoutSeed> {
        if !self
            .windows
            .get(&id)
            .is_some_and(|s| s.buffers.splits().is_none() || s.buffers.is_empty())
        {
            return None;
        }
        let buf = self.alloc_buffer_id();
        let mut state = crate::state::EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            std::sync::Arc::clone(&self.authority().filesystem),
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

    /// Move a tab's buffer into a new orchestrator workspace (a `Window`)
    /// and switch to it. File-backed tabs root the new workspace at the
    /// file's parent directory; terminal tabs root it at the shell's current
    /// working directory (the live PTY moves along — the running process is
    /// untouched).
    ///
    /// A buffer that is neither file-backed nor a terminal has no directory
    /// to root the new workspace at, so the extraction is refused with a
    /// status message. If a workspace already exists at that root the buffer
    /// moves into it instead (one-session-per-directory invariant); if that
    /// workspace is the *current* one there is nowhere to extract to and a
    /// status message says so.
    ///
    /// The live `EditorState` moves — unsaved modifications and undo history
    /// travel with the tab rather than being re-read from disk.
    pub fn extract_tab_to_new_workspace(&mut self, buffer_id: fresh_core::BufferId) {
        use rust_i18n::t;

        if self.active_window().is_terminal_buffer(buffer_id) {
            self.extract_terminal_tab_to_new_workspace(buffer_id);
            return;
        }

        let path = self
            .buffers()
            .get(&buffer_id)
            .and_then(|state| state.buffer.file_path().map(|p| p.to_path_buf()));
        let Some(path) = path else {
            self.set_status_message(t!("workspace.extract_no_file_path").to_string());
            return;
        };
        let root = match path.parent() {
            Some(p) if !p.as_os_str().is_empty() => p.to_path_buf(),
            _ => {
                self.set_status_message(t!("workspace.extract_no_file_path").to_string());
                return;
            }
        };

        if self.find_window_by_root(&root) == Some(self.active_window) {
            self.set_status_message(
                t!(
                    "workspace.extract_already_rooted",
                    root = root.display().to_string()
                )
                .to_string(),
            );
            return;
        }

        // Re-point every visible leaf that displays this buffer at another
        // of its tabs before the move, so the source window's split tree
        // never dangles on a buffer it no longer owns.
        self.retarget_leaves_off_buffer(buffer_id);

        let label = root
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| root.to_string_lossy().into_owned());
        let target = self.create_window_at(root, label);
        self.handle_open_file_in_inactive_session(target, path.clone());

        let target_label = self
            .windows
            .get(&target)
            .map(|w| w.label.clone())
            .unwrap_or_default();
        self.set_active_window(target);
        self.set_active_buffer(buffer_id);

        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| path.to_string_lossy().into_owned());
        self.set_status_message(
            t!("workspace.extracted_tab", name = name, label = target_label).to_string(),
        );
    }

    /// Terminal-tab body of [`Self::extract_tab_to_new_workspace`]: root the
    /// new workspace at the shell's current working directory and move the
    /// live terminal — PTY handle, backing/log files, launch/resume argv,
    /// and process-group registration — to the new window alongside the
    /// buffer. The running process is untouched; its output threads are
    /// retagged so the stream follows it (`TerminalManager::adopt`).
    fn extract_terminal_tab_to_new_workspace(&mut self, buffer_id: fresh_core::BufferId) {
        use rust_i18n::t;

        let win = self.active_window();
        let Some(terminal_id) = win
            .terminal_buffers
            .get(&buffer_id)
            .map(|tb| tb.terminal_id)
        else {
            return;
        };
        // A binding without a PTY handle is a dormant remote shell waiting
        // for reconnect — there is nothing live to move.
        let Some(handle) = win.terminal_manager.get(terminal_id) else {
            self.set_status_message(t!("workspace.extract_terminal_dormant").to_string());
            return;
        };
        // Root at where the user has `cd`'d to, not where the terminal was
        // spawned; fall back to the spawn cwd, then the window root (which
        // the already-rooted guard below then refuses).
        let root = handle
            .current_working_dir()
            .unwrap_or_else(|| win.root.clone());

        if self.find_window_by_root(&root) == Some(self.active_window) {
            self.set_status_message(
                t!(
                    "workspace.extract_already_rooted",
                    root = root.display().to_string()
                )
                .to_string(),
            );
            return;
        }

        // The tab title (OSC/explicit/fg-command derived) — captured before
        // the move while this window can still resolve it.
        let name = self.get_buffer_display_name(buffer_id);

        self.retarget_leaves_off_buffer(buffer_id);

        let label = root
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| root.to_string_lossy().into_owned());
        let target = self.create_window_at(root, label);

        self.move_terminal_machinery_to_window(buffer_id, terminal_id, target);
        self.move_buffer_membership_to_window(buffer_id, target);

        let target_label = self
            .windows
            .get(&target)
            .map(|w| w.label.clone())
            .unwrap_or_default();
        self.set_active_window(target);
        self.set_active_buffer(buffer_id);
        // Focus changes that bypass the usual tab-click path must restore
        // terminal mode themselves, and the PTY must match its new split.
        self.sync_terminal_mode_to_active_buffer();
        self.active_window_mut().resize_visible_terminals();

        self.set_status_message(
            t!("workspace.extracted_tab", name = name, label = target_label).to_string(),
        );
    }

    /// Move every piece of per-terminal state for `buffer_id`'s terminal
    /// from the active window to `target`: the PTY handle (adopted under a
    /// fresh id, since terminal ids are per-window), the backing/log file
    /// bindings, launch/resume argv, the ephemeral flag, title/fg-name
    /// caches, and the process-group registration. Mirrors the remap loop
    /// in `respawn_terminals_through_authority`, which is the same
    /// "terminal changes identity" bookkeeping within one window.
    fn move_terminal_machinery_to_window(
        &mut self,
        buffer_id: fresh_core::BufferId,
        terminal_id: crate::services::terminal::TerminalId,
        target: WindowId,
    ) {
        let source = self.active_window;
        if source == target {
            return;
        }

        let Some(src) = self.windows.get_mut(&source) else {
            return;
        };
        if src.terminal_buffers.remove(&buffer_id).is_none() {
            return;
        }
        let Some(handle) = src.terminal_manager.release(terminal_id) else {
            return;
        };
        let backing = src.terminal_backing_files.remove(&terminal_id);
        let log = src.terminal_log_files.remove(&terminal_id);
        let command = src.terminal_commands.remove(&terminal_id);
        let resume = src.terminal_resume_commands.remove(&terminal_id);
        let ephemeral = src.ephemeral_terminals.remove(&terminal_id);
        let explicit_title = src.terminal_explicit_titles.remove(&buffer_id);
        let fg_name = src.terminal_fg_cache.remove(&buffer_id);
        let pid = handle.pid();
        if let Some(pid) = pid {
            src.process_groups.forget(pid);
        }

        let Some(tgt) = self.windows.get_mut(&target) else {
            return;
        };
        let new_id = tgt.terminal_manager.adopt(handle);
        // A fresh live binding: the old scrollback-split set referenced the
        // source window's leaves, which mean nothing in the target.
        tgt.terminal_buffers.insert(
            buffer_id,
            crate::app::window::TerminalBuffer::new_live(new_id),
        );
        if let Some(p) = backing {
            tgt.terminal_backing_files.insert(new_id, p);
        }
        if let Some(p) = log {
            tgt.terminal_log_files.insert(new_id, p);
        }
        if let Some(c) = command {
            tgt.terminal_commands.insert(new_id, c);
        }
        if let Some(c) = resume {
            tgt.terminal_resume_commands.insert(new_id, c);
        }
        if ephemeral {
            tgt.ephemeral_terminals.insert(new_id);
        }
        if explicit_title {
            tgt.terminal_explicit_titles.insert(buffer_id);
        }
        if let Some(n) = fg_name {
            tgt.terminal_fg_cache.insert(buffer_id, n);
        }
        if let Some(pid) = pid {
            tgt.process_groups
                .register(pid, format!("terminal #{}", new_id.0));
        }
    }

    /// Switch every leaf of the active window that currently displays
    /// `buffer_id` to a different tab, closing the leaf (or seeding a fresh
    /// scratch buffer when it is the last one) when it has no other tab to
    /// fall back to. Prepares a buffer for extraction to another window.
    fn retarget_leaves_off_buffer(&mut self, buffer_id: fresh_core::BufferId) {
        use crate::view::split::TabTarget;

        let Some((mgr, view_states)) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
        else {
            return;
        };

        // (leaf, fallback tab) for every leaf whose displayed buffer is the
        // one being extracted. The rect is a probe — only ids matter here.
        let probe = ratatui::layout::Rect::new(0, 0, 1, 1);
        let showing: Vec<(crate::model::event::LeafId, Option<fresh_core::BufferId>)> = mgr
            .root()
            .get_leaves_with_rects(probe)
            .into_iter()
            .filter(|(_, displayed, _)| *displayed == buffer_id)
            .map(|(leaf_id, _, _)| {
                let replacement = view_states.get(&leaf_id).and_then(|vs| {
                    vs.open_buffers.iter().find_map(|t| match t {
                        TabTarget::Buffer(id) if *id != buffer_id => Some(*id),
                        _ => None,
                    })
                });
                (leaf_id, replacement)
            })
            .collect();

        for (leaf_id, replacement) in showing {
            if let Some(replacement) = replacement {
                self.active_window_mut()
                    .set_pane_buffer(leaf_id, replacement);
                continue;
            }
            let leaf_count = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(mgr, _)| mgr.root().count_leaves())
                .unwrap_or(1);
            if leaf_count > 1 {
                self.handle_close_split(leaf_id.into());
            } else {
                // Last leaf with no other tab: seed a fresh scratch buffer so
                // the source window keeps a renderable tab after the move.
                self.new_buffer();
            }
        }
    }

    /// Close a session and drop its `Session` entry. Refuses to
    /// close the currently active session — the caller must switch
    /// to a different session first. Refuses to close the *last*
    /// remaining window — the editor must always host at least one.
    ///
    /// There is no special "base" window any more: id 1 is just the
    /// window the editor launched into, closable like any other once
    /// another window exists. The real invariant is "≥1 window", not
    /// "id 1 lives forever".
    ///
    /// Returns `true` on success, `false` on rejection.
    pub fn close_window(&mut self, id: WindowId) -> bool {
        // A dormant remote session usually has no `Window` and no live
        // connection — closing it just drops its descriptor so it leaves the
        // dock. (Without a window it can never be active, and isn't the
        // "last window".) A dormant session that DOES have a window — the
        // disconnected shell a failed reconnect leaves behind — falls through
        // to the normal close path below (which honours the active/last-window
        // guards) and drops its descriptor together with the window.
        if self.dormant_remote.contains_key(&id) && !self.windows.contains_key(&id) {
            self.dormant_remote.remove(&id);
            self.plugin_manager
                .read()
                .unwrap()
                .run_hook("window_closed", HookArgs::WindowClosed { id: id.0 });
            return true;
        }
        if self.windows.len() <= 1 {
            tracing::warn!("close_window: refusing to close the last remaining window (id {id})");
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
        // Closing a dormant session's disconnected shell drops the whole
        // session: the descriptor must leave the dock with the window.
        self.dormant_remote.remove(&id);
        // Tear down a born-attached remote session's connection (carrier +
        // reconnect/heartbeat + runtime) when its window closes. No-op for
        // local windows, which never have an entry.
        if self.session_keepalives.remove(&id).is_some() {
            tracing::info!("close_window: dropped remote session keepalive for window {id}");
        }

        self.plugin_manager
            .read()
            .unwrap()
            .run_hook("window_closed", HookArgs::WindowClosed { id: id.0 });

        true
    }

    /// Born-attached remote session: create a **new window** whose authority is
    /// the already-connected remote backend (Kubernetes / SSH / …), seed its
    /// terminal *inside* that backend, and park the connection `keepalive`
    /// keyed by the window so it outlives editor rebuilds and is torn down on
    /// close.
    ///
    /// Unlike the global `install_authority_with_keepalive` restart, existing
    /// windows are left untouched — the remote session coexists with them, and
    /// `set_active_window` (Gap A) retargets the active authority when the user
    /// switches. The new window is born under `authority` because it is passed
    /// straight to `create_window_with_terminal` as that window's backend, so
    /// its filesystem, LSP spawner, and terminal wrapper all act in the backend
    /// from birth (there are no stale local handles to invalidate — the caveat
    /// that gates hot-swapping an *existing* window's authority doesn't apply
    /// here). `create_window_with_terminal` adopts the new window's authority
    /// into the editor-wide caches before returning.
    pub(crate) fn create_remote_session_window(
        &mut self,
        authority: crate::services::authority::Authority,
        keepalive: Box<dyn std::any::Any + Send>,
        root: PathBuf,
        label: String,
        command: Option<Vec<String>>,
        spec: crate::services::authority::SessionAuthoritySpec,
    ) -> Result<WindowId, String> {
        match self.create_window_with_terminal(
            root.clone(),
            label,
            Some(root),
            command,
            None,
            authority,
            None,
        ) {
            Ok((window_id, _terminal, _buffer)) => {
                self.session_keepalives.insert(window_id, keepalive);
                // Persist how to reconnect this backend on the new session so
                // a restart / relaunch can bring it back rather than degrade
                // it to local.
                if let Some(w) = self.windows.get_mut(&window_id) {
                    w.authority_spec = spec;
                }
                Ok(window_id)
            }
            Err(e) => {
                // The connect succeeded but the window couldn't be seeded
                // (e.g. the backend has no python3 / the pod died):
                // `create_window_with_terminal` already rolled the active
                // pointer back to the previous window and left the
                // editor-wide authority untouched (it never installed the
                // remote one), so just drop the keepalive (tears down the
                // carrier).
                drop(keepalive);
                Err(e)
            }
        }
    }

    /// Begin bringing a **dormant remote** session online: connect its SSH/kube
    /// backend, then — on success — promote it to a real `Window`
    /// ([`Self::promote_dormant_remote`]). Used when the user dives into a
    /// session that boot discovered but never connected: it has no `Window` yet,
    /// only a `dormant_remote` descriptor (no authority). The active window is
    /// left unchanged until the connection lands, so the editor never shows a
    /// window without its real backend.
    pub(crate) fn bring_dormant_remote_online(&mut self, id: WindowId) {
        let Some(descriptor) = self.dormant_remote.get(&id) else {
            return;
        };
        // Only remote-agent sessions are ever placed in `dormant_remote`.
        let spec = match &descriptor.authority_spec {
            crate::services::authority::SessionAuthoritySpec::RemoteAgent(s) => s.clone(),
            _ => return,
        };
        let request_id = u64::MAX - id.0;
        if self.remote_attach_inflight.contains(&request_id) {
            return; // a connect for this session is already in flight
        }
        // A prior failed connect may have left a disconnected shell window
        // for this session — clear its recorded failure so the indicator
        // shows "Connecting" (not a stale error) while this retry runs.
        if let Some(w) = self.windows.get_mut(&id) {
            w.remote_reconnect_error = None;
        }
        // `start_remote_connect` posts a "Connecting to …" status and, on
        // success, emits `RemoteAttachReady` in `Reconnect { window_id: id }`
        // mode — which `promote_dormant_remote` turns into the live window. The
        // remote connect machinery is plugins-gated (dormant remote sessions are
        // created through the orchestrator plugin); without it there is nothing
        // to connect through, so diving into one is a no-op.
        #[cfg(feature = "plugins")]
        self.start_remote_connect(spec, Some(id), request_id);
        #[cfg(not(feature = "plugins"))]
        let _ = (spec, request_id);
    }

    /// Promote a dormant remote session to a live `Window`, **born with the
    /// freshly-connected `authority`**. Its persisted workspace is restored
    /// through that authority, so its terminals spawn on the remote backend —
    /// never the local host. This is the *only* path that turns a
    /// `dormant_remote` descriptor into a `Window`; there is deliberately no way
    /// to build that window without the connected backend in hand, which is what
    /// makes "a restored remote terminal running locally" unrepresentable.
    pub(crate) fn promote_dormant_remote(
        &mut self,
        id: WindowId,
        authority: crate::services::authority::Authority,
        keepalive: Box<dyn std::any::Any + Send>,
    ) {
        let Some(descriptor) = self.dormant_remote.remove(&id) else {
            // Raced with a close / a second connect — nothing to promote.
            drop(authority);
            drop(keepalive);
            return;
        };
        let root = descriptor.root.clone();

        // Resources rooted at this window's *own* backend filesystem (remote),
        // so its file explorer / quick-open ride the session's backend.
        let mut resources = self.window_resources();
        resources.fs_manager = std::sync::Arc::new(crate::services::fs::FsManager::new(
            std::sync::Arc::clone(&authority.filesystem),
        ));

        // Restore the persisted workspace through the connected authority (its
        // terminals spawn over SSH/kube), or seed an empty layout when there is
        // no saved workspace. Either constructor takes the authority by value —
        // the window is born owning its real backend.
        let workspace = if let Some(name) = self.session_name.clone() {
            crate::workspace::Workspace::load_session(&name, &root)
                .ok()
                .flatten()
        } else {
            crate::workspace::Workspace::load(&root).ok().flatten()
        };
        let mut window = match workspace {
            Some(ws) => crate::app::window::Window::from_workspace(
                id,
                descriptor.label.clone(),
                root.clone(),
                authority,
                resources,
                &ws,
            ),
            None => {
                let mut w = crate::app::window::Window::new(
                    id,
                    descriptor.label.clone(),
                    root.clone(),
                    authority,
                    resources,
                );
                w.seed_initial_layout();
                w
            }
        };
        window.terminal_width = self.terminal_width;
        window.terminal_height = self.terminal_height;
        window.plugin_state = descriptor.plugin_state.clone();
        window.authority_spec = descriptor.authority_spec.clone();
        // Captured before the insert below swaps the active window's
        // authority out from under `self.authority()` — only meaningful for
        // the already-active case, where it is the disconnected shell's
        // placeholder label.
        let previous_authority_label = self.authority().display_label.clone();
        let already_active = self.active_window == id;
        self.windows.insert(id, window);
        self.session_keepalives.insert(id, keepalive);

        if already_active {
            // The restored window replaced this session's disconnected shell
            // (a failed earlier connect committed the switch), which was
            // already the active window — `set_active_window` would no-op.
            // Run the switch tail it would have run: adopt the connected
            // authority into the editor caches, refresh the plugin snapshot,
            // re-sync terminal mode for the restored active buffer, and
            // relayout.
            self.adopt_active_window_authority(&previous_authority_label);
            #[cfg(feature = "plugins")]
            self.update_plugin_state_snapshot();
            self.sync_terminal_mode_to_active_buffer();
            self.relayout();
        } else {
            // Activate through the normal switch path: nothing to materialize
            // (already restored), no reconnect re-trigger (keepalive now
            // parked), and it adopts the new authority into the editor
            // caches, fires `active_window_changed`, and relayouts.
            self.set_active_window(id);
        }
        self.set_status_message(format!("Connected: {}", descriptor.label));
    }

    /// Ensure a dormant remote session has its **empty shell** `Window`, so a
    /// dive can commit the switch immediately — before (and regardless of
    /// whether) its backend connect resolves (issue #2570: the dock must
    /// never select a workspace the editor didn't actually enter, and a dead
    /// host can keep the connect in flight for minutes).
    ///
    /// The shell is a real `Window` on a local placeholder authority with
    /// nothing restored into it: its persisted workspace can only be restored
    /// through the connected backend, so it stays on disk, authoritative
    /// (`save_workspace_for` skips descriptor-backed ids). The descriptor is
    /// deliberately **kept** in `dormant_remote`, so diving again retries the
    /// connect and a success still lands in `promote_dormant_remote` — which
    /// replaces this shell with the fully-restored window. While the shell is
    /// active, the status bar presents the in-flight connect as `Connecting`
    /// and a recorded failure as `Disconnected` (with the Retry popup).
    pub(crate) fn ensure_dormant_shell(&mut self, id: WindowId) {
        if self.windows.contains_key(&id) {
            return;
        }
        let Some(descriptor) = self.dormant_remote.get(&id) else {
            return;
        };
        let root = descriptor.root.clone();
        // Same per-session local scope a boot-discovered local shell gets:
        // its own trust + env handles, never a clone of the previous
        // window's.
        let authority = crate::services::authority::Authority::local_scoped(
            crate::services::authority::SessionScope::for_root(
                &root,
                &self.dir_context.project_state_dir(&root),
            ),
        );
        let mut window = Window::new(
            id,
            descriptor.label.clone(),
            root,
            authority,
            self.window_resources(),
        );
        window.terminal_width = self.terminal_width;
        window.terminal_height = self.terminal_height;
        window.plugin_state = descriptor.plugin_state.clone();
        // Keep the backend identity so the status bar / dock present the
        // session as its real (not-yet-connected) backend and a retry knows
        // what to reconnect to — never downgraded to local.
        window.authority_spec = descriptor.authority_spec.clone();
        // The shell renders as a placeholder page (see
        // `render_dormant_shell_page`), not as an editable buffer — nothing
        // can be meaningfully edited before the backend connects. Seed the
        // layout here (so the renderer has a populated `splits`) and lock
        // its scratch buffer.
        window.seed_initial_layout();
        let seed_buffer = window.active_buffer();
        window.mark_buffer_read_only(seed_buffer, true);
        self.windows.insert(id, window);
    }

    /// A dormant remote session's dive-triggered connect **failed** while the
    /// session has no window yet: commit the switch into its empty shell and
    /// record the reason (drives the `Disconnected` indicator + Retry popup).
    /// With dives committing the switch up front this is a fallback — it only
    /// fires when a connect was started without the shell (or the shell was
    /// closed while the connect was in flight).
    pub(crate) fn activate_failed_dormant_placeholder(&mut self, id: WindowId, reason: String) {
        if !self.dormant_remote.contains_key(&id) {
            return;
        }
        self.ensure_dormant_shell(id);
        if let Some(w) = self.windows.get_mut(&id) {
            w.remote_reconnect_error = Some(reason);
        }
        // The normal switch path seeds the empty layout and fires
        // `active_window_changed`, so the dock keeps this session selected
        // as the (now genuinely) active one. Its reconnect re-trigger is a
        // no-op here: `reconnect_dormant_session_if_needed` skips
        // descriptor-backed sessions, whose connects are owned by the dive
        // gate (`bring_dormant_remote_online`).
        self.set_active_window(id);
    }
}
