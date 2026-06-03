//! Plain accessor methods on `Editor`.
//!
//! Configuration getters, key-translator/time-source/event-broadcaster
//! handles, LSP / completion / update query helpers, mode registry
//! access, status/warning log setup, and the per-frame timer-check
//! methods (mouse hover / semantic highlight / diagnostic pull /
//! completion trigger).
//!
//! These are mostly small `&self` queries that read a single field;
//! grouping them together keeps mod.rs focused on the central
//! orchestration.

use super::*;

impl Editor {
    /// Get a reference to the async bridge (if available)
    pub fn async_bridge(&self) -> Option<&AsyncBridge> {
        self.async_bridge.as_ref()
    }

    /// Get a reference to the config
    pub fn config(&self) -> &Config {
        &self.config
    }

    /// Get a mutable reference to the config.
    ///
    /// Routes through `Arc::make_mut`: if the plugin state snapshot (or any
    /// other reader) still holds an `Arc` to the current value, this
    /// CoW-clones so existing readers observe a stable value and the next
    /// snapshot refresh sees a new pointer. `Arc<T>` has no `DerefMut`, so
    /// the only way to mutate through `self.config` is via this accessor —
    /// there is no code path that can silently leave a reader with stale
    /// data.
    ///
    /// Window-side reads (`Window::config()`) read a *separate* Arc clone
    /// stashed in `WindowResources`. Mutations through `config_mut`
    /// therefore leave window clones stale until [`Editor::sync_windows_config`]
    /// runs. Callers that mutate a config field which Window code reads
    /// (e.g. `editor.line_wrap`, `editor.enable_inlay_hints`,
    /// `languages`, etc.) must call `sync_windows_config()` afterwards.
    /// `set_config` does this automatically.
    pub fn config_mut(&mut self) -> &mut Config {
        Arc::make_mut(&mut self.config)
    }

    /// Replace the config wholesale. Used by the "reload config" path and
    /// by tests that want to swap in a freshly-parsed file. Constructs a
    /// fresh `Arc`, so any snapshot that still holds the old value sees
    /// the pointer move and will reserialize on the next refresh.
    ///
    /// Also propagates the new `Arc` to every window's
    /// `resources.config`, so window-scoped reads see the swap.
    pub fn set_config(&mut self, new_config: Config) {
        self.config = Arc::new(new_config);
        self.sync_windows_config();
    }

    /// Propagate `self.config` to every window's `resources.config` so
    /// window-side reads (`Window::config()`) see the latest value.
    /// `Arc::clone` is cheap, so this is a constant-time fanout per
    /// window. Called from `set_config` and at the end of any
    /// `config_mut()`-driven mutation that affects window-read fields.
    pub(crate) fn sync_windows_config(&mut self) {
        let cfg = self.config.clone();
        for w in self.windows.values_mut() {
            w.resources.config = cfg.clone();
        }
    }

    /// Replace the cached raw user config. Like `set_config`, constructs
    /// a fresh `Arc` so the plugin snapshot notices the change.
    pub(crate) fn set_user_config_raw(&mut self, value: serde_json::Value) {
        self.user_config_raw = Arc::new(value);
    }

    /// Mutable access to the active window's merged diagnostics map.
    /// Routes through `Arc::make_mut`, which CoW-clones while the
    /// plugin snapshot still holds the old map — readers never
    /// observe an in-place mutation.
    pub(crate) fn stored_diagnostics_mut(
        &mut self,
    ) -> &mut HashMap<String, Vec<lsp_types::Diagnostic>> {
        Arc::make_mut(&mut self.active_window_mut().stored_diagnostics)
    }

    /// Mutable access to the active window's folding-ranges map.
    /// Same `Arc::make_mut` CoW pattern as `stored_diagnostics_mut`.
    pub(crate) fn stored_folding_ranges_mut(
        &mut self,
    ) -> &mut HashMap<String, Vec<lsp_types::FoldingRange>> {
        Arc::make_mut(&mut self.active_window_mut().stored_folding_ranges)
    }

    /// Get a reference to the key translator (for input calibration)
    pub fn key_translator(&self) -> &crate::input::key_translator::KeyTranslator {
        &self.key_translator
    }

    /// Get a reference to the time source
    pub fn time_source(&self) -> &SharedTimeSource {
        &self.time_source
    }

    /// Emit a control event
    pub fn emit_event(&self, name: impl Into<String>, data: serde_json::Value) {
        self.event_broadcaster.emit_named(name, data);
    }

    /// Send a response to a plugin for an async operation
    pub(super) fn send_plugin_response(&self, response: fresh_core::api::PluginResponse) {
        self.plugin_manager
            .read()
            .unwrap()
            .deliver_response(response);
    }

    // `take_pending_semantic_token_request` and
    // `take_pending_semantic_token_range_request` live on `impl Window`
    // — call them via `self.active_window_mut()`.

    /// Get all keybindings as (key, action) pairs
    pub fn get_all_keybindings(&self) -> Vec<(String, String)> {
        self.keybindings.read().unwrap().get_all_bindings()
    }

    /// Get the formatted keybinding for a specific action (for display in messages)
    /// Returns None if no keybinding is found for the action
    pub fn get_keybinding_for_action(&self, action_name: &str) -> Option<String> {
        self.keybindings
            .read()
            .unwrap()
            .find_keybinding_for_action(action_name, self.active_window().key_context.clone())
    }

    /// Raw-event counterpart: return the `(KeyCode, KeyModifiers)` currently
    /// bound to `action` in `context`. Intended for callers that need to
    /// simulate the user pressing the bound key (e2e tests, some hotkey-
    /// chaining code) without hardcoding a default that a user's rebind
    /// would invalidate.
    pub fn keybinding_event_for_action(
        &self,
        action: &crate::input::keybindings::Action,
        context: crate::input::keybindings::KeyContext,
    ) -> Option<(crossterm::event::KeyCode, crossterm::event::KeyModifiers)> {
        self.keybindings
            .read()
            .unwrap()
            .get_keybinding_event_for_action(action, context)
    }

    /// Get mutable access to the mode registry
    pub fn mode_registry_mut(&mut self) -> &mut ModeRegistry {
        &mut self.mode_registry
    }

    /// Get immutable access to the mode registry
    pub fn mode_registry(&self) -> &ModeRegistry {
        &self.mode_registry
    }

    /// Get the currently active buffer ID.
    ///
    /// This is derived from the split manager (single source of truth).
    /// The editor always has at least one buffer, so this never fails.
    ///
    /// When the active split has a buffer-group tab as its active target
    /// (i.e., `active_group_tab.is_some()`), this returns the buffer of the
    /// currently-focused inner panel — so that input routing, command palette
    /// context, buffer mode, and other "what is the user looking at" queries
    /// resolve to the panel the user is actually interacting with rather than
    /// the split's background leaf buffer.
    ///
    /// The override only takes effect if the inner panel's buffer is still
    /// live in `self.buffers`; otherwise it falls back to the main split's
    /// leaf buffer so callers never see a stale/freed buffer id.
    #[inline]
    pub fn active_buffer(&self) -> BufferId {
        let (_, buf) = self.effective_active_pair();
        buf
    }

    /// The split id whose `SplitViewState` owns the currently-focused
    /// cursors/viewport/buffer state. For a regular split this is just
    /// `split_manager.active_split()`. For a split that has a group tab
    /// active, this returns the focused inner panel's leaf id (which
    /// lives in `split_view_states` even though it's not in the main
    /// split tree).
    #[inline]
    pub fn effective_active_split(&self) -> crate::model::event::LeafId {
        let (split, _) = self.effective_active_pair();
        split
    }

    /// Resolve the effective (split, buffer) pair for the currently-focused
    /// target. This is the single source of truth — both `active_buffer` and
    /// `effective_active_split` derive from it so they can never disagree.
    ///
    /// Returned invariant: `split_view_states[split]` exists, its
    /// `active_buffer` equals the returned buffer id, `self.buffers`
    /// contains the returned buffer id, and `split.keyed_states` contains
    /// an entry for the returned buffer id. Consequently the mutation path
    /// in `apply_event_to_active_buffer` (which indexes into
    /// `keyed_states[buffer]`) is always well-defined for the returned pair.
    ///
    /// If a buffer-group panel is focused but any of the invariants above
    /// is not satisfied for the inner leaf (for example because the panel
    /// buffer was freed without clearing `focused_group_leaf`), the helper
    /// falls back to the outer split's own leaf. The fallback is also
    /// validated before being returned.
    #[inline]
    fn effective_active_pair(&self) -> (crate::model::event::LeafId, BufferId) {
        self.active_window().effective_active_pair()
    }

    /// Get the mode name for the active buffer.
    ///
    /// Resolution order:
    ///   1. The buffer's own virtual-buffer mode, if it has one.
    ///   2. The mode declared by the buffer-group containing this
    ///      buffer (e.g. a `git-log` group's keybindings apply to its
    ///      panels regardless of whether each panel is a virtual
    ///      buffer or a file-backed one — `openFileStreaming` produces
    ///      the latter for streaming detail panels).
    pub fn active_buffer_mode(&self) -> Option<&str> {
        let buffer_id = self.active_buffer();
        let win = self.active_window();
        if let Some(mode) = win
            .buffer_metadata
            .get(&buffer_id)
            .and_then(|meta| meta.virtual_mode())
        {
            return Some(mode);
        }
        let group_id = win.buffer_to_group.get(&buffer_id).copied()?;
        win.buffer_groups.get(&group_id).map(|g| g.mode.as_str())
    }

    /// Check if the active buffer is read-only
    pub fn is_active_buffer_read_only(&self) -> bool {
        if let Some(metadata) = self
            .active_window()
            .buffer_metadata
            .get(&self.active_buffer())
        {
            if metadata.read_only {
                return true;
            }
            // Also check if the mode is read-only
            if let Some(mode_name) = metadata.virtual_mode() {
                return self.mode_registry.is_read_only(mode_name);
            }
        }
        false
    }

    // `mark_buffer_read_only` lives on `impl Window` — call it via
    // `self.active_window_mut().mark_buffer_read_only(buffer_id, ro)`.

    /// Get the effective mode for the active buffer.
    ///
    /// Buffer-local mode (virtual buffers) takes precedence over the global
    /// editor mode, so that e.g. a search-replace panel isn't hijacked by
    /// a markdown-source or vi-mode global mode.
    pub fn effective_mode(&self) -> Option<&str> {
        // When a floating widget panel is mounted, its plugin-defined
        // mode (`editor.setEditorMode(...)`) takes precedence over the
        // underlying buffer's virtual mode. Without this, opening the
        // Orchestrator picker from a python3 terminal session would
        // resolve mode-keybindings against `"terminal"` instead of
        // `"orchestrator-open"`, so picker-specific shortcuts like
        // `Alt+N` never reached their handlers.
        if self.floating_widget_panel.is_some() {
            if let Some(mode) = self.active_window().editor_mode.as_deref() {
                return Some(mode);
            }
        }
        self.active_buffer_mode()
            .or(self.active_window().editor_mode.as_deref())
    }

    // `has_active_lsp_progress`, `get_lsp_progress`, and
    // `is_lsp_server_ready` live on `impl Window` — call them via
    // `self.active_window().has_active_lsp_progress()` etc.

    /// Read-only view of the editor-wide popup stack.
    ///
    /// `global_popups` itself is `pub(crate)` so its internals stay
    /// private to the app module; tests need to inspect its depth /
    /// contents to verify "no two popups stacked across the buffer-
    /// local and global stacks" invariants (e.g. issue 1 of the LSP
    /// indicator-click bugs), so we expose an immutable accessor here.
    pub fn global_popups(&self) -> &crate::view::popup::PopupManager {
        &self.global_popups
    }

    /// The earliest wall-clock deadline at which the main event loop
    /// needs to wake up and re-render, *purely because of internal
    /// time-driven UI elements* (animations, the LSP status-bar
    /// spinner). Returns `None` when no time-driven UI is in flight —
    /// the loop can sleep until the next user / async event without
    /// missing a frame.
    ///
    /// The `Some` case includes the LSP-progress spinner: its glyph
    /// is computed from `SystemTime::now() / 100ms`, so the loop has
    /// to wake at ~100ms cadence to actually advance it. Without
    /// this signal, the indicator would only tick when an unrelated
    /// event caused a frame, and the user would see a "frozen"
    /// spinner whenever the server stopped emitting `$/progress`
    /// (e.g. died externally — see #1941 issue 3).
    pub fn next_periodic_redraw_deadline(&self) -> Option<std::time::Instant> {
        let lsp_progress_deadline = if self.active_window().has_active_lsp_progress() {
            // 100ms matches the spinner-glyph period in
            // `lsp_status::compose_lsp_status`.
            Some(std::time::Instant::now() + std::time::Duration::from_millis(100))
        } else {
            None
        };
        let anim_deadline = self.active_window().animations.next_deadline();
        // Paste-pending deadline: the editor tick falls back to the
        // internal clipboard if the async arboard read doesn't return
        // by this point. Including it here makes the main loop wake
        // exactly when the timeout needs to fire, so a hung clipboard
        // owner can't block the UI past `PASTE_ASYNC_DEADLINE`.
        let paste_deadline = self.next_paste_deadline();
        // Note: the terminal-title poll deadline is intentionally NOT folded
        // in here. This deadline path caps the loop's wait to one frame
        // (~16ms) for smooth animation, which would turn the ~1s title poll
        // into a 60Hz busy loop. The loop's existing 50ms idle poll is fine
        // granularity to notice `terminal_titles_need_poll` going true.
        [lsp_progress_deadline, anim_deadline, paste_deadline]
            .into_iter()
            .flatten()
            .min()
    }

    /// Earliest time a terminal tab needs its foreground-process title
    /// re-polled, across all windows. `None` when no window has an
    /// auto-named (non-explicit) terminal. Drives the event loop's periodic
    /// wakeups so a tab reflects a command that starts or exits while the
    /// UI is otherwise idle (the render that follows runs
    /// `Window::sync_terminal_titles`).
    pub fn terminal_title_poll_deadline(&self) -> Option<std::time::Instant> {
        if !self.config.editor.terminal_auto_title {
            return None;
        }
        let mut earliest: Option<std::time::Instant> = None;
        for window in self.windows.values() {
            let has_auto = window
                .terminal_buffers
                .keys()
                .any(|b| !window.terminal_explicit_titles.contains(b));
            if !has_auto {
                continue;
            }
            let deadline = match window.terminal_fg_poll_at {
                Some(last) => last + crate::app::terminal::FG_POLL_INTERVAL,
                None => std::time::Instant::now(),
            };
            earliest = Some(earliest.map_or(deadline, |e| e.min(deadline)));
        }
        earliest
    }

    /// Whether a terminal tab is due for a foreground-process title poll
    /// (its deadline has passed). The event loop ORs this into its
    /// `needs_render` decision so the periodic wakeup actually paints a
    /// frame, matching how animations and the LSP spinner are handled.
    pub fn terminal_titles_need_poll(&self) -> bool {
        self.terminal_title_poll_deadline()
            .is_some_and(|d| d <= std::time::Instant::now())
    }

    /// Get stored LSP diagnostics (for testing and external access)
    /// Returns a reference to the diagnostics map keyed by file URI
    pub fn get_stored_diagnostics(&self) -> &HashMap<String, Vec<lsp_types::Diagnostic>> {
        &self.active_window().stored_diagnostics
    }

    /// Check if an update is available
    pub fn is_update_available(&self) -> bool {
        self.update_checker
            .as_ref()
            .map(|c| c.is_update_available())
            .unwrap_or(false)
    }

    /// Get the latest version string if an update is available
    pub fn latest_version(&self) -> Option<&str> {
        self.update_checker
            .as_ref()
            .and_then(|c| c.latest_version())
    }

    /// Get the cached release check result (for shutdown notification)
    pub fn get_update_result(
        &self,
    ) -> Option<&crate::services::release_checker::ReleaseCheckResult> {
        self.update_checker
            .as_ref()
            .and_then(|c| c.get_cached_result())
    }

    /// Set a custom update checker (for testing)
    ///
    /// This allows injecting a custom PeriodicUpdateChecker that points to a mock server,
    /// enabling E2E tests for the update notification UI.
    #[doc(hidden)]
    pub fn set_update_checker(
        &mut self,
        checker: crate::services::release_checker::PeriodicUpdateChecker,
    ) {
        self.update_checker = Some(checker);
    }

    /// Configure LSP server for a specific language
    pub fn set_lsp_config(&mut self, language: String, config: Vec<LspServerConfig>) {
        let __active_id = self.active_window;
        if let Some(lsp) = self.windows.get_mut(&__active_id).map(|w| &mut w.lsp) {
            lsp.set_language_configs(language, config);
        }
    }

    // `running_lsp_servers`, `pending_completion_requests_count`,
    // `completion_items_count`, `initialized_lsp_server_count`, and
    // `shutdown_lsp_server` live on `impl Window` — call them via
    // `self.active_window()` / `self.active_window_mut()`.

    /// Set up warning log monitoring
    ///
    /// When warnings/errors are logged, they will be written to the specified path
    /// and the editor will be notified via the receiver.
    pub fn set_warning_log(&mut self, receiver: std::sync::mpsc::Receiver<()>, path: PathBuf) {
        self.warning_log = Some((receiver, path));
    }

    /// Take the warning-log receiver+path out of this editor.
    ///
    /// The receiver is single-consumer and lives for the process's
    /// lifetime; on a destructive editor restart (e.g. authority swap)
    /// `main.rs` lifts it from the old editor and re-installs it on the
    /// new one so warnings keep flowing post-restart instead of vanishing
    /// with the dropped editor.
    pub fn take_warning_log(&mut self) -> Option<(std::sync::mpsc::Receiver<()>, PathBuf)> {
        self.warning_log.take()
    }

    /// Set the status message log path
    pub fn set_status_log_path(&mut self, path: PathBuf) {
        self.status_log_path = Some(path);
    }

    /// Queue a new authority and restart the editor.
    ///
    /// Per the design decision in `docs/internal/AUTHORITY_DESIGN.md`,
    /// authority transitions piggy-back on the existing
    /// `change_working_dir` restart path. The caller never sees an
    /// editor that is half-transitioned: the current `Editor` is
    /// dropped, `main.rs` rebuilds a fresh one with the queued
    /// authority, and session restore reopens buffers against the new
    /// backend. This is slower than an in-place pointer swap but is
    /// far more robust — every cached `Arc<dyn FileSystem>`, LSP
    /// handle, terminal PTY, plugin state, and in-flight task is
    /// dropped cleanly by the existing restart machinery.
    pub fn install_authority(&mut self, authority: crate::services::authority::Authority) {
        self.pending_authority = Some(authority);
        // Re-open the same working directory; `main.rs` picks up the
        // pending authority from the old editor just before dropping it.
        self.request_restart(self.working_dir().to_path_buf());
    }

    /// Install a new authority that owns a live connection, parking its
    /// keepalive bundle so the connection survives the restart.
    ///
    /// Remote-agent backends (SSH-style, K8s) hold carrier processes,
    /// reconnect/heartbeat tasks, and a Tokio handle that must outlive
    /// the `Editor` rebuild — exactly the role of the daemon's
    /// `session_keepalive` slot. The restart loop pairs
    /// `take_pending_authority` with `take_pending_keepalive` and moves
    /// the bundle into the process-/server-level keepalive, dropping the
    /// previous one (tearing down the prior connection). Opaque
    /// `Box<dyn Any + Send>` so core/main need not name the backend.
    pub fn install_authority_with_keepalive(
        &mut self,
        authority: crate::services::authority::Authority,
        keepalive: Box<dyn std::any::Any + Send>,
        working_dir: std::path::PathBuf,
    ) {
        // Unlike `install_authority` (which re-opens the *current* working
        // dir), a remote-agent attach must re-root the editor at the pod-side
        // workspace — otherwise the explorer, quick-open, and open-file all
        // operate on the local host path, which doesn't exist in the pod.
        self.pending_keepalive = Some(keepalive);
        self.pending_authority = Some(authority);
        self.request_restart(working_dir);
    }

    /// Restore the default local authority. Same destructive-restart
    /// semantics as `install_authority` — the caller never observes a
    /// half-transitioned editor.
    pub fn clear_authority(&mut self) {
        // Reuse the editor's live trust handle so the restored local authority
        // is gated by the same workspace-trust state.
        let trust = std::sync::Arc::clone(&self.authority.workspace_trust);
        let env = std::sync::Arc::clone(&self.authority.env_provider);
        self.install_authority(crate::services::authority::Authority::local(trust, env));
    }

    /// Take the queued authority (if any). Called by `main.rs` on
    /// restart to move the queued authority into the fresh editor.
    pub fn take_pending_authority(&mut self) -> Option<crate::services::authority::Authority> {
        self.pending_authority.take()
    }

    /// Take the keepalive bundle queued alongside a pending authority by
    /// [`Self::install_authority_with_keepalive`]. Called by the restart
    /// loop right beside `take_pending_authority` so the new connection's
    /// carrier/tasks are parked before the old `Editor` is dropped.
    pub fn take_pending_keepalive(&mut self) -> Option<Box<dyn std::any::Any + Send>> {
        self.pending_keepalive.take()
    }

    /// Directly replace the active authority without triggering a
    /// restart. Intended for the post-construction wiring in `main.rs`
    /// only, where the editor is still being set up and there is no
    /// user-visible state to preserve. Do not call this from the event
    /// loop — use `install_authority` for that.
    ///
    /// Also refreshes the plugin state snapshot so hooks that fire after
    /// this call (notably `plugins_loaded`, fired by `main.rs` right
    /// after `set_boot_authority`) see the real `authority_label` instead
    /// of the empty string the temporary `Authority::local()` carried
    /// during construction.
    pub fn set_boot_authority(&mut self, authority: crate::services::authority::Authority) {
        self.authority = authority;
        // Propagate the new authority to every window's resources so
        // window-side filesystem/path-translation reads (`Window::authority()`)
        // see the swap. `Authority` carries internal `Arc`s, so this just
        // clones cheap handles.
        let auth = self.authority.clone();
        for w in self.windows.values_mut() {
            w.resources.authority = auth.clone();
        }
        // Propagate the authority's long-running spawner into the LSP
        // manager so `force_spawn` can route server processes through
        // the right backend. The editor rebuilds on every authority
        // transition (AUTHORITY_DESIGN.md principle 7), so this is the
        // single wiring point — no need for a hot-swap API. Path
        // translation rides along for the same reason — LSP URIs need
        // to be host↔container-translated under the new authority.
        let __active_id = self.active_window;
        if let Some(lsp) = self.windows.get_mut(&__active_id).map(|w| &mut w.lsp) {
            lsp.set_long_running_spawner(self.authority.long_running_spawner.clone());
            lsp.set_path_translation(self.authority.path_translation.clone());
            lsp.set_workspace_trust(self.authority.workspace_trust.clone());
        }
        // Re-point quick-open's file provider at the new backend. The provider
        // captured the *previous* authority's filesystem + spawner; without
        // this, quick-open's `git ls-files` keeps listing the old backend's
        // files after an in-place authority swap (see
        // `QuickOpenRegistry::set_file_backends`).
        self.quick_open_registry.set_file_backends(
            self.authority.filesystem.clone(),
            self.authority.process_spawner.clone(),
        );
        #[cfg(feature = "plugins")]
        {
            self.update_plugin_state_snapshot();
            // Notify plugins so they can re-register state-gated
            // commands (e.g. devcontainer `Attach` only when not
            // attached). Production transitions also trigger a full
            // editor restart that re-runs plugin init, but firing
            // here keeps in-process transitions and the test harness
            // (which simulates the restart inline) consistent.
            let label = self.authority.display_label.clone();
            self.plugin_manager.read().unwrap().run_hook(
                "authority_changed",
                crate::services::plugins::hooks::HookArgs::AuthorityChanged { label },
            );
        }
    }

    /// The active window's id. The active session under the (in-progress)
    /// per-session model; today pinned to the single window.
    pub fn active_window_id(&self) -> fresh_core::WindowId {
        self.active_window
    }

    /// Swap a *single* session's (window's) authority without a restart —
    /// the per-session counterpart to [`Self::set_boot_authority`], which
    /// fans one authority across every window at boot.
    ///
    /// Updates that window's `resources.authority` and re-points its LSP
    /// backend (long-running spawner, path translation, trust); when the
    /// window is the active one, mirrors into the editor-wide `authority`
    /// cache the rest of the editor reads and fires the `authority_changed`
    /// hook. This is the activation primitive a per-session attach (the
    /// planned `attachRemoteAgent` op, and the Orchestrator session-swap)
    /// builds on, and the seam that lets distinct windows hold distinct
    /// authorities concurrently (`AUTHORITY_DESIGN.md` §"Evolution:
    /// per-session authority").
    ///
    /// Caveat — why production attach still goes through the destructive
    /// `install_authority` restart: like `set_boot_authority`, this does
    /// not invalidate per-buffer captured filesystem handles or terminals
    /// opened under the previous authority. Hot-swapping those safely is
    /// the remaining per-window cache-invalidation work gated on the live
    /// multi-session migration; until it lands, this method is the
    /// infrastructure seam, exercised by tests and the activation path,
    /// not yet the user-facing attach.
    pub fn set_session_authority(
        &mut self,
        window_id: fresh_core::WindowId,
        authority: crate::services::authority::Authority,
    ) {
        let is_active = self.active_window == window_id;
        if let Some(w) = self.windows.get_mut(&window_id) {
            w.resources.authority = authority.clone();
            // Each window owns its `LspManager` by construction (no longer an
            // `Option`); re-point its backend handles, matching the active-
            // window re-pointing `set_boot_authority` does for every window.
            let lsp = &mut w.lsp;
            lsp.set_long_running_spawner(authority.long_running_spawner.clone());
            lsp.set_path_translation(authority.path_translation.clone());
            lsp.set_workspace_trust(authority.workspace_trust.clone());
        }
        if is_active {
            self.authority = authority;
            // Re-point quick-open's file provider at the now-active backend
            // (see `set_boot_authority` — same stale-capture fix).
            self.quick_open_registry.set_file_backends(
                self.authority.filesystem.clone(),
                self.authority.process_spawner.clone(),
            );
            #[cfg(feature = "plugins")]
            {
                self.update_plugin_state_snapshot();
                let label = self.authority.display_label.clone();
                self.plugin_manager.read().unwrap().run_hook(
                    "authority_changed",
                    crate::services::plugins::hooks::HookArgs::AuthorityChanged { label },
                );
            }
        }
    }

    /// Read-only access to the active authority.
    pub fn authority(&self) -> &crate::services::authority::Authority {
        &self.authority
    }

    /// The editor's current working directory — the active window's
    /// project root. Derived, not stored: there is no separate
    /// `working_dir` field that could drift out of sync with the active
    /// window (issue #2056). Individual buffers may live elsewhere.
    pub fn working_dir(&self) -> &std::path::Path {
        &self.active_window().root
    }

    /// The currently active `Session`. Always `WindowId(1)` until
    /// the multi-session migration step lands; until then this is
    /// effectively a typed wrapper around `working_dir`. New code
    /// should prefer this accessor so the eventual migration is a
    /// no-op for the call site.
    ///
    /// Panics if the active session id is not present in the
    /// `sessions` map. That invariant is upheld by the constructor
    /// and `setActiveWindow` (when added) — if the panic ever fires
    /// it indicates a bug in session lifecycle code.
    pub fn active_window(&self) -> &crate::app::window::Window {
        self.windows
            .get(&self.active_window)
            .expect("active_window id must be a member of sessions")
    }

    /// The active session's id.
    pub fn active_session_id(&self) -> fresh_core::WindowId {
        self.active_window
    }

    /// True iff the editor-global dock is open AND currently holds
    /// keyboard focus. Test helpers use this to wait for `Toggle Dock`'s
    /// async focus-grab to settle before dispatching subsequent keys;
    /// without that readiness check, keys can race into the editor
    /// during the gap and the test silently waits for a dock response
    /// that never comes.
    pub fn is_dock_focused(&self) -> bool {
        self.dock.as_ref().is_some_and(|d| d.focused)
    }

    /// Allocate the next globally-unique `BufferId`. Use this in
    /// `impl Editor` handler bodies that mint new buffer ids. Handlers
    /// that have already moved to `impl Window` use
    /// `Window::alloc_buffer_id` (which delegates to the same
    /// `Arc<BufferIdAllocator>` shared via `WindowResources`).
    ///
    /// Keeps `next_buffer_id` in sync with the allocator's high-water
    /// mark so workspace snapshots that read the `next_buffer_id`
    /// counter directly continue to see a correct value. The
    /// allocator's atomic is the source of truth; this counter mirrors
    /// it for serialization compatibility.
    pub(crate) fn alloc_buffer_id(&mut self) -> fresh_core::BufferId {
        let id = self.buffer_id_alloc.next();
        // Bump the legacy counter past the freshly-issued id so
        // workspace serialization snapshots see a value at least one
        // greater than every issued id.
        if id.0 + 1 > self.next_buffer_id {
            self.next_buffer_id = id.0 + 1;
        }
        id
    }

    /// Number of sessions currently in the editor. Always 1 until
    /// the multi-session step lands.
    pub fn session_count(&self) -> usize {
        self.windows.len()
    }

    /// Look up a session by id. Returns `None` if `id` is not in
    /// the sessions map. Useful for tests; production code that
    /// needs the active session should use `active_window()`.
    pub fn session(&self, id: fresh_core::WindowId) -> Option<&crate::app::window::Window> {
        self.windows.get(&id)
    }

    /// Active session's utility-dock panel-id → buffer-id map.
    /// Used by tests to assert that the active window's dock
    /// occupancy is what was set on it. (Pre-0b this asserted
    /// "warm-swap restored the stash"; post-0b every window owns
    /// its own dock, so the assertion is just "this window's
    /// `panel_ids` map matches expectations.")
    #[doc(hidden)]
    pub fn panel_ids_for_test(&self) -> &std::collections::HashMap<String, fresh_core::BufferId> {
        self.panel_ids()
    }

    /// Inject a panel_ids entry. Used by tests to populate the
    /// active session's dock occupancy without going through the
    /// async plugin command path.
    #[doc(hidden)]
    pub fn insert_panel_id_for_test(&mut self, key: String, buffer_id: fresh_core::BufferId) {
        self.panel_ids_mut().insert(key, buffer_id);
    }

    /// True iff the active session has an LSP manager attached. Every
    /// window now owns one by construction (`Window::new`), so this is
    /// always true; the helper is retained as a regression guard so a
    /// future change that reintroduces a manager-less window state is
    /// caught by the orchestrator-window tests.
    #[doc(hidden)]
    pub fn has_lsp_for_test(&self) -> bool {
        self.lsp().is_some()
    }

    /// Most-recent `path_changed` event the editor received.
    /// Test-only — used by `watch_path` e2e tests to assert
    /// kernel events surfaced to the editor.
    #[doc(hidden)]
    pub fn last_path_change_for_test(&self) -> Option<&(u64, std::path::PathBuf, &'static str)> {
        self.last_path_change_for_test.as_ref()
    }

    /// Most-recent `WatchPathRegistered` plugin response, paired
    /// with its request_id. Test-only.
    #[doc(hidden)]
    pub fn last_watch_response_for_test(&self) -> Option<&(u64, Result<u64, String>)> {
        self.last_watch_response_for_test.as_ref()
    }

    /// Inject an mtime entry into the active session's mod-time
    /// cache. Used by tests to populate `Window.file_mod_times`
    /// without going through real file I/O. (Pre-0b this was
    /// reaching the warm-swap stash; post-0b it's a direct
    /// insert into the active window's cache.)
    #[doc(hidden)]
    pub fn insert_mtime_for_test(&mut self, path: std::path::PathBuf, t: std::time::SystemTime) {
        self.file_mod_times_mut().insert(path, t);
    }

    /// Whether the active session's mtime cache contains `path`.
    #[doc(hidden)]
    pub fn has_mtime_for_test(&self, path: &std::path::Path) -> bool {
        self.file_mod_times().contains_key(path)
    }

    /// Mutable access to the active session. Used by lifecycle code
    /// that re-targets per-session state (renaming, etc.). Same
    /// panic invariant as `active_window()`.
    pub fn active_window_mut(&mut self) -> &mut crate::app::window::Window {
        let id = self.active_window;
        self.windows
            .get_mut(&id)
            .expect("active_window id must be a member of sessions")
    }

    /// Borrow one of the two coexisting widget-panel slots (centered
    /// modal vs. left dock). See `PanelSlot`.
    pub(crate) fn panel(
        &self,
        slot: crate::app::PanelSlot,
    ) -> Option<&crate::app::FloatingWidgetState> {
        match slot {
            crate::app::PanelSlot::Floating => self.floating_widget_panel.as_ref(),
            crate::app::PanelSlot::Dock => self.dock.as_ref(),
        }
    }

    /// Mutable handle to one of the two widget-panel slots.
    pub(crate) fn panel_mut(
        &mut self,
        slot: crate::app::PanelSlot,
    ) -> Option<&mut crate::app::FloatingWidgetState> {
        match slot {
            crate::app::PanelSlot::Floating => self.floating_widget_panel.as_mut(),
            crate::app::PanelSlot::Dock => self.dock.as_mut(),
        }
    }

    /// Mutable handle to the slot *option* itself (for take/assign).
    pub(crate) fn panel_opt_mut(
        &mut self,
        slot: crate::app::PanelSlot,
    ) -> &mut Option<crate::app::FloatingWidgetState> {
        match slot {
            crate::app::PanelSlot::Floating => &mut self.floating_widget_panel,
            crate::app::PanelSlot::Dock => &mut self.dock,
        }
    }

    /// Which slot currently holds the panel with this id, if any.
    #[cfg(feature = "plugins")]
    pub(crate) fn slot_of_panel(&self, panel_id: u64) -> Option<crate::app::PanelSlot> {
        if self
            .floating_widget_panel
            .as_ref()
            .is_some_and(|f| f.panel_id == panel_id)
        {
            Some(crate::app::PanelSlot::Floating)
        } else if self.dock.as_ref().is_some_and(|f| f.panel_id == panel_id) {
            Some(crate::app::PanelSlot::Dock)
        } else {
            None
        }
    }

    /// Map a panel sentinel buffer-id back to its slot.
    pub(crate) fn slot_for_panel_buffer(buffer_id: BufferId) -> Option<crate::app::PanelSlot> {
        if buffer_id == crate::app::FLOATING_PANEL_BUFFER_ID {
            Some(crate::app::PanelSlot::Floating)
        } else if buffer_id == crate::app::DOCK_PANEL_BUFFER_ID {
            Some(crate::app::PanelSlot::Dock)
        } else {
            None
        }
    }

    /// The active window's layout-cache (split-leaf rects, tab rects,
    /// file-explorer rect, view-line mappings). Mouse hit-testing and
    /// visual-line motion read from here.
    pub(crate) fn active_layout(&self) -> &crate::app::types::WindowLayoutCache {
        &self.active_window().layout_cache
    }

    /// Mutable handle to the active window's layout cache. Renderer
    /// writes split / tab / file-explorer hit-test rects here at the
    /// end of each frame.
    pub(crate) fn active_layout_mut(&mut self) -> &mut crate::app::types::WindowLayoutCache {
        &mut self.active_window_mut().layout_cache
    }

    /// The active window's editor-chrome layout cache (status bar,
    /// menu, popups, prompt overlay, full-frame cell-theme map).
    /// Mouse hit-testing reads from here.
    pub(crate) fn active_chrome(&self) -> &crate::app::types::ChromeLayout {
        &self.active_window().chrome_layout
    }

    /// Mutable handle to the active window's chrome-layout cache.
    /// Renderer writes status-bar / menu / popup / prompt-overlay
    /// hit-test rects here at the end of each frame.
    pub(crate) fn active_chrome_mut(&mut self) -> &mut crate::app::types::ChromeLayout {
        &mut self.active_window_mut().chrome_layout
    }

    /// Active window's utility-dock panel-id → buffer-id map.
    /// Each window owns its own dock; switching windows shows a
    /// different (possibly empty) dock.
    pub(crate) fn panel_ids(&self) -> &std::collections::HashMap<String, BufferId> {
        &self.active_window().panel_ids
    }

    /// Mutable handle to the active window's panel-id map.
    pub(crate) fn panel_ids_mut(&mut self) -> &mut std::collections::HashMap<String, BufferId> {
        &mut self.active_window_mut().panel_ids
    }

    /// Active window's open-file mtime cache. Auto-revert only
    /// fires for files in the active window — dormant windows
    /// keep their mtime snapshot until the next dive.
    pub(crate) fn file_mod_times(
        &self,
    ) -> &std::collections::HashMap<std::path::PathBuf, std::time::SystemTime> {
        &self.active_window().file_mod_times
    }

    /// Mutable handle to the active window's mtime cache.
    pub(crate) fn file_mod_times_mut(
        &mut self,
    ) -> &mut std::collections::HashMap<std::path::PathBuf, std::time::SystemTime> {
        &mut self.active_window_mut().file_mod_times
    }

    /// Active window's file-explorer view (`None` if it's never been
    /// opened in this window). Each window has its own tree;
    /// switching windows shows that window's view (or none).
    pub fn file_explorer(&self) -> Option<&FileTreeView> {
        self.active_window().file_explorer.as_ref()
    }

    /// Mutable handle to the active window's file-explorer view.
    /// Holds `&mut self` for the call's lifetime — for sites that
    /// also need to read other Editor fields, use direct
    /// `self.windows.get_mut(&self.active_window).and_then(|w| w.file_explorer.as_mut())`
    /// instead so the borrow on `self.windows` stays disjoint.
    pub fn file_explorer_mut(&mut self) -> Option<&mut FileTreeView> {
        self.active_window_mut().file_explorer.as_mut()
    }

    /// Active window's buffer storage. Each window owns its
    /// `EditorState` map outright; closing the window drops them.
    /// Cross-window iteration goes through `self.windows.values()`
    /// directly.
    pub(crate) fn buffers(&self) -> &crate::app::window::WindowBuffers {
        &self.active_window().buffers
    }

    /// Mutable handle to the active window's buffer storage.
    /// Holds `&mut self` for the call's lifetime — at sites that
    /// need a concurrent mutable borrow on another Window field
    /// (`splits`, `event_logs`, etc.) take a single
    /// `let window = self.windows.get_mut(&self.active_window).unwrap()`
    /// and split-access the disjoint sub-fields directly.
    pub(crate) fn buffers_mut(&mut self) -> &mut crate::app::window::WindowBuffers {
        &mut self.active_window_mut().buffers
    }

    /// Active window's LSP manager. Each window owns one rooted at its
    /// project root (built in `Window::new`), so this is always
    /// present; the `Option` is retained for call-site ergonomics and
    /// because the active-window lookup is itself fallible in spirit.
    pub(crate) fn lsp(&self) -> Option<&crate::services::lsp::manager::LspManager> {
        Some(&self.active_window().lsp)
    }

    /// Mutable handle to the active window's LSP manager. Same
    /// borrow caveat as `file_explorer_mut()`: at sites that also
    /// need to read other Editor fields, prefer direct
    /// `self.windows.get_mut(&self.active_window).map(|w| &mut w.lsp)`.
    pub(crate) fn lsp_mut(&mut self) -> Option<&mut crate::services::lsp::manager::LspManager> {
        Some(&mut self.active_window_mut().lsp)
    }

    /// Active window's split tree. Panics if the window has no
    /// layout yet — the invariant is "the active window always has
    /// `splits` populated", upheld by `set_active_window` (which
    /// seeds the layout on first dive) and by editor init (which
    /// hands the initial layout to the base window).
    pub(crate) fn split_manager(&self) -> &crate::view::split::SplitManager {
        &self
            .active_window()
            .buffers
            .splits()
            .expect("active window must have a populated split layout")
            .0
    }

    /// Mutable handle to the active window's split tree.
    pub(crate) fn split_manager_mut(&mut self) -> &mut crate::view::split::SplitManager {
        &mut self
            .active_window_mut()
            .buffers
            .splits_mut()
            .expect("active window must have a populated split layout")
            .0
    }

    /// Active window's per-leaf view state map.
    #[cfg(test)]
    pub(crate) fn split_view_states(
        &self,
    ) -> &std::collections::HashMap<crate::model::event::LeafId, crate::view::split::SplitViewState>
    {
        &self
            .active_window()
            .buffers
            .splits()
            .expect("active window must have a populated split layout")
            .1
    }

    /// Mutable handle to the active window's per-leaf view state map.
    pub(crate) fn split_view_states_mut(
        &mut self,
    ) -> &mut std::collections::HashMap<
        crate::model::event::LeafId,
        crate::view::split::SplitViewState,
    > {
        &mut self
            .active_window_mut()
            .buffers
            .splits_mut()
            .expect("active window must have a populated split layout")
            .1
    }

    /// Return buffer ids whose on-disk path sits at or under `root`.
    /// Used by file-explorer operations that need to react when a file
    /// or directory on disk goes away or moves.
    pub fn buffer_ids_under_path(&self, root: &std::path::Path) -> Vec<BufferId> {
        self.windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .iter()
            .filter_map(|(id, state)| {
                let p = state.buffer.file_path()?;
                if p == root || p.starts_with(root) {
                    Some(*id)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Get remote connection info if editing remote files
    ///
    /// Returns `Some("user@host")` for remote editing, `None` for local.
    pub fn remote_connection_info(&self) -> Option<&str> {
        self.authority.filesystem.remote_connection_info()
    }

    /// Get connection string for display in status bar and file explorer.
    ///
    /// Per principle 9, identity lives in the authority. The label set
    /// by whoever constructed the authority wins; if it is empty (the
    /// SSH constructor leaves it that way) we fall back to the
    /// filesystem's `remote_connection_info()`, which knows how to
    /// annotate disconnected SSH sessions.
    pub fn connection_display_string(&self) -> Option<String> {
        if !self.authority.display_label.is_empty() {
            return Some(self.authority.display_label.clone());
        }
        self.remote_connection_info().map(|conn| {
            if self.authority.filesystem.is_remote_connected() {
                conn.to_string()
            } else {
                format!("{} (Disconnected)", conn)
            }
        })
    }

    /// Get the status log path
    pub fn get_status_log_path(&self) -> Option<&PathBuf> {
        self.status_log_path.as_ref()
    }

    /// Open the status log file (user clicked on status message)
    pub fn open_status_log(&mut self) {
        if let Some(path) = self.status_log_path.clone() {
            // Use open_local_file since log files are always local
            match self.active_window_mut().open_local_file(&path) {
                Ok(buffer_id) => {
                    self.active_window_mut()
                        .mark_buffer_read_only(buffer_id, true);
                }
                Err(e) => {
                    tracing::error!("Failed to open status log: {}", e);
                }
            }
        } else {
            self.set_status_message("Status log not available".to_string());
        }
    }

    /// Check for and handle any new warnings in the warning log
    ///
    /// Updates the general warning domain for the status bar.
    /// Returns true if new warnings were found.
    pub fn check_warning_log(&mut self) -> bool {
        let path = match &self.warning_log {
            Some((receiver, path)) => {
                let mut new_warning_count = 0usize;
                while receiver.try_recv().is_ok() {
                    new_warning_count += 1;
                }
                if new_warning_count == 0 {
                    return false;
                }
                (path.clone(), new_warning_count)
            }
            None => return false,
        };
        let (path, new_warning_count) = path;
        self.active_window_mut()
            .warning_domains
            .general
            .add_warnings(new_warning_count);
        self.active_window_mut()
            .warning_domains
            .general
            .set_log_path(path);

        true
    }

    /// Get the warning domain registry
    // Warning-domain accessors live on `impl Window`:
    //  - `clear_warnings` — call as `self.active_window_mut().clear_warnings()`.
    //  - Read access via `active_window().warning_domains` directly
    //    (and its `.general` / `.lsp` sub-registries).
    // `has_lsp_error`, `get_effective_warning_level`,
    // `get_general_warning_level`, `get_general_warning_count`,
    // `get_warning_domains`, `get_warning_log_path`,
    // `clear_warning_indicator` were thin getters with no remaining
    // callers and have been removed.

    /// Open the warning log file (user-initiated action). Stays on
    /// `impl Editor` because it calls editor-orchestration helpers
    /// (`open_local_file`, `mark_buffer_read_only`).
    pub fn open_warning_log(&mut self) {
        if let Some(path) = self
            .active_window_mut()
            .warning_domains
            .general
            .log_path
            .clone()
        {
            // Use open_local_file since log files are always local
            match self.active_window_mut().open_local_file(&path) {
                Ok(buffer_id) => {
                    self.active_window_mut()
                        .mark_buffer_read_only(buffer_id, true);
                }
                Err(e) => {
                    tracing::error!("Failed to open warning log: {}", e);
                }
            }
        }
    }

    // `update_lsp_warning_domain` lives on `impl Window` — call it via
    // `self.active_window_mut().update_lsp_warning_domain()`.

    /// Check if mouse hover timer has expired and trigger LSP hover request
    ///
    /// This implements debounced hover - we wait for the configured delay before
    /// sending the request to avoid spamming the LSP server on every mouse move.
    /// Returns true if a hover request was triggered.
    pub fn check_mouse_hover_timer(&mut self) -> bool {
        // Check if mouse hover is enabled
        if !self.config.editor.mouse_hover_enabled {
            return false;
        }

        let hover_delay = std::time::Duration::from_millis(self.config.editor.mouse_hover_delay_ms);

        // Get hover state without borrowing self
        let hover_info = match self.active_window_mut().mouse_state.lsp_hover_state {
            Some((byte_pos, start_time, screen_x, screen_y)) => {
                if self.active_window_mut().mouse_state.lsp_hover_request_sent {
                    return false; // Already sent request for this position
                }
                if start_time.elapsed() < hover_delay {
                    return false; // Timer hasn't expired yet
                }
                Some((byte_pos, screen_x, screen_y))
            }
            None => return false,
        };

        let Some((byte_pos, screen_x, screen_y)) = hover_info else {
            return false;
        };

        // Store mouse position for popup positioning
        self.active_window_mut()
            .hover
            .set_screen_position((screen_x, screen_y));

        // Request hover at the byte position — only mark as sent if dispatched
        match self.request_hover_at_position(byte_pos) {
            Ok(true) => {
                self.active_window_mut().mouse_state.lsp_hover_request_sent = true;
                true
            }
            Ok(false) => false, // no server ready, timer will retry
            Err(e) => {
                tracing::debug!("Failed to request hover: {}", e);
                false
            }
        }
    }

    // `check_semantic_highlight_timer` lives on `impl Window` — call it
    // via `self.active_window().check_semantic_highlight_timer()`.

    // `check_diagnostic_pull_timer` lives on `impl Window` — call it via
    // `self.active_window_mut().check_diagnostic_pull_timer()`. Pulls
    // run against the active window's LSP manager and its per-window
    // `scheduled_diagnostic_pull` debounce slot.

    /// Check if completion trigger timer has expired and trigger completion if so
    ///
    /// This implements debounced completion - we wait for quick_suggestions_delay_ms
    /// before sending the completion request to avoid spamming the LSP server.
    /// Returns true if a completion request was triggered.
    pub fn check_completion_trigger_timer(&mut self) -> bool {
        // Check if we have a scheduled completion trigger
        let Some(trigger_time) = self.active_window_mut().scheduled_completion_trigger else {
            return false;
        };

        // Check if the timer has expired
        if Instant::now() < trigger_time {
            return false;
        }

        // Clear the scheduled trigger
        self.active_window_mut().scheduled_completion_trigger = None;

        // Don't trigger if a popup is already visible
        if self.active_state().popups.is_visible() {
            return false;
        }

        // Trigger the completion request
        self.request_completion();

        true
    }
}
