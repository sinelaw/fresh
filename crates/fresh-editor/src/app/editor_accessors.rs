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
    pub fn config_mut(&mut self) -> &mut Config {
        Arc::make_mut(&mut self.config)
    }

    /// Replace the config wholesale. Used by the "reload config" path and
    /// by tests that want to swap in a freshly-parsed file. Constructs a
    /// fresh `Arc`, so any snapshot that still holds the old value sees
    /// the pointer move and will reserialize on the next refresh.
    pub fn set_config(&mut self, new_config: Config) {
        self.config = Arc::new(new_config);
    }

    /// Replace the cached raw user config. Like `set_config`, constructs
    /// a fresh `Arc` so the plugin snapshot notices the change.
    pub(crate) fn set_user_config_raw(&mut self, value: serde_json::Value) {
        self.user_config_raw = Arc::new(value);
    }

    /// Mutable access to the merged diagnostics map. Routes through
    /// `Arc::make_mut`, which CoW-clones while the plugin snapshot still
    /// holds the old map — readers never observe an in-place mutation.
    pub(crate) fn stored_diagnostics_mut(
        &mut self,
    ) -> &mut HashMap<String, Vec<lsp_types::Diagnostic>> {
        Arc::make_mut(&mut self.stored_diagnostics)
    }

    /// Mutable access to the folding-ranges map. CoW-clones through
    /// `Arc::make_mut` for the same reason as `stored_diagnostics_mut`.
    pub(crate) fn stored_folding_ranges_mut(
        &mut self,
    ) -> &mut HashMap<String, Vec<lsp_types::FoldingRange>> {
        Arc::make_mut(&mut self.stored_folding_ranges)
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
        self.plugin_manager.deliver_response(response);
    }

    /// Remove a pending semantic token request from tracking maps.
    pub(super) fn take_pending_semantic_token_request(
        &mut self,
        request_id: u64,
    ) -> Option<SemanticTokenFullRequest> {
        if let Some(request) = self.pending_semantic_token_requests.remove(&request_id) {
            self.semantic_tokens_in_flight.remove(&request.buffer_id);
            Some(request)
        } else {
            None
        }
    }

    /// Remove a pending semantic token range request from tracking maps.
    pub(super) fn take_pending_semantic_token_range_request(
        &mut self,
        request_id: u64,
    ) -> Option<SemanticTokenRangeRequest> {
        if let Some(request) = self
            .pending_semantic_token_range_requests
            .remove(&request_id)
        {
            self.semantic_tokens_range_in_flight
                .remove(&request.buffer_id);
            Some(request)
        } else {
            None
        }
    }

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
            .find_keybinding_for_action(action_name, self.key_context.clone())
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
        let active_split = self.split_manager.active_split();
        if let Some(vs) = self.split_view_states.get(&active_split) {
            if vs.active_group_tab.is_some() {
                if let Some(inner_leaf) = vs.focused_group_leaf {
                    if let Some(inner_vs) = self.split_view_states.get(&inner_leaf) {
                        let inner_buf = inner_vs.active_buffer;
                        if self.buffers.contains_key(&inner_buf)
                            && inner_vs.keyed_states.contains_key(&inner_buf)
                        {
                            return (inner_leaf, inner_buf);
                        }
                    }
                }
            }
        }
        let outer_buf = self
            .split_manager
            .active_buffer_id()
            .expect("Editor always has at least one buffer");
        (active_split, outer_buf)
    }

    /// Get the mode name for the active buffer (if it's a virtual buffer)
    pub fn active_buffer_mode(&self) -> Option<&str> {
        self.buffer_metadata
            .get(&self.active_buffer())
            .and_then(|meta| meta.virtual_mode())
    }

    /// Check if the active buffer is read-only
    pub fn is_active_buffer_read_only(&self) -> bool {
        if let Some(metadata) = self.buffer_metadata.get(&self.active_buffer()) {
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

    /// Check if editing should be disabled for the active buffer
    /// This returns true when editing_disabled is true (e.g., for read-only virtual buffers)
    pub fn is_editing_disabled(&self) -> bool {
        self.active_state().editing_disabled
    }

    /// Mark a buffer as read-only, setting both metadata and editor state consistently.
    /// This is the single entry point for making a buffer read-only.
    pub fn mark_buffer_read_only(&mut self, buffer_id: BufferId, read_only: bool) {
        if let Some(metadata) = self.buffer_metadata.get_mut(&buffer_id) {
            metadata.read_only = read_only;
        }
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.editing_disabled = read_only;
        }
    }

    /// Get the effective mode for the active buffer.
    ///
    /// Buffer-local mode (virtual buffers) takes precedence over the global
    /// editor mode, so that e.g. a search-replace panel isn't hijacked by
    /// a markdown-source or vi-mode global mode.
    pub fn effective_mode(&self) -> Option<&str> {
        self.active_buffer_mode().or(self.editor_mode.as_deref())
    }

    /// Check if LSP has any active progress tasks (e.g., indexing)
    pub fn has_active_lsp_progress(&self) -> bool {
        !self.lsp_progress.is_empty()
    }

    /// Toggle the LSP auto-prompt popup on this editor instance.
    ///
    /// See `app::lsp_auto_prompt` for the full rationale. In short:
    /// tests default this to `false` to stop the popup from
    /// swallowing keystrokes in scenarios that don't exercise LSP;
    /// tests that DO exercise it re-enable on the specific harness
    /// they care about.
    pub fn set_lsp_auto_prompt_enabled(&mut self, enabled: bool) {
        self.lsp_auto_prompt_enabled = enabled;
    }

    /// Get the current LSP progress info (if any)
    pub fn get_lsp_progress(&self) -> Vec<(String, String, Option<String>)> {
        self.lsp_progress
            .iter()
            .map(|(token, info)| (token.clone(), info.title.clone(), info.message.clone()))
            .collect()
    }

    /// Check if any LSP server for a given language is running (ready)
    pub fn is_lsp_server_ready(&self, language: &str) -> bool {
        use crate::services::async_bridge::LspServerStatus;
        self.lsp_server_statuses
            .iter()
            .any(|((lang, server_name), status)| {
                if !matches!(status, LspServerStatus::Running) {
                    return false;
                }
                if lang == language {
                    return true;
                }
                // Check if this server's scope accepts the queried language
                self.lsp
                    .as_ref()
                    .and_then(|lsp| lsp.server_scope(server_name))
                    .map(|scope| scope.accepts(language))
                    .unwrap_or(false)
            })
    }

    /// Get stored LSP diagnostics (for testing and external access)
    /// Returns a reference to the diagnostics map keyed by file URI
    pub fn get_stored_diagnostics(&self) -> &HashMap<String, Vec<lsp_types::Diagnostic>> {
        &self.stored_diagnostics
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
        if let Some(ref mut lsp) = self.lsp {
            lsp.set_language_configs(language, config);
        }
    }

    /// Get a list of currently running LSP server languages
    pub fn running_lsp_servers(&self) -> Vec<String> {
        self.lsp
            .as_ref()
            .map(|lsp| lsp.running_servers())
            .unwrap_or_default()
    }

    /// Return the number of pending completion requests.
    pub fn pending_completion_requests_count(&self) -> usize {
        self.pending_completion_requests.len()
    }

    /// Return the number of stored completion items.
    pub fn completion_items_count(&self) -> usize {
        self.completion_items.as_ref().map_or(0, |v| v.len())
    }

    /// Return the number of initialized LSP servers for a given language.
    pub fn initialized_lsp_server_count(&self, language: &str) -> usize {
        self.lsp
            .as_ref()
            .map(|lsp| {
                lsp.get_handles(language)
                    .iter()
                    .filter(|sh| sh.capabilities.initialized)
                    .count()
            })
            .unwrap_or(0)
    }

    /// Shutdown an LSP server by language (marks it as disabled until manual restart)
    ///
    /// Returns true if the server was found and shutdown, false otherwise
    pub fn shutdown_lsp_server(&mut self, language: &str) -> bool {
        if let Some(ref mut lsp) = self.lsp {
            lsp.shutdown_server(language)
        } else {
            false
        }
    }

    /// Enable event log streaming to a file
    pub fn enable_event_streaming<P: AsRef<Path>>(&mut self, path: P) -> AnyhowResult<()> {
        // Enable streaming for all existing event logs
        for event_log in self.event_logs.values_mut() {
            event_log.enable_streaming(&path)?;
        }
        Ok(())
    }

    /// Log keystroke for debugging
    pub fn log_keystroke(&mut self, key_code: &str, modifiers: &str) {
        if let Some(event_log) = self.event_logs.get_mut(&self.active_buffer()) {
            event_log.log_keystroke(key_code, modifiers);
        }
    }

    /// Set up warning log monitoring
    ///
    /// When warnings/errors are logged, they will be written to the specified path
    /// and the editor will be notified via the receiver.
    pub fn set_warning_log(&mut self, receiver: std::sync::mpsc::Receiver<()>, path: PathBuf) {
        self.warning_log = Some((receiver, path));
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
        self.request_restart(self.working_dir.clone());
    }

    /// Restore the default local authority. Same destructive-restart
    /// semantics as `install_authority` — the caller never observes a
    /// half-transitioned editor.
    pub fn clear_authority(&mut self) {
        self.install_authority(crate::services::authority::Authority::local());
    }

    /// Take the queued authority (if any). Called by `main.rs` on
    /// restart to move the queued authority into the fresh editor.
    pub fn take_pending_authority(&mut self) -> Option<crate::services::authority::Authority> {
        self.pending_authority.take()
    }

    /// Directly replace the active authority without triggering a
    /// restart. Intended for the post-construction wiring in `main.rs`
    /// only, where the editor is still being set up and there is no
    /// user-visible state to preserve. Do not call this from the event
    /// loop — use `install_authority` for that.
    pub fn set_boot_authority(&mut self, authority: crate::services::authority::Authority) {
        self.authority = authority;
    }

    /// Read-only access to the active authority.
    pub fn authority(&self) -> &crate::services::authority::Authority {
        &self.authority
    }

    /// The editor's current working directory.  This is the project
    /// root; individual buffers may live elsewhere.
    pub fn working_dir(&self) -> &std::path::Path {
        &self.working_dir
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
            match self.open_local_file(&path) {
                Ok(buffer_id) => {
                    self.mark_buffer_read_only(buffer_id, true);
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
        let Some((receiver, path)) = &self.warning_log else {
            return false;
        };

        // Non-blocking check for any warnings
        let mut new_warning_count = 0usize;
        while receiver.try_recv().is_ok() {
            new_warning_count += 1;
        }

        if new_warning_count > 0 {
            // Update general warning domain (don't auto-open file)
            self.warning_domains.general.add_warnings(new_warning_count);
            self.warning_domains.general.set_log_path(path.clone());
        }

        new_warning_count > 0
    }

    /// Get the warning domain registry
    pub fn get_warning_domains(&self) -> &WarningDomainRegistry {
        &self.warning_domains
    }

    /// Get the warning log path (for opening when user clicks indicator)
    pub fn get_warning_log_path(&self) -> Option<&PathBuf> {
        self.warning_domains.general.log_path.as_ref()
    }

    /// Open the warning log file (user-initiated action)
    pub fn open_warning_log(&mut self) {
        if let Some(path) = self.warning_domains.general.log_path.clone() {
            // Use open_local_file since log files are always local
            match self.open_local_file(&path) {
                Ok(buffer_id) => {
                    self.mark_buffer_read_only(buffer_id, true);
                }
                Err(e) => {
                    tracing::error!("Failed to open warning log: {}", e);
                }
            }
        }
    }

    /// Clear the general warning indicator (user dismissed)
    pub fn clear_warning_indicator(&mut self) {
        self.warning_domains.general.clear();
    }

    /// Clear all warning indicators (user dismissed via command)
    pub fn clear_warnings(&mut self) {
        self.warning_domains.general.clear();
        self.warning_domains.lsp.clear();
        self.status_message = Some("Warnings cleared".to_string());
    }

    /// Check if any LSP server is in error state
    pub fn has_lsp_error(&self) -> bool {
        self.warning_domains.lsp.level() == WarningLevel::Error
    }

    /// Get the effective warning level for the status bar (LSP indicator)
    /// Returns Error if LSP has errors, Warning if there are warnings, None otherwise
    pub fn get_effective_warning_level(&self) -> WarningLevel {
        self.warning_domains.lsp.level()
    }

    /// Get the general warning level (for the general warning badge)
    pub fn get_general_warning_level(&self) -> WarningLevel {
        self.warning_domains.general.level()
    }

    /// Get the general warning count
    pub fn get_general_warning_count(&self) -> usize {
        self.warning_domains.general.count
    }

    /// Update LSP warning domain from server statuses
    pub fn update_lsp_warning_domain(&mut self) {
        self.warning_domains
            .lsp
            .update_from_statuses(&self.lsp_server_statuses);
    }

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
        let hover_info = match self.mouse_state.lsp_hover_state {
            Some((byte_pos, start_time, screen_x, screen_y)) => {
                if self.mouse_state.lsp_hover_request_sent {
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
        self.hover.set_screen_position((screen_x, screen_y));

        // Request hover at the byte position — only mark as sent if dispatched
        match self.request_hover_at_position(byte_pos) {
            Ok(true) => {
                self.mouse_state.lsp_hover_request_sent = true;
                true
            }
            Ok(false) => false, // no server ready, timer will retry
            Err(e) => {
                tracing::debug!("Failed to request hover: {}", e);
                false
            }
        }
    }

    /// Check if semantic highlight debounce timer has expired
    ///
    /// Returns true if a redraw is needed because the debounce period has elapsed
    /// and semantic highlights need to be recomputed.
    pub fn check_semantic_highlight_timer(&self) -> bool {
        // Check all buffers for pending semantic highlight redraws
        for state in self.buffers.values() {
            if let Some(remaining) = state.reference_highlight_overlay.needs_redraw() {
                if remaining.is_zero() {
                    return true;
                }
            }
        }
        false
    }

    /// Check if diagnostic pull timer has expired and trigger re-pull if so.
    ///
    /// Debounced diagnostic re-pull after document changes — waits 500ms after
    /// the last edit before requesting fresh diagnostics from the LSP server.
    pub fn check_diagnostic_pull_timer(&mut self) -> bool {
        let Some((buffer_id, trigger_time)) = self.scheduled_diagnostic_pull else {
            return false;
        };

        if Instant::now() < trigger_time {
            return false;
        }

        self.scheduled_diagnostic_pull = None;

        // Get URI and language for this buffer
        let Some(metadata) = self.buffer_metadata.get(&buffer_id) else {
            return false;
        };
        let Some(uri) = metadata.file_uri().cloned() else {
            return false;
        };
        let Some(language) = self.buffers.get(&buffer_id).map(|s| s.language.clone()) else {
            return false;
        };

        let Some(lsp) = self.lsp.as_mut() else {
            return false;
        };
        let Some(sh) = lsp.handle_for_feature_mut(&language, crate::types::LspFeature::Diagnostics)
        else {
            return false;
        };
        let client = &mut sh.handle;

        let request_id = self.next_lsp_request_id;
        self.next_lsp_request_id += 1;
        let previous_result_id = self.diagnostic_result_ids.get(uri.as_str()).cloned();
        if let Err(e) = client.document_diagnostic(request_id, uri.clone(), previous_result_id) {
            tracing::debug!(
                "Failed to pull diagnostics after edit for {}: {}",
                uri.as_str(),
                e
            );
        } else {
            tracing::debug!(
                "Pulling diagnostics after edit for {} (request_id={})",
                uri.as_str(),
                request_id
            );
        }

        false // no immediate redraw needed; diagnostics arrive asynchronously
    }

    /// Check if completion trigger timer has expired and trigger completion if so
    ///
    /// This implements debounced completion - we wait for quick_suggestions_delay_ms
    /// before sending the completion request to avoid spamming the LSP server.
    /// Returns true if a completion request was triggered.
    pub fn check_completion_trigger_timer(&mut self) -> bool {
        // Check if we have a scheduled completion trigger
        let Some(trigger_time) = self.scheduled_completion_trigger else {
            return false;
        };

        // Check if the timer has expired
        if Instant::now() < trigger_time {
            return false;
        }

        // Clear the scheduled trigger
        self.scheduled_completion_trigger = None;

        // Don't trigger if a popup is already visible
        if self.active_state().popups.is_visible() {
            return false;
        }

        // Trigger the completion request
        self.request_completion();

        true
    }
}
