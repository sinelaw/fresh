//! Async-message dispatch on `Editor`.
//!
//! `process_async_messages` runs each frame and drains the AsyncBridge,
//! routing each AsyncMessage to its handler — LSP responses,
//! initialization/errors, plugin commands, filesystem polling, etc. The
//! `match` is a thin dispatch table: every arm forwards to a `handle_*`
//! method on `Editor` that owns the actual logic for that variant.

use rust_i18n::t;

use crate::services::async_bridge::AsyncMessage;
use crate::view::prompt::PromptType;

use super::Editor;

impl Editor {
    /// Resolve the `attachRemoteAgent` promise behind `request_id` — the
    /// session (authority + window) is fully constructed. Resolves with `null`;
    /// the plugin only needs the success signal to close its dialog. Lives here
    /// (not in the plugins-gated `plugin_dispatch`) because the non-plugin
    /// `RemoteAttach*` async handlers call it; the plugin manager is a no-op
    /// without the `plugins` feature, so this safely does nothing then.
    pub(crate) fn resolve_remote_attach(&self, request_id: u64) {
        self.plugin_manager.read().unwrap().resolve_callback(
            fresh_core::api::JsCallbackId::from(request_id),
            "null".to_string(),
        );
    }

    /// Reject the `attachRemoteAgent` promise behind `request_id` with `error`
    /// — the connect failed, the spec was bad / the runtime unavailable, or
    /// window creation failed. The plugin surfaces the reason and creates no
    /// window.
    pub(crate) fn reject_remote_attach(&self, request_id: u64, error: String) {
        tracing::warn!("attachRemoteAgent rejected: {error}");
        self.plugin_manager
            .read()
            .unwrap()
            .reject_callback(fresh_core::api::JsCallbackId::from(request_id), error);
    }

    /// Mark every in-flight `attachRemoteAgent` connect as cancelled, signal the
    /// background connect thread to tear down its in-flight carrier (killing the
    /// ssh/kubectl child), and reject the awaiting promise now. If a connect
    /// races past cancellation its eventual `RemoteAttachReady`/`Failed` is
    /// dropped on arrival (see `remote_attach_was_cancelled`) — so no window is
    /// ever built. This is the host side of the New-Session dialog's Cancel.
    pub(crate) fn cancel_remote_attaches(&mut self) {
        let inflight: Vec<u64> = self.remote_attach_inflight.drain().collect();
        let any = !inflight.is_empty();
        for id in inflight {
            self.remote_attach_cancelled.insert(id);
            // Signal the background connect thread to abort. Its `select!` drops
            // the in-flight connect future, which drops the ssh child (spawned
            // kill-on-drop), so even a host that never completes the handshake
            // leaves no orphaned process. A connect that already finished (its
            // result still queued) ignores the signal; the late result is
            // discarded by `remote_attach_was_cancelled`.
            if let Some(cancel) = self.remote_attach_cancels.remove(&id) {
                #[allow(clippy::let_underscore_must_use)]
                let _ = cancel.send(());
            }
            self.reject_remote_attach(id, "cancelled".to_string());
        }
        // Clear the lingering "Connecting to …" status the connect set, so the
        // status line doesn't keep claiming a connection is in progress.
        if any {
            self.set_status_message("Connection cancelled".to_string());
        }
    }

    /// Consume the in-flight/cancelled tracking for `request_id` as a late
    /// result arrives. Returns `true` if the connect was cancelled (the result
    /// should be discarded), `false` for a normal completion (which still
    /// clears the in-flight entry).
    pub(crate) fn remote_attach_was_cancelled(&mut self, request_id: u64) -> bool {
        self.remote_attach_inflight.remove(&request_id);
        self.remote_attach_cancels.remove(&request_id);
        self.remote_attach_cancelled.remove(&request_id)
    }

    /// Process pending async messages from the async bridge
    ///
    /// This should be called each frame in the main loop to handle:
    /// - LSP diagnostics
    /// - LSP initialization/errors
    /// - File system changes (future)
    /// - Git status updates
    pub fn process_async_messages(&mut self) -> bool {
        // Check plugin thread health - will panic if thread died due to error
        // This ensures plugin errors surface quickly instead of causing silent hangs
        self.plugin_manager.write().unwrap().check_thread_health();

        // Lazily wire an event-driven reconnect forwarder for each remote
        // window (idempotent; cheap when already wired). This replaces the old
        // per-frame connection-state poll: the forwarder awaits the channel's
        // reconnect notification and posts `RemoteReconnected`.
        self.ensure_remote_reconnect_forwarders();

        let Some(bridge) = &self.async_bridge else {
            return false;
        };

        // Drain editor-global async messages first (plugin runtime
        // callbacks, file dialog, etc.), then drain each window's
        // per-window bridge (LSP responses, terminal output, etc.).
        // Order matters only for cosmetic message ordering on a
        // very-busy frame; semantically the dispatcher is the same
        // for every source.
        let mut messages = {
            let _s = tracing::info_span!("try_recv_all").entered();
            bridge.try_recv_all()
        };
        for window in self.windows.values() {
            messages.extend(window.bridge.try_recv_all());
        }
        // A render is only warranted if a message can actually change the
        // screen. A `DelayComplete` just resolves a debounced
        // `editor.delay()` callback in the plugin runtime; on its own it
        // paints nothing. Any visual outcome of the resumed plugin code
        // arrives as a follow-up plugin *command* and is caught by
        // `process_plugin_commands`'s `has_visual_commands` check below (or
        // on the next tick). Forcing a render for the bare completion made
        // live_diff's per-keystroke debounce repaint the screen with no
        // change — invisible locally, but real lag over serial (#2100).
        let needs_render = messages.iter().any(|m| {
            !matches!(
                m,
                AsyncMessage::Plugin(fresh_core::api::PluginAsyncMessage::DelayComplete { .. })
            )
        });
        tracing::trace!(
            async_message_count = messages.len(),
            "received async messages"
        );

        for message in messages {
            match message {
                AsyncMessage::LspDiagnostics {
                    uri,
                    diagnostics,
                    server_name,
                } => {
                    self.handle_lsp_diagnostics(uri, diagnostics, server_name);
                }
                AsyncMessage::LspInitialized {
                    language,
                    server_name,
                    capabilities,
                } => {
                    self.handle_lsp_initialized(language, server_name, capabilities);
                }
                AsyncMessage::LspError {
                    language,
                    error,
                    stderr_log_path,
                } => {
                    self.handle_lsp_error(language, error, stderr_log_path);
                }
                AsyncMessage::LspCompletion { request_id, items } => {
                    if let Err(e) = self.handle_completion_response(request_id, items) {
                        tracing::error!("Error handling completion response: {}", e);
                    }
                }
                AsyncMessage::LspGotoDefinition {
                    request_id,
                    locations,
                } => {
                    if let Err(e) = self.handle_goto_definition_response(request_id, locations) {
                        tracing::error!("Error handling goto definition response: {}", e);
                    }
                }
                AsyncMessage::LspImplementation {
                    request_id,
                    locations,
                } => {
                    if let Err(e) = self.handle_implementation_response(request_id, locations) {
                        tracing::error!("Error handling implementation response: {}", e);
                    }
                }
                AsyncMessage::LspRename { request_id, result } => {
                    if let Err(e) = self.handle_rename_response(request_id, result) {
                        tracing::error!("Error handling rename response: {}", e);
                    }
                }
                AsyncMessage::LspHover {
                    request_id,
                    contents,
                    is_markdown,
                    range,
                } => {
                    self.handle_hover_response(request_id, contents, is_markdown, range);
                }
                AsyncMessage::LspReferences {
                    request_id,
                    locations,
                } => {
                    if let Err(e) = self.handle_references_response(request_id, locations) {
                        tracing::error!("Error handling references response: {}", e);
                    }
                }
                AsyncMessage::LspSignatureHelp {
                    request_id,
                    signature_help,
                } => {
                    self.handle_signature_help_response(request_id, signature_help);
                }
                AsyncMessage::LspCodeActions {
                    request_id,
                    actions,
                } => {
                    self.handle_code_actions_response(request_id, actions);
                }
                AsyncMessage::LspApplyEdit { edit, label } => {
                    self.handle_lsp_apply_edit(edit, label);
                }
                AsyncMessage::LspCodeActionResolved {
                    request_id: _,
                    action,
                } => {
                    self.handle_lsp_code_action_resolved(action);
                }
                AsyncMessage::LspCompletionResolved {
                    request_id: _,
                    item,
                } => {
                    if let Ok(resolved) = item {
                        self.handle_completion_resolved(resolved);
                    }
                }
                AsyncMessage::LspFormatting {
                    request_id: _,
                    uri,
                    edits,
                } => {
                    if !edits.is_empty() {
                        if let Err(e) = self.apply_formatting_edits(&uri, edits) {
                            tracing::error!("Failed to apply formatting: {}", e);
                        }
                    }
                }
                AsyncMessage::LspPrepareRename {
                    request_id: _,
                    result,
                } => {
                    self.handle_prepare_rename_response(result);
                }
                AsyncMessage::LspPulledDiagnostics {
                    request_id: _,
                    uri,
                    server_name,
                    result_id,
                    diagnostics,
                    unchanged,
                } => {
                    self.handle_lsp_pulled_diagnostics(
                        uri,
                        server_name,
                        result_id,
                        diagnostics,
                        unchanged,
                    );
                }
                AsyncMessage::LspInlayHints {
                    request_id,
                    uri,
                    hints,
                } => {
                    self.handle_lsp_inlay_hints(request_id, uri, hints);
                }
                AsyncMessage::LspFoldingRanges {
                    request_id,
                    uri,
                    ranges,
                } => {
                    self.handle_lsp_folding_ranges(request_id, uri, ranges);
                }
                AsyncMessage::LspSemanticTokens {
                    request_id,
                    uri,
                    response,
                } => {
                    self.handle_lsp_semantic_tokens(request_id, uri, response);
                }
                AsyncMessage::LspServerQuiescent { language } => {
                    self.handle_lsp_server_quiescent(language);
                }
                AsyncMessage::LspDiagnosticRefresh { language } => {
                    self.handle_lsp_diagnostic_refresh(language);
                }
                AsyncMessage::LspInlayHintRefresh { language } => {
                    self.handle_lsp_inlay_hint_refresh(language);
                }
                AsyncMessage::LspSemanticTokensRefresh { language } => {
                    self.handle_lsp_semantic_tokens_refresh(language);
                }
                AsyncMessage::LspDynamicCapabilities {
                    language,
                    server_name,
                    register,
                    registrations,
                } => {
                    self.handle_lsp_dynamic_capabilities(
                        language,
                        server_name,
                        register,
                        registrations,
                    );
                }
                AsyncMessage::FileChanged { path } => {
                    self.handle_async_file_changed(path);
                }
                AsyncMessage::GitStatusChanged { status } => {
                    tracing::info!("Git status changed: {}", status);
                    // TODO: Handle git status changes
                }
                AsyncMessage::FileExplorerInitialized { window, view } => {
                    self.handle_file_explorer_initialized(window, view);
                }
                AsyncMessage::FileExplorerInitFailed { window } => {
                    if let Some(win) = self.windows.get_mut(&window) {
                        win.file_explorer_init_failed();
                    }
                }
                AsyncMessage::FileExplorerToggleNode(node_id) => {
                    self.handle_file_explorer_toggle_node(node_id);
                }
                AsyncMessage::FileExplorerRefreshNode(node_id) => {
                    self.handle_file_explorer_refresh_node(node_id);
                }
                AsyncMessage::FileExplorerExpandedToPath { window, view } => {
                    self.handle_file_explorer_expanded_to_path(window, view);
                }
                AsyncMessage::Plugin(plugin_msg) => {
                    self.handle_plugin_async_message(plugin_msg);
                }
                AsyncMessage::LspProgress {
                    language,
                    token,
                    value,
                } => {
                    self.handle_lsp_progress(language, token, value);
                }
                AsyncMessage::LspWindowMessage {
                    language,
                    message_type,
                    message,
                } => {
                    self.handle_lsp_window_message(language, message_type, message);
                }
                AsyncMessage::LspLogMessage {
                    language,
                    message_type,
                    message,
                } => {
                    self.handle_lsp_log_message(language, message_type, message);
                }
                AsyncMessage::LspStatusUpdate {
                    language,
                    server_name,
                    status,
                    message: _,
                } => {
                    self.handle_lsp_status_update(language, server_name, status);
                }
                AsyncMessage::FileOpenDirectoryLoaded(result) => {
                    self.handle_file_open_directory_loaded(result);
                }
                AsyncMessage::FileOpenShortcutsLoaded(shortcuts) => {
                    self.handle_file_open_shortcuts_loaded(shortcuts);
                }
                AsyncMessage::ClipboardPasteResult { request_id, text } => {
                    self.resolve_pending_paste(request_id, text);
                }
                AsyncMessage::TerminalOutput { terminal } => {
                    self.handle_terminal_output(terminal);
                }
                AsyncMessage::PathChanged { handle, path, kind } => {
                    self.handle_path_changed(handle, path, kind);
                }
                AsyncMessage::TerminalExited {
                    terminal,
                    exit_code,
                } => {
                    // If this is the interactive self-update terminal, move the
                    // status-bar indicator to its terminal state (success = exit 0).
                    if self.self_update_terminal == Some(terminal.terminal) {
                        self.finish_self_update(exit_code == Some(0));
                        self.self_update_terminal = None;
                    }
                    self.handle_terminal_exited(terminal, exit_code);
                }

                AsyncMessage::LspServerRequest {
                    language,
                    server_command,
                    method,
                    params,
                } => {
                    self.handle_lsp_server_request(language, server_command, method, params);
                }
                AsyncMessage::PluginLspResponse {
                    language: _,
                    request_id,
                    result,
                } => {
                    self.handle_plugin_lsp_response(request_id, result);
                }
                AsyncMessage::RemoteAttachReady(ready) => {
                    self.handle_remote_attach_ready(ready);
                }
                AsyncMessage::RemoteReconnected { connection_id } => {
                    self.handle_remote_reconnected(connection_id);
                }
                AsyncMessage::RemoteAttachFailed {
                    error,
                    request_id,
                    reconnect_window,
                } => {
                    self.handle_remote_attach_failed(error, request_id, reconnect_window);
                }
                AsyncMessage::PluginProcessOutput {
                    process_id,
                    stdout,
                    stderr,
                    exit_code,
                } => {
                    // Drop any host-process kill handle tied to this
                    // id. The spawn task has exited (that's what this
                    // event means) so the handle is stale; a late
                    // `KillHostProcess` from the plugin should be a
                    // silent no-op rather than a dangling send. For
                    // non-host-process spawns the key won't be in
                    // the map and the remove is a no-op.
                    self.host_process_handles.remove(&process_id);
                    self.handle_plugin_process_output(
                        fresh_core::api::JsCallbackId::from(process_id),
                        stdout,
                        stderr,
                        exit_code,
                    );
                }
                AsyncMessage::GrammarRegistryBuilt {
                    registry,
                    callback_ids,
                } => {
                    self.handle_grammar_registry_built(registry, callback_ids);
                }
                AsyncMessage::QuickOpenFilesLoaded {
                    cwd,
                    files,
                    complete,
                } => {
                    self.handle_quick_open_files_loaded(cwd, files, complete);
                }
                AsyncMessage::PluginsDirLoaded {
                    dir,
                    errors,
                    discovered_plugins,
                } => {
                    self.handle_plugins_dir_loaded(dir, errors, discovered_plugins);
                }
                AsyncMessage::PluginDeclarationsReady { declarations } => {
                    self.handle_plugin_declarations_ready(declarations);
                }
                AsyncMessage::PluginInitScriptLoaded(outcome) => {
                    self.handle_plugin_init_script_loaded(outcome);
                }
            }
        }

        // Update plugin state snapshot BEFORE processing commands
        // This ensures plugins have access to current editor state (cursor positions, etc.)
        #[cfg(feature = "plugins")]
        {
            let _s = tracing::info_span!("update_plugin_state_snapshot").entered();
            self.update_plugin_state_snapshot();
        }

        // Process TypeScript plugin commands
        #[cfg(not(feature = "plugins"))]
        let processed_any_commands = false;
        #[cfg(feature = "plugins")]
        let processed_any_commands = {
            let _s = tracing::info_span!("process_plugin_commands").entered();
            self.process_plugin_commands()
        };

        // Re-sync snapshot after commands — commands like SetViewMode change
        // state that plugins read via getBufferInfo().  Without this, a
        // subsequent lines_changed callback would see stale values.
        #[cfg(feature = "plugins")]
        if processed_any_commands {
            let _s = tracing::info_span!("update_plugin_state_snapshot_post").entered();
            self.update_plugin_state_snapshot();
        }

        // Process pending plugin action completions
        #[cfg(feature = "plugins")]
        {
            let _s = tracing::info_span!("process_pending_plugin_actions").entered();
            self.process_pending_plugin_actions();
        }

        // Process pending LSP server restarts (with exponential backoff)
        {
            let _s = tracing::info_span!("process_pending_lsp_restarts").entered();
            self.process_pending_lsp_restarts();
        }

        // Check and clear the plugin render request flag
        #[cfg(feature = "plugins")]
        let plugin_render = {
            let render = self.plugin_render_requested;
            self.plugin_render_requested = false;
            render
        };
        #[cfg(not(feature = "plugins"))]
        let plugin_render = false;

        // Poll periodic update checker for new results
        if let Some(ref mut checker) = self.update_checker {
            // Poll for results but don't act on them - just cache
            let _ = checker.poll_result();
        }

        // Poll for file changes (auto-revert) and file tree changes
        let file_changes = {
            let _s = tracing::info_span!("poll_file_changes").entered();
            self.poll_file_changes()
        };
        let tree_changes = {
            let _s = tracing::info_span!("poll_file_tree_changes").entered();
            self.poll_file_tree_changes()
        };

        // Trigger render if any async messages, plugin commands were processed, or plugin requested render
        needs_render || processed_any_commands || plugin_render || file_changes || tree_changes
    }

    /// Handle a server's `initialize` response: record capabilities and kick off
    /// the deferred per-language requests that were gated on them.
    fn handle_lsp_initialized(
        &mut self,
        language: String,
        server_name: String,
        capabilities: crate::services::lsp::manager::ServerCapabilitySummary,
    ) {
        tracing::info!(
            "LSP server '{}' initialized for language: {}",
            server_name,
            language
        );
        self.active_window_mut().status_message = Some(format!("LSP ({}) ready", language));

        // Store capabilities on the specific server handle
        let __active_id = self.active_window;
        if let Some(lsp) = self.windows.get_mut(&__active_id).map(|w| &mut w.lsp) {
            lsp.set_server_capabilities(&language, &server_name, capabilities);
        }

        // Send didOpen for all open buffers of this language
        self.resend_did_open_for_language(&language);
        self.request_semantic_tokens_for_language(&language);
        self.request_folding_ranges_for_language(&language);
        // Now that capabilities are known, kick off inlay hints
        // and pull-diagnostics for buffers that opened before the
        // `initialize` handshake completed. Both paths route
        // through `handle_for_feature_mut`, so servers that
        // didn't advertise the capability are skipped.
        self.request_inlay_hints_for_language(&language);
        self.pull_diagnostics_for_language(&language);
    }

    /// Handle an LSP server crash/spawn failure: surface it, fire the
    /// `lsp_server_error` hook, and open the stderr log in the background.
    fn handle_lsp_error(
        &mut self,
        language: String,
        error: String,
        stderr_log_path: Option<std::path::PathBuf>,
    ) {
        tracing::error!("LSP error for {}: {}", language, error);
        self.active_window_mut().status_message =
            Some(format!("LSP error ({}): {}", language, error));

        // Get server command from config for the hook
        let server_command = self
            .config
            .lsp
            .get(&language)
            .and_then(|configs| configs.as_slice().first())
            .map(|c| c.command.clone())
            .unwrap_or_else(|| "unknown".to_string());

        // Determine error type from error message
        let error_type = if error.contains("not found") || error.contains("NotFound") {
            "not_found"
        } else if error.contains("permission") || error.contains("PermissionDenied") {
            "spawn_failed"
        } else if error.contains("timeout") {
            "timeout"
        } else {
            "spawn_failed"
        }
        .to_string();

        // Fire the LspServerError hook for plugins
        self.plugin_manager.read().unwrap().run_hook(
            "lsp_server_error",
            crate::services::plugins::hooks::HookArgs::LspServerError {
                language: language.clone(),
                server_command,
                error_type,
                message: error.clone(),
            },
        );

        // Open stderr log as read-only buffer if it exists and has content
        // Opens in background (new tab) without stealing focus
        if let Some(log_path) = stderr_log_path {
            let has_content = log_path.metadata().map(|m| m.len() > 0).unwrap_or(false);
            if has_content {
                tracing::info!("Opening LSP stderr log in background: {:?}", log_path);
                match self.open_file_no_focus(&log_path) {
                    Ok(buffer_id) => {
                        self.active_window_mut()
                            .mark_buffer_read_only(buffer_id, true);
                        self.active_window_mut().status_message = Some(format!(
                            "LSP error ({}): {} - See stderr log",
                            language, error
                        ));
                    }
                    Err(e) => {
                        tracing::error!("Failed to open LSP stderr log: {}", e);
                    }
                }
            }
        }
    }

    /// Apply a server-initiated `workspace/applyEdit`.
    fn handle_lsp_apply_edit(&mut self, edit: lsp_types::WorkspaceEdit, label: Option<String>) {
        tracing::info!("Applying workspace edit from server (label: {:?})", label);
        match self.apply_workspace_edit(edit) {
            Ok(n) => {
                if let Some(label) = label {
                    self.set_status_message(
                        t!("lsp.code_action_applied", title = &label, count = n).to_string(),
                    );
                }
            }
            Err(e) => {
                tracing::error!("Failed to apply workspace edit: {}", e);
            }
        }
    }

    /// Execute a resolved code action, or report the `codeAction/resolve` error.
    fn handle_lsp_code_action_resolved(&mut self, action: Result<lsp_types::CodeAction, String>) {
        match action {
            Ok(resolved) => {
                self.execute_resolved_code_action(resolved);
            }
            Err(e) => {
                tracing::warn!("codeAction/resolve failed: {}", e);
                self.set_status_message(format!("Code action resolve failed: {e}"));
            }
        }
    }

    /// Route a plugin-runtime async message (process I/O, delays, LSP and
    /// generic plugin responses) to its handler/hook.
    fn handle_plugin_async_message(&mut self, plugin_msg: fresh_core::api::PluginAsyncMessage) {
        use fresh_core::api::{JsCallbackId, PluginAsyncMessage};
        match plugin_msg {
            PluginAsyncMessage::ProcessOutput {
                process_id,
                stdout,
                stderr,
                exit_code,
            } => {
                self.handle_plugin_process_output(
                    JsCallbackId::from(process_id),
                    stdout,
                    stderr,
                    exit_code,
                );
            }
            PluginAsyncMessage::DelayComplete { callback_id } => {
                self.plugin_manager
                    .read()
                    .unwrap()
                    .resolve_callback(JsCallbackId::from(callback_id), "null".to_string());
            }
            PluginAsyncMessage::ProcessStdout { process_id, data } => {
                self.plugin_manager.read().unwrap().run_hook(
                    "onProcessStdout",
                    crate::services::plugins::hooks::HookArgs::ProcessOutput { process_id, data },
                );
            }
            PluginAsyncMessage::ProcessStderr { process_id, data } => {
                self.plugin_manager.read().unwrap().run_hook(
                    "onProcessStderr",
                    crate::services::plugins::hooks::HookArgs::ProcessOutput { process_id, data },
                );
            }
            PluginAsyncMessage::ProcessExit {
                process_id,
                callback_id,
                exit_code,
            } => {
                self.background_process_handles.remove(&process_id);
                let result = fresh_core::api::BackgroundProcessResult {
                    process_id,
                    exit_code,
                };
                self.plugin_manager.read().unwrap().resolve_callback(
                    JsCallbackId::from(callback_id),
                    serde_json::to_string(&result).unwrap(),
                );
            }
            PluginAsyncMessage::LspResponse {
                language: _,
                request_id,
                result,
            } => {
                self.handle_plugin_lsp_response(request_id, result);
            }
            PluginAsyncMessage::PluginResponse(response) => {
                self.handle_plugin_response(response);
            }
        }
    }

    /// Handle new terminal output: follow the bottom when appropriate and fire
    /// the `terminal_output` hook, attributing it to the owning session.
    fn handle_terminal_output(&mut self, terminal: fresh_core::WindowTerminalId) {
        // The message carries its owning window: terminal ids
        // collide across windows, so we trust the tag rather
        // than scanning windows for a matching id (which would
        // attribute output to the wrong session).
        let terminal_id = terminal.terminal;
        let owner = terminal.window;
        // Terminal output received - check if we should auto-jump back to terminal mode
        tracing::trace!("Terminal output received for {}", terminal);

        // If the focused split is viewing this terminal in scrollback and
        // jump_to_end_on_output is enabled, snap it back to the live grid.
        //
        // ...but never yank the view away from a text selection: a drag that
        // just started on the live grid (`terminal_drag_pending`), an
        // in-progress selection drag, or a completed selection waiting to be
        // copied all pin the scrollback view. A chatty program would
        // otherwise destroy the selection the instant its next output
        // arrived — the exact case drag-to-select exists for. Output keeps
        // streaming underneath; the auto-jump resumes once the selection is
        // gone (Ctrl+Space, typing, or a click that collapses it).
        let selection_active = {
            let win = self.active_window();
            win.mouse_state.dragging_text_selection
                || win.mouse_state.terminal_drag_pending.is_some()
                || win
                    .buffers
                    .splits()
                    .and_then(|(mgr, view_states)| view_states.get(&mgr.active_split()))
                    .map(|vs| {
                        let c = vs.cursors.primary();
                        c.anchor.is_some_and(|a| a != c.position)
                    })
                    .unwrap_or(false)
        };
        if self.config.terminal.jump_to_end_on_output
            && !self.active_window().focused_terminal_live()
            && !selection_active
        {
            // Check if active buffer is this terminal
            if let Some(active_terminal_id) =
                self.active_window().get_terminal_id(self.active_buffer())
            {
                if active_terminal_id == terminal_id {
                    self.enter_terminal_mode();
                }
            }
        }

        // When the focused split's terminal is live, keep its grid pinned to the
        // bottom so it follows new output. (Unfocused live splits follow on their
        // own — their grid sits at display_offset 0.)
        if self.active_window().focused_terminal_live() {
            if let Some(handle) = self.active_window().terminal_manager.get(terminal_id) {
                if let Ok(mut state) = handle.state.lock() {
                    state.scroll_to_bottom();
                }
            }
        }

        // Notify plugins, attributing output to the owning
        // *session* even when it's a background one (terminals
        // live in their own window's manager, not the active
        // window's). Snapshot the cursor row's text from that
        // same window so prompt detection works off-focus too.
        // The grid lock is released before `run_hook` runs to
        // avoid holding it across plugin code.
        let last_line = self
            .windows
            .get(&owner)
            .and_then(|w| w.terminal_manager.get(terminal_id))
            .and_then(|handle| handle.state.lock().ok().map(|s| s.last_visible_line()))
            .unwrap_or_default();
        // The terminal's current tab title, so a plugin can name a workspace
        // after whatever it's running. The auto-numbered default
        // (`*Terminal N*`) carries no signal, so pass it through as empty and
        // let the plugin fall back to its own naming.
        let terminal_title = self
            .windows
            .get(&owner)
            .and_then(|w| w.terminal_tab_title(terminal_id))
            .filter(|t| !(t.starts_with("*Terminal ") && t.ends_with('*')))
            .unwrap_or_default();
        // The program's explicit activity marker (OSC 133 / OSC 9;4), if it
        // emits one — lets a plugin drive the working/idle dot off a real
        // signal instead of output timing.
        let osc_activity = self
            .windows
            .get(&owner)
            .and_then(|w| w.terminal_manager.get(terminal_id))
            .and_then(|handle| handle.state.lock().ok().and_then(|s| s.osc_activity()));
        self.plugin_manager.read().unwrap().run_hook(
            "terminal_output",
            crate::services::plugins::hooks::HookArgs::TerminalOutput {
                terminal_id: terminal_id.0 as u64,
                window_id: owner.0,
                last_line,
                terminal_title,
                osc_activity,
            },
        );
    }

    /// Forward a watched-path filesystem event to the `path_changed` hook.
    fn handle_path_changed(
        &mut self,
        handle: u64,
        path: std::path::PathBuf,
        kind: crate::services::async_bridge::PathChangeKind,
    ) {
        self.path_changes_for_test
            .push((handle, path.clone(), kind.as_str()));
        self.plugin_manager.read().unwrap().run_hook(
            "path_changed",
            crate::services::plugins::hooks::HookArgs::PathChanged {
                handle,
                path: path.to_string_lossy().into_owned(),
                kind: kind.as_str().to_owned(),
            },
        );
    }

    /// Tear down (or preserve, for a pending remote reconnect) a terminal whose
    /// process exited, then fire the `terminal_exit` hook.
    /// Per-frame detector for *silent* agent-channel reconnects.
    ///
    /// The SSH / Kubernetes agent channel re-establishes itself in the
    /// background by hot-swapping its transport (`spawn_reconnect_task`),
    /// without ever routing through the app-level `RemoteAttachMode::Reconnect`
    /// flow — and that flow is the only thing that respawns the embedded
    /// terminal PTYs. Those PTYs are a *separate* `ssh -t` / `kubectl exec`
    /// carrier from the agent channel, so they die when the link drops and,
    /// on the automatic recovery path, would otherwise stay dead even though
    /// the filesystem/LSP came back.
    ///
    /// Bring `window_id`'s remote session back to life after its carrier
    /// reconnected. The single convergence point for *every* reconnect path:
    ///
    ///   * the silent background transport hot-swap (`spawn_reconnect_task`),
    ///     which keeps the existing authority and notifies via
    ///     `AsyncMessage::RemoteReconnected`; and
    ///   * the app-level rebuild (`RemoteAttachMode::Reconnect`), which installs
    ///     a fresh authority first and then calls this.
    ///
    /// Either way the embedded terminal PTYs died with the old carrier (a
    /// separate `ssh -t` / `kubectl exec` from the agent channel), so we respawn
    /// them in place through the now-live authority, reusing each backing file
    /// so scrollback continues. `respawn_terminals_through_authority` skips
    /// still-live terminals, so this is idempotent under duplicate signals.
    pub(crate) fn reattach_window(&mut self, window_id: fresh_core::WindowId) {
        let Some(window) = self.windows.get_mut(&window_id) else {
            return;
        };
        window.remote_reconnect_error = None;
        let revived = window.respawn_terminals_through_authority();
        if revived > 0 {
            let label = window.label.clone();
            self.set_status_message(format!("Reconnected: {label}"));
            // Reactivate the focused buffer. A terminal that was focused (and in
            // terminal input mode) when the carrier dropped had `terminal_mode`
            // cleared by `handle_terminal_exited`; respawning the PTY doesn't
            // restore it, so without this the reborn terminal is stranded in
            // scrollback / Normal mode until the user clicks it. Re-deriving the
            // flags from the active buffer's remembered (still-`Live`) mode
            // brings it back live in place. Only the active window owns the
            // `terminal_mode` / `key_context` input state; a background window
            // re-syncs when the user next focuses it.
            if window_id == self.active_window {
                self.sync_terminal_mode_to_active_buffer();
            }
        }
    }

    /// Ensure each remote window has a background task forwarding its agent
    /// channel's reconnect notifications onto the bridge as
    /// `AsyncMessage::RemoteReconnected`. Idempotent and cheap: it only spawns
    /// for channels not already in `remote_reconnect_forwarders`. Called every
    /// frame so windows born/reconnected after startup are covered without
    /// hooking each authority-install site.
    ///
    /// The forwarder loops (`notified().await` → `send`) so it survives many
    /// reconnects. A window whose authority is later rebuilt gets a *new*
    /// channel id and a fresh forwarder; the old forwarder parks forever on a
    /// notify that can no longer fire (its channel is dropped) — a single idle
    /// task per rebuild, which is rare. Cleaning those up is a future refinement.
    fn ensure_remote_reconnect_forwarders(&mut self) {
        let (Some(runtime), Some(bridge)) =
            (self.tokio_runtime.as_ref(), self.async_bridge.as_ref())
        else {
            return;
        };
        // Collect first to avoid spawning while holding the `windows` borrow.
        let mut to_spawn: Vec<(u64, std::sync::Arc<tokio::sync::Notify>)> = Vec::new();
        for window in self.windows.values() {
            let fs = &window.authority().filesystem;
            if let (Some(id), Some(notify)) = (fs.remote_channel_id(), fs.remote_reconnect_notify())
            {
                if !self.remote_reconnect_forwarders.contains(&id) {
                    to_spawn.push((id, notify));
                }
            }
        }
        for (id, notify) in to_spawn {
            self.remote_reconnect_forwarders.insert(id);
            let sender = bridge.sender();
            runtime.spawn(async move {
                loop {
                    notify.notified().await;
                    if sender
                        .send(AsyncMessage::RemoteReconnected { connection_id: id })
                        .is_err()
                    {
                        break; // editor/bridge gone
                    }
                }
            });
        }
    }

    /// Detect remote-connection state changes (a link dropped or came back)
    /// across windows and report whether any changed since the last poll.
    ///
    /// The background reconnect task flips `is_remote_connected()` on its own
    /// timeline: a plain drop fires no async message at all, and the eventual
    /// settle after a flap may land between the `RemoteReconnected` events that
    /// do fire. Neither is tied to an input event, so without this poll the
    /// status-bar remote indicator goes stale — still reading "connected" after
    /// a drop, or "(Disconnected)" after the link came back — until the user
    /// happens to press a key. Called once per editor tick; when it returns
    /// `true` the caller re-renders. Cheap: a bool load per remote window, and
    /// no allocation at all when there are no remote windows.
    pub(crate) fn poll_remote_connection_changes(&mut self) -> bool {
        let current: std::collections::HashMap<fresh_core::WindowId, bool> = self
            .windows
            .iter()
            .filter_map(|(id, w)| {
                let fs = &w.authority().filesystem;
                // Only windows with a real remote authority matter; a local
                // filesystem's default `is_remote_connected() == true` is noise.
                fs.remote_connection_info()
                    .is_some()
                    .then(|| (*id, fs.is_remote_connected()))
            })
            .collect();
        if current != self.remote_connected_cache {
            self.remote_connected_cache = current;
            true
        } else {
            false
        }
    }

    /// Test-only seam: drive the reconnect dispatch directly, as if a
    /// `RemoteReconnected` event for `connection_id` had arrived on the bridge.
    /// Lets component tests exercise the id→window→reattach mapping without a
    /// live agent channel or tokio runtime.
    #[doc(hidden)]
    pub fn test_dispatch_remote_reconnected(&mut self, connection_id: u64) {
        self.handle_remote_reconnected(connection_id);
    }

    /// Map a reconnected agent channel (identified by its stable connection id)
    /// back to the window whose live authority owns it, and reattach. Driven by
    /// the background reconnect task via `AsyncMessage::RemoteReconnected`.
    fn handle_remote_reconnected(&mut self, connection_id: u64) {
        let Some(window_id) = self.windows.iter().find_map(|(id, w)| {
            (w.authority().filesystem.remote_channel_id() == Some(connection_id)).then_some(*id)
        }) else {
            // The window was closed, or its authority was swapped out from under
            // this connection — nothing to reattach.
            return;
        };
        tracing::info!("agent channel {connection_id} reconnected; reattaching window {window_id}");
        self.reattach_window(window_id);
    }

    fn handle_terminal_exited(
        &mut self,
        terminal: fresh_core::WindowTerminalId,
        exit_code: Option<i32>,
    ) {
        // The message is tagged with its owning window, so the
        // plugin hook is attributed correctly even for a
        // background session's terminal.
        let terminal_id = terminal.terminal;
        let exited_window_id = terminal.window;
        tracing::info!("Terminal {} exited", terminal);
        // A remote window whose carrier just dropped: its embedded PTY (a
        // separate `ssh -t` / `kubectl exec` from the agent channel) died with
        // the link, not because the user exited the shell. Keep the
        // buffer↔terminal binding (and the backing/command maps, which this
        // handler already leaves intact) so a reconnect can respawn it in
        // place (`respawn_terminals_through_authority`, driven on the automatic
        // path by `detect_remote_terminal_reconnects`). Removing it here would
        // strand the buffer as a dead read-only tab with no way back.
        //
        // The signal is the *live authority*: a remote filesystem that is
        // currently disconnected. Gating on `authority_spec` instead would
        // miss a plain `fresh ssh://…` launch, whose spec stays `Local`. A
        // normal exit (remote still connected, or any local terminal) falls
        // through to the usual permanent teardown.
        let preserve_for_reconnect = {
            let fs = &self.active_window().authority().filesystem;
            fs.remote_connection_info().is_some() && !fs.is_remote_connected()
        };
        // Find the buffer associated with this terminal
        if let Some((&buffer_id, _)) = self
            .active_window()
            .terminal_buffers
            .iter()
            .find(|(_, tb)| tb.terminal_id == terminal_id)
        {
            // A genuinely exited terminal has no PTY left to drive, so EVERY
            // split showing it becomes read-only scrollback (not just the
            // focused one) — otherwise an unfocused split would keep rendering a
            // "live" grid of a dead terminal. A terminal preserved for remote
            // reconnect keeps its per-split live state so it comes back live
            // when the carrier respawns it.
            if !preserve_for_reconnect {
                let dead_splits: Vec<crate::model::event::LeafId> = self
                    .active_window()
                    .buffers
                    .splits()
                    .map(|(_, vs_map)| {
                        vs_map
                            .iter()
                            .filter(|(_, svs)| svs.active_buffer == buffer_id)
                            .map(|(leaf, _)| *leaf)
                            .collect()
                    })
                    .unwrap_or_default();
                for leaf in dead_splits {
                    self.active_window_mut()
                        .set_split_terminal_scrollback(leaf, buffer_id, true);
                }
            }

            // If the focused split was driving this now-dead terminal, leave the
            // Terminal key context (its derived live state is already false).
            if self.active_buffer() == buffer_id
                && self.active_window().key_context
                    == crate::input::keybindings::KeyContext::Terminal
            {
                self.active_window_mut().key_context =
                    crate::input::keybindings::KeyContext::Normal;
            }

            // Sync terminal content to buffer (final screen state)
            self.active_window_mut().sync_terminal_to_buffer(buffer_id);

            // Append exit message to the backing file and reload
            let exit_msg = "\n[Terminal process exited]\n";

            if let Some(backing_path) = self
                .active_window()
                .terminal_backing_files
                .get(&terminal_id)
                .cloned()
            {
                if let Ok(mut file) =
                    crate::app::terminal::terminal_backing_fs().open_file_for_append(&backing_path)
                {
                    use std::io::Write;
                    if let Err(e) = file.write_all(exit_msg.as_bytes()) {
                        tracing::warn!("Failed to write terminal exit message: {}", e);
                    }
                }

                // Force reload buffer from file to pick up the exit message
                if let Err(e) = self.revert_buffer_by_id(buffer_id, &backing_path) {
                    tracing::warn!("Failed to revert terminal buffer: {}", e);
                }

                // After revert, scroll the viewport so the just-
                // appended exit message is visible. sync_terminal_to_buffer
                // pinned the viewport to the start of the visible screen
                // (so exit is pixel-identical to the last live frame); the
                // exit message is appended *after* that pinned region,
                // so we have to deliberately scroll past the pin to bring
                // it on-screen. Move the cursor to the new end-of-buffer
                // and clear the skip_ensure_visible flag the sync path
                // armed; the next render's ensure_visible will then scroll
                // the cursor (and the exit-message line above it) into
                // view.
                let new_total = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.get(&buffer_id))
                    .map(|s| s.buffer.total_bytes())
                    .unwrap_or(0);
                if let Some((mgr, view_states)) = self
                    .windows
                    .get_mut(&self.active_window)
                    .map(|w| &mut w.buffers)
                    .expect("active window present")
                    .splits_mut()
                {
                    let active_split = mgr.active_split();
                    if let Some(view_state) = view_states.get_mut(&active_split) {
                        view_state.cursors.primary_mut().position = new_total;
                        view_state.viewport.clear_skip_ensure_visible();
                    }
                }
            }

            // Ensure buffer remains read-only with no line numbers
            if let Some(state) = self
                .windows
                .get_mut(&self.active_window)
                .map(|w| &mut w.buffers)
                .expect("active window present")
                .get_mut(&buffer_id)
            {
                state.editing_disabled = true;
                state.margins.configure_for_line_numbers(false);
                state.buffer.set_modified(false);
            }

            // Remove from terminal_buffers so it's no longer treated
            // as a terminal — unless we're holding it for a remote
            // reconnect to respawn in place (see above).
            if !preserve_for_reconnect {
                self.active_window_mut().terminal_buffers.remove(&buffer_id);
            }

            self.set_status_message(t!("terminal.exited", id = terminal_id.0).to_string());
        }
        self.active_window_mut().terminal_manager.close(terminal_id);

        // Notify plugins after the editor's own exit handling
        // is complete. Orchestrator's state machine reads this
        // to transition agents to READY (code 0) or ERRORED.
        // `exit_code` is currently always `None` here; full
        // wait-status capture is a follow-up commit.
        self.plugin_manager.read().unwrap().run_hook(
            "terminal_exit",
            crate::services::plugins::hooks::HookArgs::TerminalExited {
                terminal_id: terminal_id.0 as u64,
                window_id: exited_window_id.0,
                exit_code,
            },
        );
    }

    /// Install a completed `attachRemoteAgent` connection per its mode
    /// (restart / new window / dormant-session reconnect).
    fn handle_remote_attach_ready(
        &mut self,
        ready: crate::services::async_bridge::RemoteAttachReady,
    ) {
        // The background connect succeeded. Install per `mode`:
        // Restart rebuilds the whole editor around the backend
        // (global), Window spawns a born-attached session beside
        // the existing ones.
        let crate::services::async_bridge::RemoteAttachReady {
            authority,
            keepalive,
            working_dir,
            mode,
            spec,
            request_id,
        } = ready;
        // If the plugin cancelled this connect while it was
        // in flight (the New-Session dialog's Cancel), the result
        // arrives too late to matter: drop the authority and its
        // keepalive here so the carrier is torn down and no window
        // is ever built. The reject was already delivered at cancel
        // time, so there's nothing left to resolve.
        if self.remote_attach_was_cancelled(request_id) {
            tracing::info!(
                "Remote attach for request {} arrived after cancellation; discarding",
                request_id
            );
            drop(keepalive);
            drop(authority);
            return;
        }
        // Re-root at the pod's workspace (or its home if the plugin
        // didn't supply one) — never the stale local path. The
        // filesystem call is safe here: `process_async_messages`
        // runs on the main loop, not inside a runtime.
        let root = working_dir
            .or_else(|| authority.filesystem.home_dir().ok())
            .unwrap_or_else(|| std::path::PathBuf::from("/"));
        match mode {
            crate::services::async_bridge::RemoteAttachMode::Restart => {
                tracing::info!(
                    "Remote attach connected ({}); installing authority (restart), rooting at {}",
                    authority.display_label,
                    root.display()
                );
                // Resolve before the restart tears the plugin
                // runtime down, so the awaiting caller observes
                // success rather than a vanished promise.
                self.resolve_remote_attach(request_id);
                // Record the reconnect spec on the (re-rooted)
                // active session before the restart so it persists
                // and the rebuilt editor restores this backend.
                self.active_window_mut().authority_spec = spec;
                self.install_authority_with_keepalive(authority, keepalive, root);
            }
            crate::services::async_bridge::RemoteAttachMode::Window { label, command } => {
                tracing::info!(
                    "Remote attach connected ({}); opening born-attached window at {}",
                    authority.display_label,
                    root.display()
                );
                // The session is only "ready" once the window
                // exists. Resolve on success; on a window-creation
                // failure reject so the plugin keeps its dialog
                // open with the reason and no half-built window.
                match self
                    .create_remote_session_window(authority, keepalive, root, label, command, spec)
                {
                    Ok(_) => self.resolve_remote_attach(request_id),
                    Err(e) => self.reject_remote_attach(request_id, e),
                }
            }
            crate::services::async_bridge::RemoteAttachMode::Reconnect { window_id } => {
                // The common case: a dormant remote session the user
                // dived into finished connecting. It has no `Window`
                // yet (it lived in `dormant_remote` as an
                // authority-less descriptor) — promote it now, born
                // with this connected authority, restoring its
                // workspace through it so its terminals run on the
                // remote backend, not the local host.
                if self.dormant_remote.contains_key(&window_id) {
                    tracing::info!(
                        "Promoting dormant remote session {window_id} ({})",
                        authority.display_label
                    );
                    self.promote_dormant_remote(window_id, authority, keepalive);
                } else if self.windows.contains_key(&window_id) {
                    tracing::info!(
                        "Reconnected dormant session {window_id} ({})",
                        authority.display_label
                    );
                    // This path *rebuilt* the authority, so install it first,
                    // then reattach: `reattach_window` clears the FailedAttach
                    // indicator and respawns the embedded terminal(s) that died
                    // with the old carrier, through the freshly-installed
                    // authority. The silent hot-swap path keeps the existing
                    // authority and reaches the same `reattach_window` via
                    // `AsyncMessage::RemoteReconnected`.
                    self.set_session_authority(window_id, authority);
                    self.session_keepalives.insert(window_id, keepalive);
                    self.reattach_window(window_id);
                } else {
                    // The window was closed while the connect was in
                    // flight — drop the backend we just built.
                    drop(authority);
                    drop(keepalive);
                }
            }
        }
    }

    /// Reject a failed `attachRemoteAgent` connect, recording the reason on the
    /// reconnecting window so its status-bar indicator shows `FailedAttach`.
    fn handle_remote_attach_failed(
        &mut self,
        error: String,
        request_id: u64,
        reconnect_window: Option<fresh_core::WindowId>,
    ) {
        // A cancelled connect was already rejected at cancel time;
        // swallow the late failure rather than rejecting twice.
        if self.remote_attach_was_cancelled(request_id) {
            tracing::info!(
                "Remote attach for request {} failed after cancellation; discarding",
                request_id
            );
            return;
        }
        tracing::warn!("Remote attach failed: {}", error);
        // A *dive-triggered* reconnect of a dormant workspace has no
        // awaiting JS callback for `reject_remote_attach` to reject
        // and no plugin dialog open, so its only user-visible signal
        // is the status-bar remote indicator. Record the reason on
        // the workspace's window so the indicator renders
        // `FailedAttach` (persistent, error-styled, with a Retry-only
        // popup) until the next reconnect attempt.
        // Born-attached / restart attaches carry `None` here; their
        // failure is surfaced by the launching plugin's rejected
        // promise (e.g. the New-Session dialog's inline error).
        if let Some(window_id) = reconnect_window {
            let reason = error.lines().next().unwrap_or(&error).to_string();
            if let Some(w) = self.windows.get_mut(&window_id) {
                // An already-live remote window whose reconnect failed — or a
                // dormant session's shell the dive committed into while this
                // connect ran: record the reason on the window. The status-bar
                // indicator renders a short "Disconnected" from this (the full
                // error went to the `tracing::warn!` above, which lights the
                // warning indicator); the reason itself is surfaced in the
                // remote-indicator popup. No activation here — the user may
                // have switched away while the connect was in flight, and a
                // background failure must not steal focus.
                w.remote_reconnect_error = Some(reason.clone());
                // For a dormant session's shell, ALSO surface the reason on
                // the status line, as the window-less path always did —
                // on the SHELL's own status line (messages are per-window;
                // the user may be looking at another workspace by now).
                if self.dormant_remote.contains_key(&window_id) {
                    w.set_status_message(format!("Connection failed: {reason}"));
                }
            } else if self.dormant_remote.contains_key(&window_id) {
                // A dive-triggered connect of a dormant session failed. The
                // user asked for that workspace, so commit the switch: build
                // an empty disconnected shell for it and activate it (dock
                // selection and active window agree again — issue #2570).
                // The session stays dormant behind the shell; diving again
                // (or the indicator's Retry) reconnects, and a success
                // replaces the shell with the fully-restored window. The
                // failure message is posted *after* the activation so the
                // switch machinery can't clear it off the status line.
                self.activate_failed_dormant_placeholder(window_id, reason.clone());
                self.set_status_message(format!("Connection failed: {reason}"));
            } else {
                // The session was closed while the connect was in flight —
                // nothing to attach the failure to; just surface it.
                self.set_status_message(format!("Connection failed: {reason}"));
            }
        }
        self.reject_remote_attach(request_id, error);
    }

    /// Swap in a freshly-built grammar registry, re-detect syntax for open
    /// buffers, and resolve any plugin callbacks that awaited the build.
    fn handle_grammar_registry_built(
        &mut self,
        registry: std::sync::Arc<crate::primitives::grammar::GrammarRegistry>,
        callback_ids: Vec<fresh_core::api::JsCallbackId>,
    ) {
        tracing::info!(
            "Background grammar build completed ({} syntaxes)",
            registry.available_syntaxes().len()
        );
        // Merge user `[languages]` config into the catalog so
        // find_by_path honours user globs/filenames/extensions.
        // The background thread just sent the Arc through the
        // channel, so we're the sole owner here. Assert rather
        // than silently drop config.
        let mut registry = registry;
        std::sync::Arc::get_mut(&mut registry)
            .expect("freshly-received grammar registry Arc must be uniquely owned")
            .apply_language_config(&self.config.languages);
        crate::config::reload_indent_overrides(&self.config.languages);
        self.grammar_registry = registry;
        // Propagate the new grammar registry to every window's
        // resources so window-side syntax detection picks up the
        // freshly-built grammars without waiting for a restart.
        for w in self.windows.values_mut() {
            w.resources.grammar_registry = self.grammar_registry.clone();
        }
        self.grammar_build_in_progress = false;

        // Re-detect syntax for all open buffers with the full registry
        let buffers_to_update: Vec<_> = self
            .active_window()
            .buffer_metadata
            .iter()
            .filter_map(|(id, meta)| meta.file_path().map(|p| (*id, p.to_path_buf())))
            .collect();

        for (buf_id, path) in buffers_to_update {
            if let Some(state) = self
                .windows
                .get_mut(&self.active_window)
                .map(|w| &mut w.buffers)
                .expect("active window present")
                .get_mut(&buf_id)
            {
                let first_line = state.buffer.first_line_lossy();
                let detected = crate::primitives::detected_language::DetectedLanguage::from_path(
                    &path,
                    first_line.as_deref(),
                    &self.grammar_registry,
                    &self.config.languages,
                );

                if detected.highlighter.has_highlighting() || !state.highlighter.has_highlighting()
                {
                    state.apply_language(detected);
                    state.apply_buffer_config(&self.config);
                }
            }
        }

        // Resolve plugin callbacks that were waiting for this build
        #[cfg(feature = "plugins")]
        for cb_id in callback_ids {
            self.plugin_manager
                .read()
                .unwrap()
                .resolve_callback(cb_id, "null".to_string());
        }

        // Flush any plugin grammars that arrived during the build
        self.flush_pending_grammars();
    }

    /// Update the Quick Open file cache from a background scan and refresh the
    /// open prompt's suggestions.
    fn handle_quick_open_files_loaded(
        &mut self,
        cwd: String,
        files: std::sync::Arc<Vec<crate::input::quick_open::providers::FileEntry>>,
        complete: bool,
    ) {
        // Update the file provider cache and refresh suggestions
        // if Quick Open is currently showing file mode (empty prefix).
        if let Some((provider, _)) = self.quick_open_registry.get_provider_for_input("") {
            if let Some(fp) = provider
                .as_any()
                .downcast_ref::<crate::input::quick_open::providers::FileProvider>()
            {
                if complete {
                    fp.set_cache(&cwd, files);
                } else {
                    fp.set_partial_cache(&cwd, files);
                }
            }
        }
        // Refresh the Quick Open suggestions if the prompt is open
        if let Some(prompt) = &self.active_window_mut().prompt {
            if prompt.prompt_type == PromptType::QuickOpen {
                let input = prompt.input.clone();
                self.update_quick_open_suggestions(&input);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;
    use crate::config_io::DirectoryContext;
    use std::sync::Arc;

    fn test_editor() -> Editor {
        let temp = tempfile::tempdir().unwrap();
        let dir_context = DirectoryContext::for_testing(temp.path());
        // Keep the temp dir alive for the editor's lifetime.
        std::mem::forget(temp);
        // Plugins disabled: an enabled plugin can set its own status on its
        // first tick (the bundled i18n test plugin does), which would clobber
        // the status this test asserts on. The handler under test is core, not
        // plugin-gated, so this isolates it cleanly.
        Editor::for_test(
            Config::default(),
            80,
            24,
            None,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            Arc::new(crate::model::filesystem::StdFileSystem),
            None,
            None,
            false,
            false,
        )
        .unwrap()
    }

    #[test]
    fn dive_reconnect_failure_records_error_on_its_window() {
        // A failed dive-triggered reconnect records the (first line of the)
        // error on its own window, which drives the status-bar remote indicator
        // into FailedAttach for that workspace.
        let mut editor = test_editor();
        let win = editor.active_window;

        let sender = editor.async_bridge.as_ref().unwrap().sender();
        sender
            .send(AsyncMessage::RemoteAttachFailed {
                error: "Agent failed to start: SSH could not connect\nsecond line".to_string(),
                request_id: u64::MAX - win.0,
                reconnect_window: Some(win),
            })
            .unwrap();
        editor.process_async_messages();

        let err = editor
            .windows
            .get(&win)
            .unwrap()
            .remote_reconnect_error
            .clone();
        assert_eq!(
            err.as_deref(),
            Some("Agent failed to start: SSH could not connect"),
            "only the first line of a multi-line error is recorded"
        );
    }

    #[test]
    fn non_reconnect_attach_failure_sets_no_window_error() {
        // Born-attached / restart attaches (reconnect_window = None) surface
        // their failure via the launching plugin's rejected promise, not the
        // per-window indicator — so no window error is recorded.
        let mut editor = test_editor();
        let win = editor.active_window;

        let sender = editor.async_bridge.as_ref().unwrap().sender();
        sender
            .send(AsyncMessage::RemoteAttachFailed {
                error: "boom".to_string(),
                request_id: 7,
                reconnect_window: None,
            })
            .unwrap();
        editor.process_async_messages();

        assert!(
            editor
                .windows
                .get(&win)
                .unwrap()
                .remote_reconnect_error
                .is_none(),
            "a non-reconnect failure must not set a window error"
        );
    }
}
