//! Async-message dispatch on `Editor`.
//!
//! `process_async_messages` runs each frame and drains the AsyncBridge,
//! routing each AsyncMessage to its handler — LSP responses,
//! initialization/errors, plugin commands, filesystem polling, etc.
//! ~650 lines of `match`-armed dispatch.

use rust_i18n::t;

use crate::services::async_bridge::AsyncMessage;
use crate::view::prompt::PromptType;

use super::Editor;

impl Editor {
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
                    tracing::info!(
                        "LSP server '{}' initialized for language: {}",
                        server_name,
                        language
                    );
                    self.active_window_mut().status_message =
                        Some(format!("LSP ({}) ready", language));

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
                AsyncMessage::LspError {
                    language,
                    error,
                    stderr_log_path,
                } => {
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
                    tracing::info!("Applying workspace edit from server (label: {:?})", label);
                    match self.apply_workspace_edit(edit) {
                        Ok(n) => {
                            if let Some(label) = label {
                                self.set_status_message(
                                    t!("lsp.code_action_applied", title = &label, count = n)
                                        .to_string(),
                                );
                            }
                        }
                        Err(e) => {
                            tracing::error!("Failed to apply workspace edit: {}", e);
                        }
                    }
                }
                AsyncMessage::LspCodeActionResolved {
                    request_id: _,
                    action,
                } => match action {
                    Ok(resolved) => {
                        self.execute_resolved_code_action(resolved);
                    }
                    Err(e) => {
                        tracing::warn!("codeAction/resolve failed: {}", e);
                        self.set_status_message(format!("Code action resolve failed: {e}"));
                    }
                },
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
                    result_id,
                    diagnostics,
                    unchanged,
                } => {
                    self.handle_lsp_pulled_diagnostics(uri, result_id, diagnostics, unchanged);
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
                            self.plugin_manager.read().unwrap().resolve_callback(
                                JsCallbackId::from(callback_id),
                                "null".to_string(),
                            );
                        }
                        PluginAsyncMessage::ProcessStdout { process_id, data } => {
                            self.plugin_manager.read().unwrap().run_hook(
                                "onProcessStdout",
                                crate::services::plugins::hooks::HookArgs::ProcessOutput {
                                    process_id,
                                    data,
                                },
                            );
                        }
                        PluginAsyncMessage::ProcessStderr { process_id, data } => {
                            self.plugin_manager.read().unwrap().run_hook(
                                "onProcessStderr",
                                crate::services::plugins::hooks::HookArgs::ProcessOutput {
                                    process_id,
                                    data,
                                },
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
                    // The message carries its owning window: terminal ids
                    // collide across windows, so we trust the tag rather
                    // than scanning windows for a matching id (which would
                    // attribute output to the wrong session).
                    let terminal_id = terminal.terminal;
                    let owner = terminal.window;
                    // Terminal output received - check if we should auto-jump back to terminal mode
                    tracing::trace!("Terminal output received for {}", terminal);

                    // If viewing scrollback for this terminal and jump_to_end_on_output is enabled,
                    // automatically re-enter terminal mode
                    if self.config.terminal.jump_to_end_on_output
                        && !self.active_window().terminal_mode
                    {
                        // Check if active buffer is this terminal
                        if let Some(&active_terminal_id) = self
                            .active_window()
                            .terminal_buffers
                            .get(&self.active_buffer())
                        {
                            if active_terminal_id == terminal_id {
                                self.enter_terminal_mode();
                            }
                        }
                    }

                    // When in terminal mode, ensure display stays at bottom (follows new output)
                    if self.active_window().terminal_mode {
                        if let Some(handle) = self.active_window().terminal_manager.get(terminal_id)
                        {
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
                    self.plugin_manager.read().unwrap().run_hook(
                        "terminal_output",
                        crate::services::plugins::hooks::HookArgs::TerminalOutput {
                            terminal_id: terminal_id.0 as u64,
                            window_id: owner.0,
                            last_line,
                        },
                    );
                }
                AsyncMessage::PathChanged { handle, path, kind } => {
                    self.last_path_change_for_test = Some((handle, path.clone(), kind.as_str()));
                    self.plugin_manager.read().unwrap().run_hook(
                        "path_changed",
                        crate::services::plugins::hooks::HookArgs::PathChanged {
                            handle,
                            path: path.to_string_lossy().into_owned(),
                            kind: kind.as_str().to_owned(),
                        },
                    );
                }
                AsyncMessage::TerminalExited {
                    terminal,
                    exit_code,
                } => {
                    // The message is tagged with its owning window, so the
                    // plugin hook is attributed correctly even for a
                    // background session's terminal.
                    let terminal_id = terminal.terminal;
                    let exited_window_id = terminal.window;
                    tracing::info!("Terminal {} exited", terminal);
                    // Find the buffer associated with this terminal
                    if let Some((&buffer_id, _)) = self
                        .active_window()
                        .terminal_buffers
                        .iter()
                        .find(|(_, &tid)| tid == terminal_id)
                    {
                        // Exit terminal mode if this is the active buffer
                        if self.active_buffer() == buffer_id && self.active_window().terminal_mode {
                            self.active_window_mut().terminal_mode = false;
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
                            if let Ok(mut file) = self
                                .authority
                                .filesystem
                                .open_file_for_append(&backing_path)
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

                        // Remove from terminal_buffers so it's no longer treated as a terminal
                        self.active_window_mut().terminal_buffers.remove(&buffer_id);

                        self.set_status_message(
                            t!("terminal.exited", id = terminal_id.0).to_string(),
                        );
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
                    // The background connect succeeded. Install per `mode`:
                    // Restart rebuilds the whole editor around the backend
                    // (global), Window spawns a born-attached session beside
                    // the existing ones.
                    let crate::services::async_bridge::RemoteAttachReady {
                        authority,
                        keepalive,
                        working_dir,
                        mode,
                        request_id,
                    } = ready;
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
                            self.install_authority_with_keepalive(authority, keepalive, root);
                        }
                        crate::services::async_bridge::RemoteAttachMode::Window {
                            label,
                            command,
                        } => {
                            tracing::info!(
                                "Remote attach connected ({}); opening born-attached window at {}",
                                authority.display_label,
                                root.display()
                            );
                            // The session is only "ready" once the window
                            // exists. Resolve on success; on a window-creation
                            // failure reject so the plugin keeps its dialog
                            // open with the reason and no half-built window.
                            match self.create_remote_session_window(
                                authority, keepalive, root, label, command,
                            ) {
                                Ok(_) => self.resolve_remote_attach(request_id),
                                Err(e) => self.reject_remote_attach(request_id, e),
                            }
                        }
                    }
                }
                AsyncMessage::RemoteAttachFailed { error, request_id } => {
                    tracing::warn!("Remote attach failed: {}", error);
                    self.reject_remote_attach(request_id, error);
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
                            let detected =
                                crate::primitives::detected_language::DetectedLanguage::from_path(
                                    &path,
                                    first_line.as_deref(),
                                    &self.grammar_registry,
                                    &self.config.languages,
                                );

                            if detected.highlighter.has_highlighting()
                                || !state.highlighter.has_highlighting()
                            {
                                state.apply_language(detected);
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
                AsyncMessage::QuickOpenFilesLoaded {
                    cwd,
                    files,
                    complete,
                } => {
                    // Update the file provider cache and refresh suggestions
                    // if Quick Open is currently showing file mode (empty prefix).
                    if let Some((provider, _)) = self.quick_open_registry.get_provider_for_input("")
                    {
                        if let Some(fp) = provider
                            .as_any()
                            .downcast_ref::<crate::input::quick_open::providers::FileProvider>(
                        ) {
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
}
