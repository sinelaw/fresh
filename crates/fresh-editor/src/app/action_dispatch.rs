//! Action execution: carrying out a resolved [`Action`] against the editor.
//!
//! Once the input orchestration (`app/input.rs`) — or a menu, a mouse
//! click, a plugin, a macro — has resolved an [`Action`], this is where
//! the editor performs it. Executing an action is inherently high-level
//! `Editor` behaviour (it drives buffers, windows, prompts, plugins), so
//! unlike the decision layers in `crate::input::router` these are methods;
//! the split from `app/input.rs` keeps *deciding what a key means*
//! separate from *doing what the action says*.

use super::*;
use anyhow::Result as AnyhowResult;
use crossterm::event::KeyModifiers as KM;
use rust_i18n::t;

impl Editor {
    /// Change the current workspace's trust level, persist it, and report it.
    /// The new policy applies live at the next authority-routed spawn (the
    /// guarding spawners read the level on every spawn) — there is NO editor
    /// restart here, deliberately: a rebuild would reset every other
    /// orchestrator session's buffers/layout (see the body). Trust-gated work
    /// re-triggers via the `trust_changed` hook instead (e.g. env-manager
    /// re-activates a now-trusted env). Already-correct selections (e.g.
    /// confirming the current level) only persist the decision.
    pub(crate) fn set_workspace_trust_level(
        &mut self,
        level: crate::services::workspace_trust::TrustLevel,
    ) {
        use crate::services::workspace_trust::TrustLevel;
        // Trust is a per-window gate: each `Window` owns its own authority +
        // `WorkspaceTrust` (issue #2280), and the guarding spawners read the
        // level live at spawn time. Writing the new level here is the whole
        // change — `set_level` itself documents "no rebuild required". The
        // next authority-routed spawn (LSP, terminal command, task, formatter,
        // plugin `spawnProcess`) honours the new level automatically.
        //
        // We deliberately do NOT `request_restart` here: that tears down and
        // rebuilds the *entire* editor — every orchestrator session window,
        // not just this one — which discarded other sessions' buffers and
        // reset the layout when toggling a single session's trust (the
        // trust-level-modal reset bug).
        let trust = &self.authority().workspace_trust;
        trust.set_level(level);
        let msg = match level {
            TrustLevel::Trusted => t!("trust.now_trusted"),
            TrustLevel::Restricted => t!("trust.now_restricted"),
            TrustLevel::Blocked => t!("trust.now_blocked"),
        }
        .to_string();
        self.active_window_mut().status_message = Some(msg);

        // Refresh the plugin-visible state snapshot so `editor.workspaceTrustLevel()`
        // reflects the new level, then notify plugins. The `trust_changed` hook
        // lets trust-gated work re-trigger inline — env-manager re-activates a
        // now-trusted env without a window switch — and it is the single signal
        // every trust-change path (modal confirm, status pill, plugin action)
        // funnels through, since they all route here. Deliberately a hook and a
        // snapshot refresh, NOT a `request_restart`: a rebuild would reset every
        // other session's buffers/layout (see the note above).
        #[cfg(feature = "plugins")]
        {
            self.update_plugin_state_snapshot();
            self.plugin_manager.read().unwrap().run_hook(
                "trust_changed",
                crate::services::plugins::hooks::HookArgs::TrustChanged {
                    level: level.as_str().to_string(),
                },
            );
        }
    }

    /// Handle an action (for normal mode and command execution).
    /// Used by the app module internally and by the GUI module for native menu dispatch.
    pub(crate) fn handle_action(&mut self, action: Action) -> AnyhowResult<()> {
        use crate::input::keybindings::Action;

        // Actions are the funnel for command-driven UI mutation (palette,
        // menus, plugin commands, macros) — any of them can change what the
        // overlay stack / chrome tree would derive, so spoil the
        // per-generation UI memos.
        self.bump_ui_gen();

        // Record action to macro if recording
        self.record_macro_action(&action);

        // Reset dabbrev cycling session on any non-dabbrev action.
        if !matches!(action, Action::DabbrevExpand) {
            self.reset_dabbrev_state();
        }

        // Enter on a line that points somewhere (`editor.setLineTargets`)
        // follows it, the same as clicking it. Intercepted here rather than
        // inside the newline handler so the behaviour is identical whether
        // the buffer is editable or not — an index built by a script is
        // usually a plain file, and typing a newline into it is never what
        // pressing Enter on an entry meant.
        #[cfg(feature = "plugins")]
        if matches!(action, Action::InsertNewline) {
            let buffer_id = self.active_buffer();
            if let Some(line) = self.cursor_line_in_active_buffer() {
                if let Some(target) = self.line_target_at(buffer_id, line) {
                    let source = self.active_split_id();
                    self.follow_line_target(target, source);
                    return Ok(());
                }
            }
        }

        match action {
            Action::Quit => self.quit(),
            Action::ForceQuit => {
                self.should_quit = true;
            }
            Action::Detach => {
                self.should_detach = true;
            }
            Action::WorkspaceTrustTrust => {
                self.set_workspace_trust_level(
                    crate::services::workspace_trust::TrustLevel::Trusted,
                );
            }
            Action::WorkspaceTrustRestrict => {
                self.set_workspace_trust_level(
                    crate::services::workspace_trust::TrustLevel::Restricted,
                );
            }
            Action::WorkspaceTrustBlock => {
                self.set_workspace_trust_level(
                    crate::services::workspace_trust::TrustLevel::Blocked,
                );
            }
            Action::WorkspaceTrustPrompt => {
                // Voluntarily-opened: cancellable (Esc / Cancel just closes).
                self.show_workspace_trust_popup(true);
            }
            Action::Save => {
                // Check if buffer has a file path - if not, redirect to SaveAs
                if self.active_state().buffer.file_path().is_none() {
                    self.start_prompt_with_initial_text(
                        t!("file.save_as_prompt").to_string(),
                        PromptType::SaveFileAs,
                        String::new(),
                    );
                    self.init_file_open_state();
                } else if self.check_save_conflict().is_some() {
                    // Check if file was modified externally since we opened/saved it
                    self.start_prompt(
                        t!("file.file_changed_prompt").to_string(),
                        PromptType::ConfirmSaveConflict,
                    );
                } else if let Err(e) = self.save() {
                    let msg = format!("{}", e);
                    self.active_window_mut().status_message =
                        Some(t!("file.save_failed", error = &msg).to_string());
                }
            }
            Action::SaveAs => {
                // Get current filename as default suggestion
                let current_path = self
                    .active_state()
                    .buffer
                    .file_path()
                    .map(|p| {
                        // Make path relative to working_dir if possible
                        p.strip_prefix(self.working_dir())
                            .unwrap_or(p)
                            .to_string_lossy()
                            .to_string()
                    })
                    .unwrap_or_default();
                self.start_prompt_with_initial_text(
                    t!("file.save_as_prompt").to_string(),
                    PromptType::SaveFileAs,
                    current_path,
                );
                self.init_file_open_state();
            }
            Action::SaveAll => {
                let msg = match self.save_all() {
                    Ok((saved, failed)) => {
                        if failed > 0 {
                            t!(
                                "status.save_all_partial",
                                saved = saved.to_string(),
                                failed = failed.to_string()
                            )
                            .to_string()
                        } else if saved == 0 {
                            t!("status.save_all_none").to_string()
                        } else {
                            t!("status.save_all", count = saved.to_string()).to_string()
                        }
                    }
                    Err(e) => t!("file.save_failed", error = &format!("{}", e)).to_string(),
                };
                self.active_window_mut().status_message = Some(msg);
            }
            Action::Open => {
                self.start_prompt(t!("file.open_prompt").to_string(), PromptType::OpenFile);
                self.prefill_open_file_prompt();
                self.init_file_open_state();
            }
            Action::SwitchProject => {
                self.start_prompt(
                    t!("file.switch_project_prompt").to_string(),
                    PromptType::SwitchProject,
                );
                self.init_folder_open_state();
            }
            Action::GotoLine => {
                let has_line_index = self.active_buffer_has_line_index();
                if has_line_index {
                    self.start_prompt(
                        t!("file.goto_line_prompt").to_string(),
                        PromptType::GotoLine,
                    );
                } else {
                    self.start_prompt(
                        t!("goto.scan_confirm_prompt", yes = "y", no = "N").to_string(),
                        PromptType::GotoLineScanConfirm,
                    );
                }
            }
            Action::ScanLineIndex => {
                self.start_incremental_line_scan(false);
            }
            Action::New => {
                self.new_buffer();
            }
            Action::Close | Action::CloseTab => {
                // Both Close and CloseTab use close_tab() which handles:
                // - Closing the split if this is the last buffer and there are other splits
                // - Prompting for unsaved changes
                // - Properly closing the buffer
                self.close_tab();
            }
            Action::Revert => {
                // Check if buffer has unsaved changes - prompt for confirmation
                if self.active_state().buffer.is_modified() {
                    let revert_key = t!("prompt.key.revert").to_string();
                    let cancel_key = t!("prompt.key.cancel").to_string();
                    self.start_prompt(
                        t!(
                            "prompt.revert_confirm",
                            revert_key = revert_key,
                            cancel_key = cancel_key
                        )
                        .to_string(),
                        PromptType::ConfirmRevert,
                    );
                } else {
                    // No local changes, just revert
                    if let Err(e) = self.revert_file() {
                        self.set_status_message(
                            t!("error.failed_to_revert", error = e.to_string()).to_string(),
                        );
                    }
                }
            }
            Action::ToggleAutoRevert => {
                self.toggle_auto_revert();
            }
            Action::OpenUpdateLog => {
                self.show_self_update_output();
            }
            Action::UpdateFresh => {
                // Once an update is running or finished, the indicator's job is
                // to surface the update terminal, not to re-offer the update —
                // except at the two terminal states that leave something for
                // the user to act on.
                use crate::services::release_checker::SelfUpdatePhase;
                match self.self_update_phase() {
                    // Failed: retry / show-log / cancel.
                    SelfUpdatePhase::Failed => self.show_update_failed_popup(),
                    // Action required: the update ran cleanly but left a command
                    // to run. Surface *that*, rather than re-offering an update
                    // that has already been downloaded and verified.
                    SelfUpdatePhase::ActionRequired => self.show_update_action_required_popup(),
                    SelfUpdatePhase::Running | SelfUpdatePhase::Succeeded => {
                        self.show_self_update_output()
                    }
                    SelfUpdatePhase::Idle => {
                        if !self.config().self_update {
                            self.set_status_message(t!("update.disabled").to_string());
                        } else if !self.is_update_available() {
                            self.set_status_message(t!("update.up_to_date").to_string());
                        } else {
                            // An update is available — offer it through a popup
                            // built from the resolved update plan, so the
                            // confirmation says how this copy was installed and
                            // what confirming will actually do.
                            let version = self.latest_version().unwrap_or("").to_string();
                            self.show_update_popup(&version);
                        }
                    }
                }
            }
            Action::FormatBuffer => {
                if self.refuse_if_editing_disabled() {
                    return Ok(());
                }
                if let Err(e) = self.format_buffer() {
                    self.set_status_message(
                        t!("error.format_failed", error = e.to_string()).to_string(),
                    );
                }
            }
            Action::TrimTrailingWhitespace => {
                if self.refuse_if_editing_disabled() {
                    return Ok(());
                }
                match self.trim_trailing_whitespace() {
                    Ok(true) => {
                        self.set_status_message(t!("whitespace.trimmed").to_string());
                    }
                    Ok(false) => {
                        self.set_status_message(t!("whitespace.no_trailing").to_string());
                    }
                    Err(e) => {
                        self.set_status_message(
                            t!("error.trim_whitespace_failed", error = e).to_string(),
                        );
                    }
                }
            }
            Action::EnsureFinalNewline => {
                if self.refuse_if_editing_disabled() {
                    return Ok(());
                }
                match self.ensure_final_newline() {
                    Ok(true) => {
                        self.set_status_message(t!("whitespace.newline_added").to_string());
                    }
                    Ok(false) => {
                        self.set_status_message(t!("whitespace.already_has_newline").to_string());
                    }
                    Err(e) => {
                        self.set_status_message(
                            t!("error.ensure_newline_failed", error = e).to_string(),
                        );
                    }
                }
            }
            Action::Copy => {
                // Editor-level popups take precedence over everything, including the file explorer.
                let popup = self
                    .global_popups
                    .top()
                    .or_else(|| self.active_state().popups.top());
                if let Some(popup) = popup {
                    if popup.has_selection() {
                        if let Some(text) = popup.get_selected_text() {
                            self.clipboard.copy(text);
                            self.set_status_message(t!("clipboard.copied").to_string());
                            return Ok(());
                        }
                    }
                }
                if self.active_window_mut().key_context
                    == crate::input::keybindings::KeyContext::FileExplorer
                {
                    self.active_window_mut().file_explorer_copy();
                    return Ok(());
                }
                // A focused widget Text input on the active buffer
                // wins over the underlying buffer's copy path. The
                // widget's selection lives in its TextEdit; this
                // bypasses `is_editing_disabled` because widget
                // inputs are independent of the underlying virtual
                // buffer's read-only-ness.
                let buffer_id = self.active_buffer();
                if let Some(panel_id) = self.focused_text_widget_panel_for_buffer(buffer_id) {
                    if self.handle_widget_copy(&panel_id) {
                        self.set_status_message(t!("clipboard.copied").to_string());
                        return Ok(());
                    }
                }
                // Check if active buffer is a composite buffer
                if self.active_window().is_composite_buffer(buffer_id) {
                    if let Some(_handled) = self.handle_composite_action(buffer_id, &Action::Copy) {
                        return Ok(());
                    }
                }
                // Copying the selection completes a drag-to-select gesture on
                // a live terminal grid: the split was only parked in implicit
                // (drag-initiated) scrollback so the selection could exist,
                // and the copy is the gesture's natural end — resume the live
                // grid. Explicit scrollback visits (Ctrl+Space, Shift+PageUp,
                // wheel) are never resumed by a copy: the user chose a stable
                // reading view and yanking it to the bottom would lose their
                // place.
                let resume_terminal = {
                    let win = self.active_window();
                    let split = win.effective_active_split();
                    win.split_terminal_drag_scrollback(split, buffer_id)
                        && win
                            .buffers
                            .splits()
                            .and_then(|(_, vs)| vs.get(&split))
                            .is_some_and(|vs| {
                                vs.cursors.iter().any(|(_, c)| {
                                    c.selection_range().is_some() || c.has_block_selection()
                                })
                            })
                };
                self.copy_selection();
                if resume_terminal {
                    self.enter_terminal_mode();
                    self.set_status_message("Copied - terminal resumed".to_string());
                }
            }
            Action::CopyWithTheme(theme) => self.copy_selection_with_theme(&theme),
            Action::CopyFilePath => self.copy_active_buffer_path(false),
            Action::CopyRelativeFilePath => self.copy_active_buffer_path(true),
            Action::Cut => {
                if self.active_window_mut().key_context
                    == crate::input::keybindings::KeyContext::FileExplorer
                {
                    self.active_window_mut().file_explorer_cut();
                    return Ok(());
                }
                // Focused widget Text wins over the buffer cut path,
                // and bypasses `is_editing_disabled` — widget inputs
                // are independent of the underlying virtual buffer.
                let buffer_id = self.active_buffer();
                if let Some(panel_id) = self.focused_text_widget_panel_for_buffer(buffer_id) {
                    if self.handle_widget_cut(&panel_id) {
                        return Ok(());
                    }
                }
                if self.active_window().is_editing_disabled() {
                    self.set_status_message(t!("buffer.editing_disabled").to_string());
                    return Ok(());
                }
                self.cut_selection()
            }
            Action::Paste => {
                if self.active_window_mut().key_context
                    == crate::input::keybindings::KeyContext::FileExplorer
                {
                    self.file_explorer_paste();
                    return Ok(());
                }
                // Focused widget Text wins over the buffer paste
                // path, and bypasses `is_editing_disabled`. Line
                // endings get normalised to LF before insertion
                // (multi-line `TextEdit` stores plain `\n`;
                // single-line strips them).
                let buffer_id = self.active_buffer();
                if let Some(panel_id) = self.focused_text_widget_panel_for_buffer(buffer_id) {
                    if let Some(text) = self.clipboard.paste() {
                        let normalized = text.replace("\r\n", "\n").replace('\r', "\n");
                        self.handle_widget_insert_str(&panel_id, &normalized);
                        self.set_status_message(t!("clipboard.pasted").to_string());
                    }
                    return Ok(());
                }
                if self.active_window().is_editing_disabled() {
                    self.set_status_message(t!("buffer.editing_disabled").to_string());
                    return Ok(());
                }
                self.paste()
            }
            Action::SelectAll => {
                // Focused widget Text wins over the buffer's
                // select-all. SelectAll on the buffer is then
                // handled by the default `apply_action_as_events`
                // catch-all path below.
                let buffer_id = self.active_buffer();
                if let Some(panel_id) = self.focused_text_widget_panel_for_buffer(buffer_id) {
                    self.handle_widget_select_all(&panel_id);
                    return Ok(());
                }
                self.apply_action_as_events(Action::SelectAll)?;
            }
            Action::YankWordForward => self.yank_word_forward(),
            Action::YankWordBackward => self.yank_word_backward(),
            Action::YankToLineEnd => self.yank_to_line_end(),
            Action::YankToLineStart => self.yank_to_line_start(),
            Action::YankViWordEnd => self.yank_vi_word_end(),
            Action::Undo => {
                self.handle_undo();
            }
            Action::Redo => {
                self.handle_redo();
            }
            Action::ShowHelp => {
                self.ensure_help_panel_mode_registered();
                self.active_window_mut().open_help_manual();
            }
            Action::ShowKeyboardShortcuts => {
                self.ensure_help_panel_mode_registered();
                self.active_window_mut().open_keyboard_shortcuts();
            }
            Action::ShowWarnings => {
                self.show_warnings_popup();
            }
            Action::ShowStatusLog => {
                self.open_status_log();
            }
            Action::ShowLspStatus => {
                self.show_lsp_status_popup();
            }
            Action::ShowRemoteIndicatorMenu => {
                self.show_remote_indicator_popup();
            }
            Action::ShowReadOnlyMenu => {
                self.show_read_only_popup();
            }
            Action::ClearWarnings => {
                self.active_window_mut().clear_warnings();
            }
            Action::CommandPalette => {
                // CommandPalette now delegates to QuickOpen (which starts with ">" prefix
                // for command mode). Toggle if already open.
                if self.close_quick_open_if_open() {
                    return Ok(());
                }
                self.start_quick_open();
            }
            Action::QuickOpen => {
                if self.close_quick_open_if_open() {
                    return Ok(());
                }
                self.start_quick_open();
            }
            Action::QuickOpenBuffers => {
                if self.close_quick_open_if_open() {
                    return Ok(());
                }
                self.start_quick_open_with_prefix("#");
            }
            Action::QuickOpenFiles => {
                if self.close_quick_open_if_open() {
                    return Ok(());
                }
                self.start_quick_open_with_prefix("");
            }
            Action::OpenLiveGrep => {
                self.handle_action(Action::PluginAction("start_live_grep".to_string()))?;
            }
            Action::ResumeLiveGrep => {
                self.handle_action(Action::PluginAction("resume_live_grep".to_string()))?;
            }
            Action::ToggleUtilityDock => {
                use crate::view::split::SplitRole;
                if let Some(dock_leaf) = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .find_leaf_by_role(SplitRole::UtilityDock)
                {
                    let active = self
                        .windows
                        .get(&self.active_window)
                        .and_then(|w| w.buffers.splits())
                        .map(|(mgr, _)| mgr)
                        .expect("active window must have a populated split layout")
                        .active_split();
                    if active == dock_leaf {
                        // Already focused — no editor-leaf history yet,
                        // so just cycle to the next leaf via the
                        // existing Alt+] command. Phase 7 will track a
                        // proper "previous editor split" pointer.
                        self.next_split();
                    } else {
                        self.windows
                            .get_mut(&self.active_window)
                            .and_then(|w| w.split_manager_mut())
                            .expect("active window must have a populated split layout")
                            .set_active_split(dock_leaf);
                    }
                } else {
                    self.set_status_message(
                        "No Utility Dock open — invoke a dock-aware utility (Diagnostics, Search/Replace, …)"
                            .to_string(),
                    );
                }
            }
            Action::CycleLiveGrepProvider => {
                // Only meaningful while the Live Grep overlay is open. Detect via prompt state —
                // both `PromptType::LiveGrep` (Resume's pre-seeded overlay) and
                // `Plugin{custom_type:"live-grep"}` (the live-running plugin's prompt) qualify.
                let in_live_grep = self
                    .active_window()
                    .prompt
                    .as_ref()
                    .map(|p| match &p.prompt_type {
                        PromptType::LiveGrep => true,
                        PromptType::Plugin { custom_type } => custom_type == "live-grep",
                        _ => false,
                    })
                    .unwrap_or(false);
                if !in_live_grep {
                    self.set_status_message(
                        "Cycle Live Grep provider only works inside Live Grep".to_string(),
                    );
                    return Ok(());
                }
                self.handle_action(Action::PluginAction("live_grep_cycle_provider".to_string()))?;
            }
            Action::OpenTerminalInDock => {
                self.handle_open_terminal_in_dock()?;
            }
            Action::ToggleLineWrap => {
                let new_value = !self.config.editor.line_wrap;
                self.config_mut().editor.line_wrap = new_value;
                // `resolve_line_wrap_for_buffer` below reads
                // `Window::config()`, which holds a *separate* `Arc<Config>`
                // clone from the Editor's. Without this sync the resolve
                // would return the pre-toggle value and we'd write the
                // *old* line-wrap state back into the viewport — silently
                // no-op'ing the toggle while still flipping the status
                // message. See `Editor::config_mut` for the broader rule.
                self.sync_windows_config();

                // Update all viewports to reflect the new line wrap setting,
                // respecting per-language overrides
                let active_split = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();
                let leaf_ids: Vec<_> = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .keys()
                    .copied()
                    .collect();
                for leaf_id in leaf_ids {
                    let buffer_id = self
                        .split_manager_mut()
                        .get_buffer_id(leaf_id.into())
                        .unwrap_or(BufferId(0));
                    let effective_wrap =
                        self.active_window().resolve_line_wrap_for_buffer(buffer_id);
                    let wrap_column = self
                        .active_window()
                        .resolve_wrap_column_for_buffer(buffer_id);
                    if let Some(view_state) = self
                        .windows
                        .get_mut(&self.active_window)
                        .and_then(|w| w.split_view_states_mut())
                        .expect("active window must have a populated split layout")
                        .get_mut(&leaf_id)
                    {
                        // The active split's own pin is dropped — the user is
                        // expressing a global intent on the view in front of
                        // them. Every other pinned split keeps its choice: a
                        // global default must not silently un-pin work the
                        // user did elsewhere (same rule as the highlight
                        // toggles below).
                        if leaf_id == active_split {
                            view_state.line_wrap_override = None;
                        }
                        if view_state.line_wrap_override.is_none() {
                            view_state.viewport.line_wrap_enabled = effective_wrap;
                            view_state.viewport.wrap_indent = self.config.editor.wrap_indent;
                            view_state.viewport.wrap_column = wrap_column;
                        }
                    }
                }

                // `editor.line_wrap` is an editor-wide default, so an
                // unsuffixed toggle saves it to the user config layer — see the
                // scope convention on `COMMANDS` in `input/commands.rs`. The
                // per-buffer variant is "Toggle Line Wrap (Current Buffer)".
                self.persist_config_change(crate::config_keys::EDITOR_LINE_WRAP, new_value);

                let state = if self.config.editor.line_wrap {
                    t!("view.state_enabled").to_string()
                } else {
                    t!("view.state_disabled").to_string()
                };
                self.set_status_message(t!("view.line_wrap_state", state = state).to_string());
            }
            Action::ToggleCurrentLineHighlight => {
                let new_value = !self.config.editor.highlight_current_line;
                self.config_mut().editor.highlight_current_line = new_value;
                let active_split = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();

                // Update all splits
                let leaf_ids: Vec<_> = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .keys()
                    .copied()
                    .collect();
                for leaf_id in leaf_ids {
                    if let Some(view_state) = self
                        .windows
                        .get_mut(&self.active_window)
                        .and_then(|w| w.split_view_states_mut())
                        .expect("active window must have a populated split layout")
                        .get_mut(&leaf_id)
                    {
                        // The active split's own pin is dropped just below —
                        // the user is expressing a global intent on the view in
                        // front of them. Every other pinned buffer keeps its
                        // choice; a global default must not silently un-pin
                        // work the user did elsewhere.
                        if leaf_id == active_split {
                            view_state.highlight_current_line_override = None;
                        }
                        if view_state.highlight_current_line_override.is_none() {
                            view_state.highlight_current_line =
                                self.config.editor.highlight_current_line;
                        }
                    }
                }

                self.persist_config_change(
                    crate::config_keys::EDITOR_HIGHLIGHT_CURRENT_LINE,
                    new_value,
                );

                let state = if self.config.editor.highlight_current_line {
                    t!("view.state_enabled").to_string()
                } else {
                    t!("view.state_disabled").to_string()
                };
                self.set_status_message(
                    t!("view.current_line_highlight_state", state = state).to_string(),
                );
            }
            Action::ToggleOccurrenceHighlight => {
                let new_value = !self.config.editor.highlight_occurrences;
                self.config_mut().editor.highlight_occurrences = new_value;
                let active_buffer = self.active_buffer();

                // Update all open buffers. A buffer the user pinned with the
                // "(Current Buffer)" variant keeps its choice — except the
                // active one, whose pin is cleared first as a global intent.
                if let Some(state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .map(|w| &mut w.buffers)
                    .expect("active window present")
                    .get_mut(&active_buffer)
                {
                    state.buffer_settings.highlight_occurrences_override = None;
                }
                for window in self.windows.values_mut() {
                    for (_, state) in &mut window.buffers {
                        if state
                            .buffer_settings
                            .highlight_occurrences_override
                            .is_some()
                        {
                            continue;
                        }
                        state.reference_highlight_overlay.enabled = new_value;
                        if !new_value {
                            state
                                .reference_highlight_overlay
                                .clear(&mut state.overlays, &mut state.marker_list);
                        }
                    }
                }

                self.persist_config_change(
                    crate::config_keys::EDITOR_HIGHLIGHT_OCCURRENCES,
                    new_value,
                );

                let state = if new_value {
                    t!("view.state_enabled").to_string()
                } else {
                    t!("view.state_disabled").to_string()
                };
                self.set_status_message(
                    t!("view.occurrence_highlight_state", state = state).to_string(),
                );
            }
            Action::ToggleReadOnly => {
                let buffer_id = self.active_buffer();
                let is_now_read_only = self
                    .active_window()
                    .buffer_metadata
                    .get(&buffer_id)
                    .map(|m| !m.read_only)
                    .unwrap_or(false);
                self.active_window_mut()
                    .mark_buffer_read_only(buffer_id, is_now_read_only);

                let state_str = if is_now_read_only {
                    t!("view.state_enabled").to_string()
                } else {
                    t!("view.state_disabled").to_string()
                };
                self.set_status_message(t!("view.read_only_state", state = state_str).to_string());
            }
            Action::TogglePageView => {
                self.active_window_mut().handle_toggle_page_view();
            }
            Action::SetPageWidth => {
                let active_split = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();
                let current = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .get(&active_split)
                    .and_then(|v| v.compose_width.map(|w| w.to_string()))
                    .unwrap_or_default();
                self.start_prompt_with_initial_text(
                    "Page width (empty = viewport): ".to_string(),
                    PromptType::SetPageWidth,
                    current,
                );
            }
            Action::SetBackground => {
                let default_path = self
                    .ansi_background_path
                    .as_ref()
                    .and_then(|p| {
                        p.strip_prefix(self.working_dir())
                            .ok()
                            .map(|rel| rel.to_string_lossy().to_string())
                    })
                    .unwrap_or_else(|| DEFAULT_BACKGROUND_FILE.to_string());

                self.start_prompt_with_initial_text(
                    "Background file: ".to_string(),
                    PromptType::SetBackgroundFile,
                    default_path,
                );
            }
            Action::SetBackgroundBlend => {
                let default_amount = format!("{:.2}", self.background_fade);
                self.start_prompt_with_initial_text(
                    "Background blend (0-1): ".to_string(),
                    PromptType::SetBackgroundBlend,
                    default_amount,
                );
            }
            Action::LspCompletion => {
                self.request_completion();
            }
            Action::DabbrevExpand => {
                if self.refuse_if_editing_disabled() {
                    return Ok(());
                }
                self.dabbrev_expand();
            }
            Action::LspGotoDefinition => {
                self.request_goto_definition()?;
            }
            Action::LspRename => {
                self.start_rename()?;
            }
            Action::LspHover => {
                self.request_hover()?;
            }
            Action::LspReferences => {
                self.request_references()?;
            }
            Action::LspImplementation => {
                self.request_implementation()?;
            }
            Action::LspSignatureHelp => {
                self.request_signature_help();
            }
            Action::LspCodeActions => {
                self.request_code_actions()?;
            }
            Action::LspRestart => {
                self.handle_lsp_restart();
            }
            Action::LspStop => {
                self.handle_lsp_stop();
            }
            Action::LspToggleForBuffer => {
                self.handle_lsp_toggle_for_buffer();
            }
            Action::ToggleInlayHints => {
                self.toggle_inlay_hints();
            }
            Action::DumpConfig => {
                self.dump_config();
            }
            Action::RedrawScreen => {
                self.request_full_redraw();
            }
            Action::SelectTheme => {
                self.start_select_theme_prompt();
            }
            Action::InspectThemeAtCursor => {
                self.inspect_theme_at_cursor();
            }
            Action::SelectKeybindingMap => {
                self.start_select_keybinding_map_prompt();
            }
            Action::SelectCursorStyle => {
                self.start_select_cursor_style_prompt();
            }
            Action::SelectLocale => {
                self.start_select_locale_prompt();
            }
            Action::Search => {
                // If already in a search-related prompt, Ctrl+F acts like Enter (confirm search)
                let is_search_prompt = self.active_window().prompt.as_ref().is_some_and(|p| {
                    matches!(
                        p.prompt_type,
                        PromptType::Search
                            | PromptType::ReplaceSearch
                            | PromptType::QueryReplaceSearch
                    )
                });

                if is_search_prompt {
                    self.confirm_prompt();
                } else {
                    self.start_search_prompt(
                        t!("file.search_prompt").to_string(),
                        PromptType::Search,
                        false,
                    );
                }
            }
            Action::Replace => {
                if self.refuse_if_editing_disabled() {
                    return Ok(());
                }
                // Use same flow as query-replace, just with confirm_each defaulting to false
                self.start_search_prompt(
                    t!("file.replace_prompt").to_string(),
                    PromptType::ReplaceSearch,
                    false,
                );
            }
            Action::QueryReplace => {
                if self.refuse_if_editing_disabled() {
                    return Ok(());
                }
                // Enable confirm mode by default for query-replace
                self.active_window_mut().search_confirm_each = true;
                self.start_search_prompt(
                    "Query replace: ".to_string(),
                    PromptType::QueryReplaceSearch,
                    false,
                );
            }
            Action::FindInSelection => {
                self.start_search_prompt(
                    t!("file.search_prompt").to_string(),
                    PromptType::Search,
                    true,
                );
            }
            Action::FindNext => {
                self.find_next();
            }
            Action::FindPrevious => {
                self.find_previous();
            }
            Action::FindSelectionNext => {
                self.find_selection_next();
            }
            Action::FindSelectionPrevious => {
                self.find_selection_previous();
            }
            Action::ClearSearch => {
                self.active_window_mut().clear_search_highlights();
            }
            Action::AddCursorNextMatch => self.add_cursor_at_next_match(),
            Action::AddCursorAbove => self.add_cursor_above(),
            Action::AddCursorBelow => self.add_cursor_below(),
            Action::AddCursorsToLineEnds => self.add_cursors_to_line_ends(),
            Action::NextBuffer => self.next_buffer(),
            Action::PrevBuffer => self.prev_buffer(),
            Action::SwitchToPreviousTab => self.switch_to_previous_tab(),
            Action::SwitchToTabByName => self.start_switch_to_tab_prompt(),

            // Tab scrolling (manual scroll - don't auto-adjust)
            Action::ScrollTabsLeft => {
                let active_split_id = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();
                if let Some(view_state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .get_mut(&active_split_id)
                {
                    view_state.tab_scroll_offset = view_state.tab_scroll_offset.saturating_sub(5);
                    self.set_status_message(t!("status.scrolled_tabs_left").to_string());
                }
            }
            Action::ScrollTabsRight => {
                let active_split_id = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();
                if let Some(view_state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .get_mut(&active_split_id)
                {
                    view_state.tab_scroll_offset = view_state.tab_scroll_offset.saturating_add(5);
                    self.set_status_message(t!("status.scrolled_tabs_right").to_string());
                }
            }
            Action::NavigateBack => self.navigate_back(),
            Action::NavigateForward => self.navigate_forward(),
            Action::SplitHorizontal => self.split_pane_horizontal(),
            Action::SplitVertical => self.split_pane_vertical(),
            Action::CloseSplit => self.close_active_split(),
            Action::NextSplit => self.next_split(),
            Action::PrevSplit => self.prev_split(),
            Action::NextPane => self.next_pane(),
            Action::PrevPane => self.prev_pane(),
            Action::NextWindow => self.next_window(),
            Action::PrevWindow => self.prev_window(),
            Action::ExtractTabToNewWorkspace => {
                let buffer_id = self.active_buffer();
                self.extract_tab_to_new_workspace(buffer_id);
            }
            Action::IncreaseSplitSize => self.adjust_split_size(0.05),
            Action::DecreaseSplitSize => self.adjust_split_size(-0.05),
            Action::ToggleMaximizeSplit => self.toggle_maximize_split(),
            Action::ToggleFileExplorer => self.toggle_file_explorer(),
            Action::ToggleFileExplorerSide => self.toggle_file_explorer_side(),
            Action::ToggleMenuBar => self.toggle_menu_bar(),
            Action::ToggleTabBar => self.toggle_tab_bar(),
            Action::ToggleStatusBar => self.toggle_status_bar(),
            Action::TogglePromptLine => self.toggle_prompt_line(),
            Action::ToggleVerticalScrollbar => self.toggle_vertical_scrollbar(),
            Action::ToggleHorizontalScrollbar => self.toggle_horizontal_scrollbar(),
            Action::ToggleLineNumbers => self.toggle_line_numbers(),
            Action::ToggleLineNumbersCurrentBuffer => self.toggle_line_numbers_current_buffer(),
            Action::ToggleLineWrapCurrentBuffer => self.toggle_line_wrap_current_buffer(),
            Action::ToggleVirtualSpaceCurrentBuffer => self.toggle_virtual_space_current_buffer(),
            Action::ToggleIndentationGuideCurrentBuffer => {
                self.toggle_indentation_guide_current_buffer()
            }
            Action::ToggleFoldIndicatorsCurrentBuffer => {
                self.toggle_fold_indicators_current_buffer()
            }
            Action::ToggleCurrentLineHighlightCurrentBuffer => {
                self.toggle_current_line_highlight_current_buffer()
            }
            Action::ToggleOccurrenceHighlightCurrentBuffer => {
                self.toggle_occurrence_highlight_current_buffer()
            }
            Action::TriggerWaveAnimation => self.trigger_wave_animation(),
            Action::ToggleScrollSync => self.active_window_mut().toggle_scroll_sync(),
            Action::ToggleMouseCapture => self.toggle_mouse_capture(),
            Action::ToggleMouseHover => self.toggle_mouse_hover(),
            Action::ToggleDebugHighlights => self.active_window_mut().toggle_debug_highlights(),
            // Rulers
            Action::AddRuler => {
                self.start_prompt(t!("rulers.add_prompt").to_string(), PromptType::AddRuler);
            }
            Action::RemoveRuler => {
                self.start_remove_ruler_prompt();
            }
            // Buffer settings
            Action::SetTabSize => {
                let current = self
                    .buffers()
                    .get(&self.active_buffer())
                    .map(|s| s.buffer_settings.tab_size.to_string())
                    .unwrap_or_else(|| "4".to_string());
                self.start_prompt_with_initial_text(
                    "Tab size: ".to_string(),
                    PromptType::SetTabSize,
                    current,
                );
            }
            Action::SetLineEnding => {
                self.start_set_line_ending_prompt();
            }
            Action::SetEncoding => {
                self.start_set_encoding_prompt();
            }
            Action::ReloadWithEncoding => {
                self.start_reload_with_encoding_prompt();
            }
            Action::SetLanguage => {
                self.start_set_language_prompt();
            }
            Action::ToggleIndentationStyle => {
                let __buffer_id = self.active_buffer();
                if let Some(state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .map(|w| &mut w.buffers)
                    .expect("active window present")
                    .get_mut(&__buffer_id)
                {
                    let new_value = !state.buffer_settings.use_tabs;
                    state.buffer_settings.use_tabs = new_value;
                    // Record the explicit override so a later `apply_config`
                    // (config reload, Set Language, save-time detection) can't
                    // re-stamp the language default over the user's choice —
                    // and so it can be persisted per file.
                    state.buffer_settings.use_tabs_override = Some(new_value);
                    let status = if state.buffer_settings.use_tabs {
                        "Indentation: Tabs"
                    } else {
                        "Indentation: Spaces"
                    };
                    self.set_status_message(status.to_string());
                }
            }
            Action::ToggleWhitespaceIndicators => {
                let __buffer_id = self.active_buffer();
                // Resolve the buffer's configured visibility up front: turning
                // the master toggle back on shows every space indicator plus
                // the tab and line-ending indicators the user actually
                // configured, rather than the hard-coded default. Fixes #2579.
                let configured = self.configured_whitespace_visibility(__buffer_id);
                if let Some(state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .map(|w| &mut w.buffers)
                    .expect("active window present")
                    .get_mut(&__buffer_id)
                {
                    state.buffer_settings.whitespace.toggle_all(configured);
                    let visible = state.buffer_settings.whitespace.any_visible();
                    state.buffer_settings.whitespace_override = Some(visible);
                    // The master toggle answers "any indicators at all?", so
                    // it subsumes a finer tab pin: "hide whitespace
                    // indicators" hiding everything *except* pinned arrows
                    // would make the master appear broken.
                    state.buffer_settings.tab_indicators_override = None;
                    let status = if visible {
                        t!("toggle.whitespace_indicators_shown")
                    } else {
                        t!("toggle.whitespace_indicators_hidden")
                    };
                    self.set_status_message(status.to_string());
                }
            }
            Action::ToggleTabIndicators => {
                let __buffer_id = self.active_buffer();
                if let Some(state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .map(|w| &mut w.buffers)
                    .expect("active window present")
                    .get_mut(&__buffer_id)
                {
                    // Only the tab-arrow trio: the command's description has
                    // always promised "tab arrow indicators (→)", but it used
                    // to share the master toggle-all with Whitespace
                    // Indicators and flipped the space dots too.
                    let ws = &mut state.buffer_settings.whitespace;
                    let new_value = !(ws.tabs_leading || ws.tabs_inner || ws.tabs_trailing);
                    ws.tabs_leading = new_value;
                    ws.tabs_inner = new_value;
                    ws.tabs_trailing = new_value;
                    state.buffer_settings.tab_indicators_override = Some(new_value);
                    let status = if new_value {
                        t!("toggle.tab_indicators_shown")
                    } else {
                        t!("toggle.tab_indicators_hidden")
                    };
                    self.set_status_message(status.to_string());
                }
            }
            Action::ResetBufferSettings => self.reset_buffer_settings(),
            Action::FocusFileExplorer => self.focus_file_explorer(),
            Action::FocusEditor => self.active_window_mut().focus_editor(),
            Action::ToggleDockFocus => {
                // Bounce keyboard focus between the editor/explorer area and
                // the orchestrator dock. `dock` is `Some` whenever the dock is
                // mounted (focused or merely visible-but-blurred); the helpers
                // flip `focused` and fire the matching `focus`/`blur`
                // widget_event so the plugin's mirror stays in sync.
                match self.dock.as_ref().map(|d| d.focused) {
                    Some(true) => self.blur_floating_panel(super::PanelSlot::Dock),
                    Some(false) => self.refocus_floating_panel(super::PanelSlot::Dock),
                    // Dock hidden: hand off to the orchestrator plugin's
                    // show-dock command so one key both opens and focuses it.
                    None => {
                        return self.handle_action(Action::PluginAction(
                            "orchestrator_dock_toggle".to_string(),
                        ));
                    }
                }
            }
            Action::FileExplorerUp => self.file_explorer_navigate_up(),
            Action::FileExplorerDown => self.file_explorer_navigate_down(),
            Action::FileExplorerPageUp => self.file_explorer_page_up(),
            Action::FileExplorerPageDown => self.file_explorer_page_down(),
            Action::FileExplorerExpand => self.file_explorer_toggle_expand(),
            Action::FileExplorerCollapse => self.file_explorer_collapse(),
            Action::FileExplorerOpen => self.file_explorer_open_file()?,
            Action::FileExplorerRefresh => self.file_explorer_refresh(),
            Action::FileExplorerNewFile => self.file_explorer_new_file(),
            Action::FileExplorerNewDirectory => self.file_explorer_new_directory(),
            Action::FileExplorerDelete => self.file_explorer_delete(),
            Action::FileExplorerRename => self.file_explorer_rename(),
            Action::FileExplorerToggleHidden => self.file_explorer_toggle_hidden(),
            Action::FileExplorerToggleGitignored => self.file_explorer_toggle_gitignored(),
            Action::FileExplorerSearchClear => {
                self.active_window_mut().file_explorer_search_clear()
            }
            Action::FileExplorerSearchBackspace => {
                self.active_window_mut().file_explorer_search_pop_char()
            }
            Action::FileExplorerCopy => self.active_window_mut().file_explorer_copy(),
            Action::FileExplorerCut => self.active_window_mut().file_explorer_cut(),
            Action::FileExplorerPaste => self.file_explorer_paste(),
            Action::FileExplorerDuplicate => self.file_explorer_duplicate(),
            Action::FileExplorerCopyFullPath => self.file_explorer_copy_path(false),
            Action::FileExplorerCopyRelativePath => self.file_explorer_copy_path(true),
            Action::FileExplorerExtendSelectionUp => {
                self.active_window_mut().file_explorer_extend_selection_up()
            }
            Action::FileExplorerExtendSelectionDown => self
                .active_window_mut()
                .file_explorer_extend_selection_down(),
            Action::FileExplorerToggleSelect => {
                self.active_window_mut().file_explorer_toggle_select()
            }
            Action::FileExplorerSelectAll => self.active_window_mut().file_explorer_select_all(),
            Action::RemoveSecondaryCursors => {
                // Convert action to events and apply them
                if let Some(events) = self
                    .active_window_mut()
                    .action_to_events(Action::RemoveSecondaryCursors)
                {
                    // Wrap in batch for atomic undo
                    let batch = Event::Batch {
                        events: events.clone(),
                        description: "Remove secondary cursors".to_string(),
                    };
                    self.active_event_log_mut().append(batch.clone());
                    self.apply_event_to_active_buffer(&batch);

                    // Ensure the primary cursor is visible after removing secondary cursors
                    let active_split = self
                        .windows
                        .get(&self.active_window)
                        .and_then(|w| w.buffers.splits())
                        .map(|(mgr, _)| mgr)
                        .expect("active window must have a populated split layout")
                        .active_split();
                    let active_buffer = self.active_buffer();
                    self.active_window_mut()
                        .ensure_cursor_visible_for_split(active_buffer, active_split);
                }
            }

            // Menu navigation actions
            Action::MenuActivate => {
                self.handle_menu_activate();
            }
            Action::MenuClose => {
                self.handle_menu_close();
            }
            Action::MenuLeft => {
                self.handle_menu_left();
            }
            Action::MenuRight => {
                self.handle_menu_right();
            }
            Action::MenuUp => {
                self.handle_menu_up();
            }
            Action::MenuDown => {
                self.handle_menu_down();
            }
            Action::MenuExecute => {
                if let Some(action) = self.handle_menu_execute() {
                    return self.handle_action(action);
                }
            }
            Action::MenuOpen(menu_name) => {
                // No mnemonics check here: with `menu_bar_mnemonics` off the
                // resolver suppresses the Alt+letter mnemonic bindings, so a
                // MenuOpen that reaches dispatch came from a live binding
                // (or a menu-bar click) and must open the menu. Gating here
                // instead used to turn Alt+F into a dead key: the mnemonic
                // still won resolution, then dispatch dropped it (#2941).
                self.handle_menu_open(&menu_name);
            }

            Action::SwitchKeybindingMap(map_name) => {
                // Delegate to the shared helper so the menu path persists the
                // choice to the user config (issue #474), matching the
                // command-palette path. This handler previously duplicated the
                // switch logic but skipped persistence, so the keybinding style
                // reset to the default on the next launch.
                self.apply_keybinding_map(&map_name);
            }

            Action::SmartHome => {
                // In composite (diff) views, use LineStart movement
                let buffer_id = self.active_buffer();
                if self.active_window().is_composite_buffer(buffer_id) {
                    if let Some(_handled) =
                        self.handle_composite_action(buffer_id, &Action::SmartHome)
                    {
                        return Ok(());
                    }
                }
                self.smart_home();
            }
            Action::ToggleComment => {
                self.toggle_comment();
            }
            Action::ToggleFold => {
                self.active_window_mut().toggle_fold_at_cursor();
            }
            Action::GoToMatchingBracket => {
                self.goto_matching_bracket();
            }
            Action::JumpToNextError => {
                self.jump_to_next_error();
            }
            Action::JumpToPreviousError => {
                self.jump_to_previous_error();
            }
            Action::SetBookmark(key) => {
                self.active_window_mut().set_bookmark(key);
            }
            Action::JumpToBookmark(key) => {
                self.jump_to_bookmark(key);
            }
            Action::ClearBookmark(key) => {
                self.active_window_mut().clear_bookmark(key);
            }
            Action::ListBookmarks => {
                self.active_window_mut().list_bookmarks();
            }
            Action::ToggleSearchCaseSensitive if !self.active_prompt_has_search_options() => {}
            Action::ToggleSearchWholeWord if !self.active_prompt_has_search_options() => {}
            Action::ToggleSearchRegex if !self.active_prompt_has_search_options() => {}
            Action::ToggleSearchCaseSensitive => {
                self.active_window_mut().search_case_sensitive =
                    !self.active_window().search_case_sensitive;
                let state = if self.active_window().search_case_sensitive {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(
                    t!("search.case_sensitive_state", state = state).to_string(),
                );
                self.refresh_active_search();
            }
            Action::ToggleSearchWholeWord => {
                self.active_window_mut().search_whole_word =
                    !self.active_window().search_whole_word;
                let state = if self.active_window().search_whole_word {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(t!("search.whole_word_state", state = state).to_string());
                self.refresh_active_search();
            }
            Action::ToggleSearchRegex => {
                self.active_window_mut().search_use_regex = !self.active_window().search_use_regex;
                let state = if self.active_window().search_use_regex {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(t!("search.regex_state", state = state).to_string());
                self.refresh_active_search();
            }
            Action::ToggleSearchConfirmEach => {
                self.active_window_mut().search_confirm_each =
                    !self.active_window().search_confirm_each;
                let state = if self.active_window().search_confirm_each {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(t!("search.confirm_each_state", state = state).to_string());
            }
            Action::FileBrowserToggleHidden => {
                // Toggle hidden files in file browser (handled via file_open_toggle_hidden)
                self.file_open_toggle_hidden();
            }
            Action::StartMacroRecording => {
                // This is a no-op; use ToggleMacroRecording instead
                self.set_status_message(
                    "Use Ctrl+Shift+R to start recording (will prompt for register)".to_string(),
                );
            }
            Action::StopMacroRecording => {
                self.stop_macro_recording();
            }
            Action::PlayMacro(key) => {
                self.play_macro(key);
            }
            Action::ToggleMacroRecording(key) => {
                self.toggle_macro_recording(key);
            }
            Action::ShowMacro(key) => {
                self.show_macro_in_buffer(key);
            }
            Action::ListMacros => {
                self.list_macros_in_buffer();
            }
            Action::PromptRecordMacro => {
                self.start_prompt("Record macro (0-9): ".to_string(), PromptType::RecordMacro);
            }
            Action::PromptPlayMacro => {
                self.start_prompt("Play macro (0-9): ".to_string(), PromptType::PlayMacro);
            }
            Action::PlayLastMacro => {
                if let Some(key) = self.active_window_mut().macros.last_register() {
                    self.play_macro(key);
                } else {
                    self.set_status_message(t!("status.no_macro_recorded").to_string());
                }
            }
            Action::PromptSaveMacroToInit => {
                self.start_prompt(
                    "Save macro to init.ts (0-9): ".to_string(),
                    PromptType::SaveMacroToInit,
                );
            }
            Action::PromptPromoteMacro => {
                self.start_prompt(
                    "Promote macro to command (0-9): ".to_string(),
                    PromptType::PromoteMacro,
                );
            }
            Action::PromptSetBookmark => {
                self.start_prompt("Set bookmark (0-9): ".to_string(), PromptType::SetBookmark);
            }
            Action::PromptJumpToBookmark => {
                self.start_prompt(
                    "Jump to bookmark (0-9): ".to_string(),
                    PromptType::JumpToBookmark,
                );
            }
            Action::CompositeNextHunk => {
                let buf = self.active_buffer();
                self.active_window_mut().composite_next_hunk_active(buf);
            }
            Action::CompositePrevHunk => {
                let buf = self.active_buffer();
                self.active_window_mut().composite_prev_hunk_active(buf);
            }
            Action::None => {}
            Action::DeleteBackward => {
                if self.active_window().is_editing_disabled() {
                    self.set_status_message(t!("buffer.editing_disabled").to_string());
                    return Ok(());
                }
                // Normal backspace handling
                if let Some(events) = self
                    .active_window_mut()
                    .action_to_events(Action::DeleteBackward)
                {
                    if events.len() > 1 {
                        // Multi-cursor: use optimized bulk edit (O(n) instead of O(n²))
                        let description = "Delete backward".to_string();
                        if let Some(bulk_edit) = self.apply_events_as_bulk_edit(events, description)
                        {
                            self.active_event_log_mut().append(bulk_edit);
                        }
                    } else {
                        for event in events {
                            self.active_event_log_mut().append(event.clone());
                            self.apply_event_to_active_buffer(&event);
                        }
                    }
                }
            }
            Action::PluginAction(action_name) => {
                tracing::debug!("handle_action: PluginAction('{}')", action_name);
                // Execute the plugin callback via TypeScript plugin thread
                // Use non-blocking version to avoid deadlock with async plugin ops
                #[cfg(feature = "plugins")]
                {
                    let result = self.plugin_manager.read().unwrap().execute_action_async(
                        &action_name,
                        None,
                        None,
                    );
                    if let Some(result) = result {
                        match result {
                            Ok(receiver) => {
                                // Store pending action for processing in main loop
                                self.pending_plugin_actions
                                    .push((action_name.clone(), receiver));
                            }
                            Err(e) => {
                                self.set_status_message(
                                    t!("view.plugin_error", error = e.to_string()).to_string(),
                                );
                                tracing::error!("Plugin action error: {}", e);
                            }
                        }
                    } else {
                        self.set_status_message(
                            t!("status.plugin_manager_unavailable").to_string(),
                        );
                    }
                }
                #[cfg(not(feature = "plugins"))]
                {
                    let _ = action_name;
                    self.set_status_message(
                        "Plugins not available (compiled without plugin support)".to_string(),
                    );
                }
            }
            Action::LoadPluginFromBuffer => {
                #[cfg(feature = "plugins")]
                {
                    let buffer_id = self.active_buffer();
                    let state = self.active_state();
                    let buffer = &state.buffer;
                    let total = buffer.total_bytes();
                    let content =
                        String::from_utf8_lossy(&buffer.slice_bytes(0..total)).to_string();

                    // Determine if TypeScript from file extension, default to TS
                    let is_ts = buffer
                        .file_path()
                        .and_then(|p| p.extension())
                        .and_then(|e| e.to_str())
                        .map(|e| e == "ts" || e == "tsx")
                        .unwrap_or(true);

                    // Derive plugin name from buffer filename
                    let name = buffer
                        .file_path()
                        .and_then(|p| p.file_name())
                        .and_then(|s| s.to_str())
                        .map(|s| s.to_string())
                        .unwrap_or_else(|| "buffer-plugin".to_string());

                    let load_result = self
                        .plugin_manager
                        .read()
                        .unwrap()
                        .load_plugin_from_source(&content, &name, is_ts);
                    match load_result {
                        Ok(()) => {
                            self.set_status_message(format!(
                                "Plugin '{}' loaded from buffer",
                                name
                            ));
                        }
                        Err(e) => {
                            self.set_status_message(format!("Failed to load plugin: {}", e));
                            tracing::error!("LoadPluginFromBuffer error: {}", e);
                        }
                    }

                    // Set up plugin dev workspace for LSP support
                    self.setup_plugin_dev_lsp(buffer_id, &content);
                }
                #[cfg(not(feature = "plugins"))]
                {
                    self.set_status_message(
                        "Plugins not available (compiled without plugin support)".to_string(),
                    );
                }
            }
            Action::InitReload => {
                // Same code path as auto-load: read init.ts and push it
                // through the existing plugin pipeline. The runtime's
                // hot-reload semantics drop prior commands / handlers /
                // event subs / settings before the new source runs.
                self.load_init_script(true);
                // Re-fire plugins_loaded so handlers expecting a "fresh"
                // post-load environment (M2) see it.
                self.fire_plugins_loaded_hook();
            }
            Action::InitEdit => {
                // Ensure the file exists (create from template if absent),
                // then open it in the editor so users can edit + reload.
                let config_dir = self.dir_context.config_dir.clone();
                match crate::init_script::ensure_starter(&config_dir) {
                    Ok(path) => {
                        // Regenerate `types/plugins.d.ts` from the live plugin
                        // set. It's written once at editor startup, but any
                        // plugin loaded/reloaded/unloaded since then would
                        // leave the aggregate stale (or missing, in builds
                        // where the plugins feature was off at boot but the
                        // user has since enabled a plugin). The user's
                        // tsconfig.json lists this file in `files`, so a
                        // stale copy is exactly when `getPluginApi("foo")`
                        // loses its typed overload.
                        let declarations =
                            self.plugin_manager.read().unwrap().plugin_declarations();
                        crate::init_script::write_plugin_declarations(&config_dir, &declarations);
                        match self.open_file(&path) {
                            Ok(_) => {
                                self.set_status_message(format!("init.ts: {}", path.display()));
                            }
                            Err(e) => {
                                self.set_status_message(format!("init.ts: open failed: {e}"));
                            }
                        }
                    }
                    Err(e) => {
                        self.set_status_message(format!("init.ts: create failed: {e}"));
                    }
                }
            }
            Action::InitCheck => {
                // Run the same parse check as `fresh --cmd init check` but
                // surface results in the status bar.
                let report = crate::init_script::check(&self.dir_context.config_dir);
                if report.ok && report.diagnostics.is_empty() {
                    self.set_status_message("init.ts: ok".into());
                } else if !report.ok {
                    let first = report
                        .diagnostics
                        .first()
                        .map(|d| format!("{}:{}: {}", d.line, d.column, d.message))
                        .unwrap_or_else(|| "unknown error".into());
                    self.set_status_message(format!(
                        "init.ts: {} error(s) — first: {first}",
                        report.diagnostics.len()
                    ));
                } else {
                    self.set_status_message(format!(
                        "init.ts: {} warning(s)",
                        report.diagnostics.len()
                    ));
                }
            }
            Action::OpenTerminal => {
                self.open_terminal();
            }
            Action::OpenTerminalRight => {
                self.open_terminal_split(crate::model::event::SplitDirection::Vertical);
            }
            Action::OpenTerminalBelow => {
                self.open_terminal_split(crate::model::event::SplitDirection::Horizontal);
            }
            Action::CloseTerminal => {
                self.close_terminal();
            }
            Action::RestartTerminal => {
                self.restart_terminal();
            }
            Action::FocusTerminal => {
                // If viewing a terminal buffer, drop the focused split into live
                // mode (clears its scrollback edge, re-enables PTY input,
                // truncates the stale screen tail, resizes).
                if self
                    .active_window()
                    .is_terminal_buffer(self.active_buffer())
                {
                    self.enter_terminal_mode();
                    self.set_status_message(t!("status.terminal_mode_enabled").to_string());
                }
            }
            Action::TerminalEscape => {
                // Drop the focused live terminal split into read-only scrollback.
                if self.active_window().focused_terminal_live() {
                    self.enter_terminal_scrollback();
                    self.set_status_message(t!("status.terminal_mode_disabled").to_string());
                }
            }
            Action::ToggleKeyboardCapture => {
                // Toggle keyboard capture mode in terminal
                if self.active_window().focused_terminal_live() {
                    self.active_window_mut().keyboard_capture =
                        !self.active_window_mut().keyboard_capture;
                    if self.active_window_mut().keyboard_capture {
                        self.set_status_message(
                            "Keyboard capture ON - all keys go to terminal (F9 to toggle)"
                                .to_string(),
                        );
                    } else {
                        self.set_status_message(
                            "Keyboard capture OFF - UI bindings active (F9 to toggle)".to_string(),
                        );
                    }
                }
            }
            Action::TerminalPaste => {
                // Paste clipboard contents into terminal as a single batch
                if self.active_window().focused_terminal_live() {
                    if let Some(text) = self.clipboard.paste() {
                        self.active_window_mut()
                            .send_terminal_input(text.as_bytes());
                    }
                }
            }
            Action::SendSelectionToTerminal => {
                self.send_selection_to_terminal();
            }
            Action::ShellCommand => {
                // Run shell command on buffer/selection, output to new buffer
                self.start_shell_command_prompt(false);
            }
            Action::ShellCommandReplace => {
                if self.refuse_if_editing_disabled() {
                    return Ok(());
                }
                // Run shell command on buffer/selection, replace content
                self.start_shell_command_prompt(true);
            }
            Action::OpenSettings => {
                self.open_settings();
            }
            Action::CloseSettings => {
                // Check if there are unsaved changes
                let has_changes = self
                    .settings_state
                    .as_ref()
                    .is_some_and(|s| s.has_changes());
                if has_changes {
                    // Show confirmation dialog
                    if let Some(ref mut state) = self.settings_state {
                        state.show_confirm_dialog();
                    }
                } else {
                    self.close_settings(false);
                }
            }
            Action::SettingsSave => {
                self.save_settings();
            }
            Action::SettingsReset => {
                if let Some(ref mut state) = self.settings_state {
                    state.reset_current_to_default();
                }
            }
            Action::SettingsInherit => {
                if let Some(ref mut state) = self.settings_state {
                    state.set_current_to_null();
                }
            }
            Action::SettingsToggleFocus => {
                if let Some(ref mut state) = self.settings_state {
                    state.toggle_focus();
                }
            }
            Action::SettingsActivate => {
                self.settings_activate_current();
            }
            Action::SettingsSearch => {
                if let Some(ref mut state) = self.settings_state {
                    state.start_search();
                }
            }
            Action::SettingsHelp => {
                if let Some(ref mut state) = self.settings_state {
                    state.toggle_help();
                }
            }
            Action::SettingsIncrement => {
                self.settings_increment_current();
            }
            Action::SettingsDecrement => {
                self.settings_decrement_current();
            }
            Action::CalibrateInput => {
                self.open_calibration_wizard();
            }
            Action::EventDebug => {
                self.active_window_mut().open_event_debug();
            }
            Action::SuspendProcess => {
                self.request_suspend();
            }
            Action::OpenKeybindingEditor => {
                self.open_keybinding_editor();
            }
            Action::PromptConfirm => {
                if let Some((input, prompt_type, selected_index)) = self.confirm_prompt() {
                    use super::prompt_actions::PromptResult;
                    match self.handle_prompt_confirm_input(input, prompt_type, selected_index) {
                        PromptResult::ExecuteAction(action) => {
                            return self.handle_action(action);
                        }
                        PromptResult::EarlyReturn => {
                            return Ok(());
                        }
                        PromptResult::Done => {}
                    }
                }
            }
            Action::PromptConfirmWithText(ref text) => {
                // For macro playback: set the prompt text before confirming
                if let Some(ref mut prompt) = self.active_window_mut().prompt {
                    prompt.set_input(text.clone());
                    self.update_prompt_suggestions();
                }
                if let Some((input, prompt_type, selected_index)) = self.confirm_prompt() {
                    use super::prompt_actions::PromptResult;
                    match self.handle_prompt_confirm_input(input, prompt_type, selected_index) {
                        PromptResult::ExecuteAction(action) => {
                            return self.handle_action(action);
                        }
                        PromptResult::EarlyReturn => {
                            return Ok(());
                        }
                        PromptResult::Done => {}
                    }
                }
            }
            Action::PopupConfirm => {
                use super::popup_actions::PopupConfirmResult;
                if let PopupConfirmResult::EarlyReturn = self.handle_popup_confirm() {
                    return Ok(());
                }
            }
            Action::PopupCancel => {
                self.handle_popup_cancel();
            }
            Action::PopupFocus => {
                self.handle_popup_focus();
            }
            Action::CompletionAccept => {
                use super::popup_actions::PopupConfirmResult;
                if let PopupConfirmResult::EarlyReturn = self.handle_popup_confirm() {
                    return Ok(());
                }
            }
            Action::CompletionDismiss => {
                self.handle_popup_cancel();
            }
            Action::InsertChar(c) => {
                if self.is_prompting() {
                    return self.handle_insert_char_prompt(c);
                } else if self.active_window_mut().key_context == KeyContext::FileExplorer {
                    self.active_window_mut().file_explorer_search_push_char(c);
                } else {
                    self.handle_insert_char_editor(c)?;
                }
            }
            // Prompt clipboard actions
            Action::PromptCopy => {
                if let Some(prompt) = &self.active_window_mut().prompt {
                    let text = prompt.selected_text().unwrap_or_else(|| prompt.get_text());
                    if !text.is_empty() {
                        self.clipboard.copy(text);
                        self.set_status_message(t!("clipboard.copied").to_string());
                    }
                }
            }
            Action::PromptCut => {
                if let Some(prompt) = &self.active_window_mut().prompt {
                    let text = prompt.selected_text().unwrap_or_else(|| prompt.get_text());
                    if !text.is_empty() {
                        self.clipboard.copy(text);
                    }
                }
                if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
                    if prompt.has_selection() {
                        prompt.delete_selection();
                    } else {
                        prompt.clear();
                    }
                }
                self.set_status_message(t!("clipboard.cut").to_string());
                self.update_prompt_suggestions();
            }
            Action::PromptPaste => {
                if let Some(text) = self.clipboard.paste() {
                    if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
                        prompt.insert_str(&text);
                    }
                    self.update_prompt_suggestions();
                }
            }

            // Prompt navigation and editing.
            //
            // These are the actions the `prompt` section of every keymap binds.
            // The prompt owns its editing logic in `Prompt`'s `InputHandler`
            // (suggestion sync, theme preview, plugin selection hooks, …), and
            // the handler runs *before* keybinding resolution — so a key it
            // handles itself never reaches its binding, and until now a key it
            // ignored resolved to an action with no dispatch arm at all and did
            // nothing. That left every `prompt` binding on a non-default key
            // silently dead (Emacs `C-b`/`C-n`/`C-g`, say).
            //
            // Each action is defined as "the same thing this key does", and
            // re-enters the prompt's handler with that key, so there is one
            // implementation of each operation rather than a second copy here
            // that would drift.
            Action::PromptCancel => self.dispatch_prompt_key_for_action(KeyCode::Esc, KM::NONE),
            Action::PromptBackspace => {
                self.dispatch_prompt_key_for_action(KeyCode::Backspace, KM::NONE)
            }
            Action::PromptDelete => self.dispatch_prompt_key_for_action(KeyCode::Delete, KM::NONE),
            Action::PromptMoveLeft => self.dispatch_prompt_key_for_action(KeyCode::Left, KM::NONE),
            Action::PromptMoveRight => {
                self.dispatch_prompt_key_for_action(KeyCode::Right, KM::NONE)
            }
            Action::PromptMoveStart => self.dispatch_prompt_key_for_action(KeyCode::Home, KM::NONE),
            Action::PromptMoveEnd => self.dispatch_prompt_key_for_action(KeyCode::End, KM::NONE),
            Action::PromptSelectPrev => self.dispatch_prompt_key_for_action(KeyCode::Up, KM::NONE),
            Action::PromptSelectNext => {
                self.dispatch_prompt_key_for_action(KeyCode::Down, KM::NONE)
            }
            Action::PromptPageUp => self.dispatch_prompt_key_for_action(KeyCode::PageUp, KM::NONE),
            Action::PromptPageDown => {
                self.dispatch_prompt_key_for_action(KeyCode::PageDown, KM::NONE)
            }
            Action::PromptAcceptSuggestion => {
                self.dispatch_prompt_key_for_action(KeyCode::Tab, KM::NONE)
            }
            Action::PromptMoveWordLeft => {
                self.dispatch_prompt_key_for_action(KeyCode::Left, KM::CONTROL)
            }
            Action::PromptMoveWordRight => {
                self.dispatch_prompt_key_for_action(KeyCode::Right, KM::CONTROL)
            }
            Action::PromptDeleteWordBackward => {
                self.dispatch_prompt_key_for_action(KeyCode::Backspace, KM::CONTROL)
            }
            Action::PromptDeleteWordForward => {
                self.dispatch_prompt_key_for_action(KeyCode::Delete, KM::CONTROL)
            }
            Action::PromptDeleteToLineEnd => {
                self.dispatch_prompt_key_for_action(KeyCode::Char('k'), KM::CONTROL)
            }
            Action::PromptSelectAll => {
                self.dispatch_prompt_key_for_action(KeyCode::Char('a'), KM::CONTROL)
            }
            Action::PromptMoveLeftSelecting => {
                self.dispatch_prompt_key_for_action(KeyCode::Left, KM::SHIFT)
            }
            Action::PromptMoveRightSelecting => {
                self.dispatch_prompt_key_for_action(KeyCode::Right, KM::SHIFT)
            }
            Action::PromptMoveHomeSelecting => {
                self.dispatch_prompt_key_for_action(KeyCode::Home, KM::SHIFT)
            }
            Action::PromptMoveEndSelecting => {
                self.dispatch_prompt_key_for_action(KeyCode::End, KM::SHIFT)
            }
            Action::PromptSelectWordLeft => {
                self.dispatch_prompt_key_for_action(KeyCode::Left, KM::CONTROL | KM::SHIFT)
            }
            Action::PromptSelectWordRight => {
                self.dispatch_prompt_key_for_action(KeyCode::Right, KM::CONTROL | KM::SHIFT)
            }
            _ => {
                // TODO: Why do we have this catch-all? It seems like actions should either:
                // 1. Be handled explicitly above (like InsertChar, PopupConfirm, etc.)
                // 2. Or be converted to events consistently
                // This catch-all makes it unclear which actions go through event conversion
                // vs. direct handling. Consider making this explicit or removing the pattern.
                self.apply_action_as_events(action)?;
            }
        }

        Ok(())
    }

    /// If the Quick Open prompt is currently open, cancel it and return `true`.
    /// All four Quick Open variants (CommandPalette, QuickOpen, QuickOpenBuffers,
    /// QuickOpenFiles) toggle off when invoked while the picker is already visible.
    fn close_quick_open_if_open(&mut self) -> bool {
        if let Some(prompt) = &self.active_window_mut().prompt {
            if prompt.prompt_type == PromptType::QuickOpen {
                self.cancel_prompt();
                return true;
            }
        }
        false
    }

    /// Re-run the active search after a search-option flag is toggled.
    /// If a search prompt is open, updates incremental highlights from the
    /// prompt's current input. Otherwise re-executes the last completed search.
    fn refresh_active_search(&mut self) {
        if let Some(prompt) = &self.active_window_mut().prompt {
            if matches!(
                prompt.prompt_type,
                PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch
            ) {
                let query = prompt.input_str().to_string();
                // Drop the committed matches: they were collected under the
                // old flags, and F3/Shift+F3 now step through them while the
                // bar is open (issue #2111). Clearing makes the next press
                // re-run the search under the new flags.
                self.active_window_mut().search_state = None;
                self.update_search_highlights(&query);
            }
        } else if let Some(search_state) = &self.active_window().search_state {
            let query = search_state.query.clone();
            self.perform_search(&query);
        }
    }

    /// Open a terminal in the utility dock, creating the dock split if none exists yet.
    fn handle_open_terminal_in_dock(&mut self) -> AnyhowResult<()> {
        use crate::model::event::SplitDirection;
        use crate::view::split::SplitRole;

        if let Some(dock_leaf) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .find_leaf_by_role(SplitRole::UtilityDock)
        {
            // Existing dock — focus it and let the regular open_terminal path attach a new tab.
            self.windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_manager_mut())
                .expect("active window must have a populated split layout")
                .set_active_split(dock_leaf);
            self.open_terminal();
            return Ok(());
        }

        // No dock yet. Spawn the PTY first so we have a real terminal buffer to seed the new
        // dock leaf with — otherwise the leaf would carry the user's previously-active buffer
        // as a placeholder and that buffer would linger as a phantom tab in the dock.
        let Some(terminal_id) = self.spawn_terminal_session() else {
            return Ok(());
        };
        let buffer_id = self.create_terminal_buffer_detached(terminal_id);

        // Split at the root so the dock spans the full width below any pre-existing side-by-side panes.
        let new_leaf = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .split_root_positioned(SplitDirection::Horizontal, buffer_id, 0.7, false)
            .map_err(|e| {
                self.set_status_message(format!("Failed to create dock for terminal: {}", e));
            });
        let Ok(new_leaf) = new_leaf else {
            return Ok(());
        };

        let mut view_state = crate::view::split::SplitViewState::with_buffer(
            self.terminal_width,
            self.terminal_height,
            buffer_id,
        );
        // Terminal-dedicated splits never show line numbers or current-line highlight.
        // (Mirrors the plugin-terminal split setup in `create_plugin_terminal`.)
        view_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
            line_numbers: false,
            highlight_current_line: false,
            line_wrap: self.active_window().resolve_line_wrap_for_buffer(buffer_id),
            wrap_indent: self.config.editor.wrap_indent,
            wrap_column: self
                .active_window()
                .resolve_wrap_column_for_buffer(buffer_id),
            rulers: self.config.editor.rulers.clone(),
            scroll_offset: 0,
        });
        // Terminals don't wrap — keep escape sequences intact.
        view_state.viewport.line_wrap_enabled = false;

        self.windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .insert(new_leaf, view_state);
        self.windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .set_leaf_role(new_leaf, Some(SplitRole::UtilityDock));
        self.windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .set_active_split(new_leaf);

        // Mirror open_terminal's post-attach bookkeeping. The buffer was
        // created via `create_terminal_buffer_detached` (empty scrollback set),
        // so it is live in this split; focus the terminal pane.
        self.active_window_mut().key_context = crate::input::keybindings::KeyContext::Terminal;
        self.active_window_mut().resize_visible_terminals();

        let exit_key = self
            .keybindings
            .read()
            .unwrap()
            .find_keybinding_for_action(
                "terminal_escape",
                crate::input::keybindings::KeyContext::Terminal,
            )
            .unwrap_or_else(|| "Ctrl+Space".to_string());
        self.set_status_message(
            rust_i18n::t!("terminal.opened", id = terminal_id.0, exit_key = exit_key).to_string(),
        );
        tracing::info!(
            "Opened terminal {:?} into new dock leaf {:?} (buffer {:?})",
            terminal_id,
            new_leaf,
            buffer_id
        );
        Ok(())
    }

    /// Run one of the `prompt_*` actions by re-entering the prompt's own
    /// input handler with the key that means the same thing.
    ///
    /// The prompt keeps its editing logic in `Prompt`'s `InputHandler` and runs
    /// it ahead of keybinding resolution, so an action arm that reimplemented
    /// (say) suggestion navigation would be a second copy of the sync, theme
    /// preview and plugin-notification behaviour, free to drift from the one
    /// the arrow keys use. Delegating keeps exactly one implementation.
    ///
    /// A no-op when no prompt is open — an action bound in the `prompt`
    /// context can still be reached from elsewhere.
    fn dispatch_prompt_key_for_action(
        &mut self,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) {
        use crate::input::handler::{InputContext, InputHandler};
        let mut ctx = InputContext::new();
        let event = crossterm::event::KeyEvent::new(code, modifiers);
        let handled = match self.active_window_mut().prompt.as_mut() {
            Some(prompt) => {
                prompt.handle_key_event(&event, &mut ctx);
                true
            }
            None => false,
        };
        if handled {
            self.process_deferred_actions(ctx);
        }
    }
}
