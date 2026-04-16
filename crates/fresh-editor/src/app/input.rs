use super::*;
use crate::model::event::LeafId;
use crate::services::plugins::hooks::HookArgs;
use anyhow::Result as AnyhowResult;
use rust_i18n::t;
impl Editor {
    /// Determine the current keybinding context based on UI state
    pub fn get_key_context(&self) -> crate::input::keybindings::KeyContext {
        use crate::input::keybindings::KeyContext;

        // Priority order: Settings > Menu > Prompt > Popup > CompositeBuffer > Current context (FileExplorer or Normal)
        if self.settings_state.as_ref().is_some_and(|s| s.visible) {
            KeyContext::Settings
        } else if self.menu_state.active_menu.is_some() {
            KeyContext::Menu
        } else if self.is_prompting() {
            KeyContext::Prompt
        } else if self.active_state().popups.is_visible() {
            KeyContext::Popup
        } else if self.is_composite_buffer(self.active_buffer()) {
            KeyContext::CompositeBuffer
        } else {
            // Use the current context (can be FileExplorer or Normal)
            self.key_context.clone()
        }
    }

    /// Handle a key event and return whether it was handled
    /// This is the central key handling logic used by both main.rs and tests
    pub fn handle_key(
        &mut self,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        use crate::input::keybindings::Action;

        let _t_total = std::time::Instant::now();

        tracing::trace!(
            "Editor.handle_key: code={:?}, modifiers={:?}",
            code,
            modifiers
        );

        // Create key event for dispatch methods
        let key_event = crossterm::event::KeyEvent::new(code, modifiers);

        // Event debug dialog intercepts ALL key events before any other processing.
        // This must be checked here (not just in main.rs/gui) so it works in
        // client/server mode where handle_key is called directly.
        if self.is_event_debug_active() {
            self.handle_event_debug_input(&key_event);
            return Ok(());
        }

        // Try terminal input dispatch first (handles terminal mode and re-entry)
        if self.dispatch_terminal_input(&key_event).is_some() {
            return Ok(());
        }

        // Clear skip_ensure_visible flag so cursor becomes visible after key press
        // (scroll actions will set it again if needed). Use the *effective*
        // active split so this clears the flag on a focused buffer-group
        // panel's own view state, not the group host's — without this, a
        // scroll action in the panel (mouse scrollbar click, plugin
        // scrollBufferToLine, etc.) sets `skip_ensure_visible` on the panel
        // and subsequent key presses never clear it, so cursor motion stops
        // scrolling the viewport.
        let active_split = self.effective_active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.viewport.clear_skip_ensure_visible();
        }

        // Dismiss theme info popup on any key press
        if self.theme_info_popup.is_some() {
            self.theme_info_popup = None;
        }

        // Determine the current context first
        let mut context = self.get_key_context();

        // Special case: Hover and Signature Help popups should be dismissed on any key press
        // EXCEPT for Ctrl+C when the popup has a text selection (allow copy first)
        if matches!(context, crate::input::keybindings::KeyContext::Popup) {
            // Check if the current popup is transient (hover, signature help)
            let (is_transient_popup, has_selection) = {
                let popup = self.active_state().popups.top();
                (
                    popup.is_some_and(|p| p.transient),
                    popup.is_some_and(|p| p.has_selection()),
                )
            };

            // Don't dismiss if popup has selection and user is pressing Ctrl+C (let them copy first)
            let is_copy_key = key_event.code == crossterm::event::KeyCode::Char('c')
                && key_event
                    .modifiers
                    .contains(crossterm::event::KeyModifiers::CONTROL);

            if is_transient_popup && !(has_selection && is_copy_key) {
                // Dismiss the popup on any key press (except Ctrl+C with selection)
                self.hide_popup();
                tracing::debug!("Dismissed transient popup on key press");
                // Recalculate context now that popup is gone
                context = self.get_key_context();
            }
        }

        // Try hierarchical modal input dispatch first (Settings, Menu, Prompt, Popup)
        if self.dispatch_modal_input(&key_event).is_some() {
            return Ok(());
        }

        // If a modal was dismissed (e.g., completion popup closed and returned Ignored),
        // recalculate the context so the key is processed in the correct context.
        if context != self.get_key_context() {
            context = self.get_key_context();
        }

        // Only check buffer mode keybindings when the editor buffer has focus.
        // FileExplorer, Menu, Prompt, Popup contexts should not trigger mode bindings
        // (e.g. markdown-source's Enter handler should not fire while the explorer is focused).
        let should_check_mode_bindings =
            matches!(context, crate::input::keybindings::KeyContext::Normal);

        if should_check_mode_bindings {
            // effective_mode() returns buffer-local mode if present, else global mode.
            // This ensures virtual buffer modes aren't hijacked by global modes.
            let effective_mode = self.effective_mode().map(|s| s.to_owned());

            if let Some(ref mode_name) = effective_mode {
                let mode_ctx = crate::input::keybindings::KeyContext::Mode(mode_name.to_string());
                let key_event = crossterm::event::KeyEvent::new(code, modifiers);

                // Mode chord resolution (via KeybindingResolver)
                let (chord_result, resolved_action) = {
                    let keybindings = self.keybindings.read().unwrap();
                    let chord_result =
                        keybindings.resolve_chord(&self.chord_state, &key_event, mode_ctx.clone());
                    let resolved = keybindings.resolve(&key_event, mode_ctx);
                    (chord_result, resolved)
                };
                match chord_result {
                    crate::input::keybindings::ChordResolution::Complete(action) => {
                        tracing::debug!("Mode chord resolved to action: {:?}", action);
                        self.chord_state.clear();
                        return self.handle_action(action);
                    }
                    crate::input::keybindings::ChordResolution::Partial => {
                        tracing::debug!("Potential chord prefix in mode '{}'", mode_name);
                        self.chord_state.push((code, modifiers));
                        return Ok(());
                    }
                    crate::input::keybindings::ChordResolution::NoMatch => {
                        if !self.chord_state.is_empty() {
                            tracing::debug!("Chord sequence abandoned in mode, clearing state");
                            self.chord_state.clear();
                        }
                    }
                }

                // Mode single-key resolution (custom > keymap > plugin defaults)
                if resolved_action != Action::None {
                    return self.handle_action(resolved_action);
                }
            }

            // Handle unbound keys for modes that want to capture input.
            //
            // Buffer-local modes with allow_text_input (e.g. search-replace-list)
            // capture character keys and block other unbound keys.
            //
            // Buffer-local modes WITHOUT allow_text_input (e.g. diff-view) let
            // unbound keys fall through to normal keybinding handling so that
            // Ctrl+C, arrows, etc. still work.
            //
            // Global editor modes (e.g. vi-normal) block all unbound keys when
            // read-only.
            if let Some(ref mode_name) = effective_mode {
                if self.mode_registry.allows_text_input(mode_name) {
                    if let KeyCode::Char(c) = code {
                        let ch = if modifiers.contains(KeyModifiers::SHIFT) {
                            c.to_uppercase().next().unwrap_or(c)
                        } else {
                            c
                        };
                        if !modifiers.intersects(KeyModifiers::CONTROL | KeyModifiers::ALT) {
                            let action_name = format!("mode_text_input:{}", ch);
                            return self.handle_action(Action::PluginAction(action_name));
                        }
                    }
                    tracing::debug!("Blocking unbound key in text-input mode '{}'", mode_name);
                    return Ok(());
                }
            }
            if let Some(ref mode_name) = self.editor_mode {
                if self.mode_registry.is_read_only(mode_name) {
                    tracing::debug!("Ignoring unbound key in read-only mode '{}'", mode_name);
                    return Ok(());
                }
                tracing::debug!(
                    "Mode '{}' is not read-only, allowing key through",
                    mode_name
                );
            }
        }

        // --- Composite buffer input routing ---
        // If the active buffer is a composite buffer (side-by-side diff),
        // route remaining composite-specific keys (scroll, pane switch, close)
        // through CompositeInputRouter before falling through to regular
        // keybinding resolution. Hunk navigation (n/p/]/[) is handled by the
        // Action system via CompositeBuffer context bindings.
        {
            let active_buf = self.active_buffer();
            let active_split = self.effective_active_split();
            if self.is_composite_buffer(active_buf) {
                if let Some(handled) =
                    self.try_route_composite_key(active_split, active_buf, &key_event)
                {
                    return handled;
                }
            }
        }

        // Check for chord sequence matches first
        let key_event = crossterm::event::KeyEvent::new(code, modifiers);
        let (chord_result, action) = {
            let keybindings = self.keybindings.read().unwrap();
            let chord_result =
                keybindings.resolve_chord(&self.chord_state, &key_event, context.clone());
            let action = keybindings.resolve(&key_event, context.clone());
            (chord_result, action)
        };

        match chord_result {
            crate::input::keybindings::ChordResolution::Complete(action) => {
                // Complete chord match - execute action and clear chord state
                tracing::debug!("Complete chord match -> Action: {:?}", action);
                self.chord_state.clear();
                return self.handle_action(action);
            }
            crate::input::keybindings::ChordResolution::Partial => {
                // Partial match - add to chord state and wait for more keys
                tracing::debug!("Partial chord match - waiting for next key");
                self.chord_state.push((code, modifiers));
                return Ok(());
            }
            crate::input::keybindings::ChordResolution::NoMatch => {
                // No chord match - clear state and try regular resolution
                if !self.chord_state.is_empty() {
                    tracing::debug!("Chord sequence abandoned, clearing state");
                    self.chord_state.clear();
                }
            }
        }

        // Regular single-key resolution (already resolved above)
        tracing::trace!("Context: {:?} -> Action: {:?}", context, action);

        // Cancel pending LSP requests on user actions (except LSP actions themselves)
        // This ensures stale completions don't show up after the user has moved on
        match action {
            Action::LspCompletion
            | Action::LspGotoDefinition
            | Action::LspReferences
            | Action::LspHover
            | Action::None => {
                // Don't cancel for LSP actions or no-op
            }
            _ => {
                // Cancel any pending LSP requests
                self.cancel_pending_lsp_requests();
            }
        }

        // Note: Modal components (Settings, Menu, Prompt, Popup, File Browser) are now
        // handled by dispatch_modal_input using the InputHandler system.
        // All remaining actions delegate to handle_action.
        self.handle_action(action)
    }

    /// Handle an action (for normal mode and command execution).
    /// Used by the app module internally and by the GUI module for native menu dispatch.
    pub(crate) fn handle_action(&mut self, action: Action) -> AnyhowResult<()> {
        use crate::input::keybindings::Action;

        // Record action to macro if recording
        self.record_macro_action(&action);

        // Reset dabbrev cycling session on any non-dabbrev action.
        if !matches!(action, Action::DabbrevExpand) {
            self.reset_dabbrev_state();
        }

        match action {
            Action::Quit => self.quit(),
            Action::ForceQuit => {
                self.should_quit = true;
            }
            Action::Detach => {
                self.should_detach = true;
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
                    self.status_message = Some(t!("file.save_failed", error = &msg).to_string());
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
                        p.strip_prefix(&self.working_dir)
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
                let has_line_index = self
                    .buffers
                    .get(&self.active_buffer())
                    .is_none_or(|s| s.buffer.line_count().is_some());
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
            Action::FormatBuffer => {
                if let Err(e) = self.format_buffer() {
                    self.set_status_message(
                        t!("error.format_failed", error = e.to_string()).to_string(),
                    );
                }
            }
            Action::TrimTrailingWhitespace => match self.trim_trailing_whitespace() {
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
            },
            Action::EnsureFinalNewline => match self.ensure_final_newline() {
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
            },
            Action::Copy => {
                // Check if there's an active popup with text selection
                let state = self.active_state();
                if let Some(popup) = state.popups.top() {
                    if popup.has_selection() {
                        if let Some(text) = popup.get_selected_text() {
                            self.clipboard.copy(text);
                            self.set_status_message(t!("clipboard.copied").to_string());
                            return Ok(());
                        }
                    }
                }
                // Check if active buffer is a composite buffer
                let buffer_id = self.active_buffer();
                if self.is_composite_buffer(buffer_id) {
                    if let Some(_handled) = self.handle_composite_action(buffer_id, &Action::Copy) {
                        return Ok(());
                    }
                }
                self.copy_selection()
            }
            Action::CopyWithTheme(theme) => self.copy_selection_with_theme(&theme),
            Action::Cut => {
                if self.is_editing_disabled() {
                    self.set_status_message(t!("buffer.editing_disabled").to_string());
                    return Ok(());
                }
                self.cut_selection()
            }
            Action::Paste => {
                if self.is_editing_disabled() {
                    self.set_status_message(t!("buffer.editing_disabled").to_string());
                    return Ok(());
                }
                self.paste()
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
                self.open_help_manual();
            }
            Action::ShowKeyboardShortcuts => {
                self.open_keyboard_shortcuts();
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
            Action::ClearWarnings => {
                self.clear_warnings();
            }
            Action::CommandPalette => {
                // CommandPalette now delegates to QuickOpen (which starts with ">" prefix
                // for command mode). Toggle if already open.
                if let Some(prompt) = &self.prompt {
                    if prompt.prompt_type == PromptType::QuickOpen {
                        self.cancel_prompt();
                        return Ok(());
                    }
                }
                self.start_quick_open();
            }
            Action::QuickOpen => {
                // Toggle Quick Open: close if already open, otherwise open it
                if let Some(prompt) = &self.prompt {
                    if prompt.prompt_type == PromptType::QuickOpen {
                        self.cancel_prompt();
                        return Ok(());
                    }
                }

                // Start Quick Open with file suggestions (default mode)
                self.start_quick_open();
            }
            Action::ToggleLineWrap => {
                self.config.editor.line_wrap = !self.config.editor.line_wrap;

                // Update all viewports to reflect the new line wrap setting,
                // respecting per-language overrides
                let leaf_ids: Vec<_> = self.split_view_states.keys().copied().collect();
                for leaf_id in leaf_ids {
                    let buffer_id = self
                        .split_manager
                        .get_buffer_id(leaf_id.into())
                        .unwrap_or(BufferId(0));
                    let effective_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
                    let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
                    if let Some(view_state) = self.split_view_states.get_mut(&leaf_id) {
                        view_state.viewport.line_wrap_enabled = effective_wrap;
                        view_state.viewport.wrap_indent = self.config.editor.wrap_indent;
                        view_state.viewport.wrap_column = wrap_column;
                    }
                }

                let state = if self.config.editor.line_wrap {
                    t!("view.state_enabled").to_string()
                } else {
                    t!("view.state_disabled").to_string()
                };
                self.set_status_message(t!("view.line_wrap_state", state = state).to_string());
            }
            Action::ToggleCurrentLineHighlight => {
                self.config.editor.highlight_current_line =
                    !self.config.editor.highlight_current_line;

                // Update all splits
                let leaf_ids: Vec<_> = self.split_view_states.keys().copied().collect();
                for leaf_id in leaf_ids {
                    if let Some(view_state) = self.split_view_states.get_mut(&leaf_id) {
                        view_state.highlight_current_line =
                            self.config.editor.highlight_current_line;
                    }
                }

                let state = if self.config.editor.highlight_current_line {
                    t!("view.state_enabled").to_string()
                } else {
                    t!("view.state_disabled").to_string()
                };
                self.set_status_message(
                    t!("view.current_line_highlight_state", state = state).to_string(),
                );
            }
            Action::ToggleReadOnly => {
                let buffer_id = self.active_buffer();
                let is_now_read_only = self
                    .buffer_metadata
                    .get(&buffer_id)
                    .map(|m| !m.read_only)
                    .unwrap_or(false);
                self.mark_buffer_read_only(buffer_id, is_now_read_only);

                let state_str = if is_now_read_only {
                    t!("view.state_enabled").to_string()
                } else {
                    t!("view.state_disabled").to_string()
                };
                self.set_status_message(t!("view.read_only_state", state = state_str).to_string());
            }
            Action::TogglePageView => {
                self.handle_toggle_page_view();
            }
            Action::SetPageWidth => {
                let active_split = self.split_manager.active_split();
                let current = self
                    .split_view_states
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
                        p.strip_prefix(&self.working_dir)
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
                let is_search_prompt = self.prompt.as_ref().is_some_and(|p| {
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
                // Use same flow as query-replace, just with confirm_each defaulting to false
                self.start_search_prompt(
                    t!("file.replace_prompt").to_string(),
                    PromptType::ReplaceSearch,
                    false,
                );
            }
            Action::QueryReplace => {
                // Enable confirm mode by default for query-replace
                self.search_confirm_each = true;
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
            Action::AddCursorNextMatch => self.add_cursor_at_next_match(),
            Action::AddCursorAbove => self.add_cursor_above(),
            Action::AddCursorBelow => self.add_cursor_below(),
            Action::NextBuffer => self.next_buffer(),
            Action::PrevBuffer => self.prev_buffer(),
            Action::SwitchToPreviousTab => self.switch_to_previous_tab(),
            Action::SwitchToTabByName => self.start_switch_to_tab_prompt(),

            // Tab scrolling (manual scroll - don't auto-adjust)
            Action::ScrollTabsLeft => {
                let active_split_id = self.split_manager.active_split();
                if let Some(view_state) = self.split_view_states.get_mut(&active_split_id) {
                    view_state.tab_scroll_offset = view_state.tab_scroll_offset.saturating_sub(5);
                    self.set_status_message(t!("status.scrolled_tabs_left").to_string());
                }
            }
            Action::ScrollTabsRight => {
                let active_split_id = self.split_manager.active_split();
                if let Some(view_state) = self.split_view_states.get_mut(&active_split_id) {
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
            Action::IncreaseSplitSize => self.adjust_split_size(0.05),
            Action::DecreaseSplitSize => self.adjust_split_size(-0.05),
            Action::ToggleMaximizeSplit => self.toggle_maximize_split(),
            Action::ToggleFileExplorer => self.toggle_file_explorer(),
            Action::ToggleMenuBar => self.toggle_menu_bar(),
            Action::ToggleTabBar => self.toggle_tab_bar(),
            Action::ToggleStatusBar => self.toggle_status_bar(),
            Action::TogglePromptLine => self.toggle_prompt_line(),
            Action::ToggleVerticalScrollbar => self.toggle_vertical_scrollbar(),
            Action::ToggleHorizontalScrollbar => self.toggle_horizontal_scrollbar(),
            Action::ToggleLineNumbers => self.toggle_line_numbers(),
            Action::ToggleScrollSync => self.toggle_scroll_sync(),
            Action::ToggleMouseCapture => self.toggle_mouse_capture(),
            Action::ToggleMouseHover => self.toggle_mouse_hover(),
            Action::ToggleDebugHighlights => self.toggle_debug_highlights(),
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
                    .buffers
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
                if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
                    state.buffer_settings.use_tabs = !state.buffer_settings.use_tabs;
                    let status = if state.buffer_settings.use_tabs {
                        "Indentation: Tabs"
                    } else {
                        "Indentation: Spaces"
                    };
                    self.set_status_message(status.to_string());
                }
            }
            Action::ToggleTabIndicators | Action::ToggleWhitespaceIndicators => {
                if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
                    state.buffer_settings.whitespace.toggle_all();
                    let status = if state.buffer_settings.whitespace.any_visible() {
                        t!("toggle.whitespace_indicators_shown")
                    } else {
                        t!("toggle.whitespace_indicators_hidden")
                    };
                    self.set_status_message(status.to_string());
                }
            }
            Action::ResetBufferSettings => self.reset_buffer_settings(),
            Action::FocusFileExplorer => self.focus_file_explorer(),
            Action::FocusEditor => self.focus_editor(),
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
            Action::FileExplorerSearchClear => self.file_explorer_search_clear(),
            Action::FileExplorerSearchBackspace => self.file_explorer_search_pop_char(),
            Action::RemoveSecondaryCursors => {
                // Convert action to events and apply them
                if let Some(events) = self.action_to_events(Action::RemoveSecondaryCursors) {
                    // Wrap in batch for atomic undo
                    let batch = Event::Batch {
                        events: events.clone(),
                        description: "Remove secondary cursors".to_string(),
                    };
                    self.active_event_log_mut().append(batch.clone());
                    self.apply_event_to_active_buffer(&batch);

                    // Ensure the primary cursor is visible after removing secondary cursors
                    let active_split = self.split_manager.active_split();
                    let active_buffer = self.active_buffer();
                    if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                        let state = self.buffers.get_mut(&active_buffer).unwrap();
                        view_state.ensure_cursor_visible(&mut state.buffer, &state.marker_list);
                    }
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
                if self.config.editor.menu_bar_mnemonics {
                    self.handle_menu_open(&menu_name);
                }
            }

            Action::SwitchKeybindingMap(map_name) => {
                // Check if the map exists (either built-in or user-defined)
                let is_builtin =
                    matches!(map_name.as_str(), "default" | "emacs" | "vscode" | "macos");
                let is_user_defined = self.config.keybinding_maps.contains_key(&map_name);

                if is_builtin || is_user_defined {
                    // Update the active keybinding map in config
                    self.config.active_keybinding_map = map_name.clone().into();

                    // Reload the keybinding resolver with the new map
                    *self.keybindings.write().unwrap() =
                        crate::input::keybindings::KeybindingResolver::new(&self.config);

                    self.set_status_message(
                        t!("view.keybindings_switched", map = map_name).to_string(),
                    );
                } else {
                    self.set_status_message(
                        t!("view.keybindings_unknown", map = map_name).to_string(),
                    );
                }
            }

            Action::SmartHome => {
                // In composite (diff) views, use LineStart movement
                let buffer_id = self.active_buffer();
                if self.is_composite_buffer(buffer_id) {
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
                self.toggle_fold_at_cursor();
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
                self.set_bookmark(key);
            }
            Action::JumpToBookmark(key) => {
                self.jump_to_bookmark(key);
            }
            Action::ClearBookmark(key) => {
                self.clear_bookmark(key);
            }
            Action::ListBookmarks => {
                self.list_bookmarks();
            }
            Action::ToggleSearchCaseSensitive => {
                self.search_case_sensitive = !self.search_case_sensitive;
                let state = if self.search_case_sensitive {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(
                    t!("search.case_sensitive_state", state = state).to_string(),
                );
                // Update incremental highlights if in search prompt, otherwise re-run completed search
                // Check prompt FIRST since we want to use current prompt input, not stale search_state
                if let Some(prompt) = &self.prompt {
                    if matches!(
                        prompt.prompt_type,
                        PromptType::Search
                            | PromptType::ReplaceSearch
                            | PromptType::QueryReplaceSearch
                    ) {
                        let query = prompt.input.clone();
                        self.update_search_highlights(&query);
                    }
                } else if let Some(search_state) = &self.search_state {
                    let query = search_state.query.clone();
                    self.perform_search(&query);
                }
            }
            Action::ToggleSearchWholeWord => {
                self.search_whole_word = !self.search_whole_word;
                let state = if self.search_whole_word {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(t!("search.whole_word_state", state = state).to_string());
                // Update incremental highlights if in search prompt, otherwise re-run completed search
                // Check prompt FIRST since we want to use current prompt input, not stale search_state
                if let Some(prompt) = &self.prompt {
                    if matches!(
                        prompt.prompt_type,
                        PromptType::Search
                            | PromptType::ReplaceSearch
                            | PromptType::QueryReplaceSearch
                    ) {
                        let query = prompt.input.clone();
                        self.update_search_highlights(&query);
                    }
                } else if let Some(search_state) = &self.search_state {
                    let query = search_state.query.clone();
                    self.perform_search(&query);
                }
            }
            Action::ToggleSearchRegex => {
                self.search_use_regex = !self.search_use_regex;
                let state = if self.search_use_regex {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(t!("search.regex_state", state = state).to_string());
                // Update incremental highlights if in search prompt, otherwise re-run completed search
                // Check prompt FIRST since we want to use current prompt input, not stale search_state
                if let Some(prompt) = &self.prompt {
                    if matches!(
                        prompt.prompt_type,
                        PromptType::Search
                            | PromptType::ReplaceSearch
                            | PromptType::QueryReplaceSearch
                    ) {
                        let query = prompt.input.clone();
                        self.update_search_highlights(&query);
                    }
                } else if let Some(search_state) = &self.search_state {
                    let query = search_state.query.clone();
                    self.perform_search(&query);
                }
            }
            Action::ToggleSearchConfirmEach => {
                self.search_confirm_each = !self.search_confirm_each;
                let state = if self.search_confirm_each {
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
                if let Some(key) = self.macros.last_register() {
                    self.play_macro(key);
                } else {
                    self.set_status_message(t!("status.no_macro_recorded").to_string());
                }
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
                self.composite_next_hunk_active(buf);
            }
            Action::CompositePrevHunk => {
                let buf = self.active_buffer();
                self.composite_prev_hunk_active(buf);
            }
            Action::None => {}
            Action::DeleteBackward => {
                if self.is_editing_disabled() {
                    self.set_status_message(t!("buffer.editing_disabled").to_string());
                    return Ok(());
                }
                // Normal backspace handling
                if let Some(events) = self.action_to_events(Action::DeleteBackward) {
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
                if let Some(result) = self.plugin_manager.execute_action_async(&action_name) {
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
                    self.set_status_message(t!("status.plugin_manager_unavailable").to_string());
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

                    match self
                        .plugin_manager
                        .load_plugin_from_source(&content, &name, is_ts)
                    {
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
            Action::OpenTerminal => {
                self.open_terminal();
            }
            Action::CloseTerminal => {
                self.close_terminal();
            }
            Action::FocusTerminal => {
                // If viewing a terminal buffer, switch to terminal mode
                if self.is_terminal_buffer(self.active_buffer()) {
                    self.terminal_mode = true;
                    self.key_context = KeyContext::Terminal;
                    self.set_status_message(t!("status.terminal_mode_enabled").to_string());
                }
            }
            Action::TerminalEscape => {
                // Exit terminal mode back to editor
                if self.terminal_mode {
                    self.terminal_mode = false;
                    self.key_context = KeyContext::Normal;
                    self.set_status_message(t!("status.terminal_mode_disabled").to_string());
                }
            }
            Action::ToggleKeyboardCapture => {
                // Toggle keyboard capture mode in terminal
                if self.terminal_mode {
                    self.keyboard_capture = !self.keyboard_capture;
                    if self.keyboard_capture {
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
                if self.terminal_mode {
                    if let Some(text) = self.clipboard.paste() {
                        self.send_terminal_input(text.as_bytes());
                    }
                }
            }
            Action::ShellCommand => {
                // Run shell command on buffer/selection, output to new buffer
                self.start_shell_command_prompt(false);
            }
            Action::ShellCommandReplace => {
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
                self.open_event_debug();
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
                if let Some(ref mut prompt) = self.prompt {
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
            Action::InsertChar(c) => {
                if self.is_prompting() {
                    return self.handle_insert_char_prompt(c);
                } else if self.key_context == KeyContext::FileExplorer {
                    self.file_explorer_search_push_char(c);
                } else {
                    self.handle_insert_char_editor(c)?;
                }
            }
            // Prompt clipboard actions
            Action::PromptCopy => {
                if let Some(prompt) = &self.prompt {
                    let text = prompt.selected_text().unwrap_or_else(|| prompt.get_text());
                    if !text.is_empty() {
                        self.clipboard.copy(text);
                        self.set_status_message(t!("clipboard.copied").to_string());
                    }
                }
            }
            Action::PromptCut => {
                if let Some(prompt) = &self.prompt {
                    let text = prompt.selected_text().unwrap_or_else(|| prompt.get_text());
                    if !text.is_empty() {
                        self.clipboard.copy(text);
                    }
                }
                if let Some(prompt) = self.prompt.as_mut() {
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
                    if let Some(prompt) = self.prompt.as_mut() {
                        prompt.insert_str(&text);
                    }
                    self.update_prompt_suggestions();
                }
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


    /// Move the cursor to a visible position within the current viewport
    /// This is called after scrollbar operations to ensure the cursor is in view
    pub(super) fn move_cursor_to_visible_area(&mut self, split_id: LeafId, buffer_id: BufferId) {
        // Get viewport info from SplitViewState
        let (top_byte, viewport_height) =
            if let Some(view_state) = self.split_view_states.get(&split_id) {
                (
                    view_state.viewport.top_byte,
                    view_state.viewport.height as usize,
                )
            } else {
                return;
            };

        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let buffer_len = state.buffer.len();

            // Find the bottom byte of the viewport
            // We iterate through viewport_height lines starting from top_byte
            let mut iter = state.buffer.line_iterator(top_byte, 80);
            let mut bottom_byte = buffer_len;

            // Consume viewport_height lines to find where the visible area ends
            for _ in 0..viewport_height {
                if let Some((pos, line)) = iter.next_line() {
                    // The bottom of this line is at pos + line.len()
                    bottom_byte = pos + line.len();
                } else {
                    // Reached end of buffer
                    bottom_byte = buffer_len;
                    break;
                }
            }

            // Check if cursor is outside visible range and move it if needed
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                let cursor_pos = view_state.cursors.primary().position;
                if cursor_pos < top_byte || cursor_pos > bottom_byte {
                    // Move cursor to the top of the viewport
                    let cursor = view_state.cursors.primary_mut();
                    cursor.position = top_byte;
                    // Keep the existing sticky_column value so vertical navigation preserves column
                }
            }
        }
    }

    /// Calculate the maximum allowed scroll position
    /// Ensures the last line is always at the bottom unless the buffer is smaller than viewport
    pub(super) fn calculate_max_scroll_position(
        buffer: &mut crate::model::buffer::Buffer,
        viewport_height: usize,
    ) -> usize {
        if viewport_height == 0 {
            return 0;
        }

        let buffer_len = buffer.len();
        if buffer_len == 0 {
            return 0;
        }

        // Count total lines in buffer
        let mut line_count = 0;
        let mut iter = buffer.line_iterator(0, 80);
        while iter.next_line().is_some() {
            line_count += 1;
        }

        // If buffer has fewer lines than viewport, can't scroll at all
        if line_count <= viewport_height {
            return 0;
        }

        // Calculate how many lines from the start we can scroll
        // We want to be able to scroll so that the last line is at the bottom
        let scrollable_lines = line_count.saturating_sub(viewport_height);

        // Find the byte position of the line at scrollable_lines offset
        let mut iter = buffer.line_iterator(0, 80);
        let mut current_line = 0;
        let mut max_byte_pos = 0;

        while current_line < scrollable_lines {
            if let Some((pos, _content)) = iter.next_line() {
                max_byte_pos = pos;
                current_line += 1;
            } else {
                break;
            }
        }

        max_byte_pos
    }

    pub(super) fn fold_toggle_line_at_screen_position(
        &self,
        col: u16,
        row: u16,
    ) -> Option<(BufferId, usize)> {
        for (split_id, buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            &self.cached_layout.split_areas
        {
            if col < content_rect.x
                || col >= content_rect.x + content_rect.width
                || row < content_rect.y
                || row >= content_rect.y + content_rect.height
            {
                continue;
            }

            if self.is_terminal_buffer(*buffer_id) || self.is_composite_buffer(*buffer_id) {
                continue;
            }

            let (gutter_width, collapsed_header_bytes) = {
                let state = self.buffers.get(buffer_id)?;
                let headers = self
                    .split_view_states
                    .get(split_id)
                    .map(|vs| {
                        vs.folds
                            .collapsed_header_bytes(&state.buffer, &state.marker_list)
                    })
                    .unwrap_or_default();
                (state.margins.left_total_width() as u16, headers)
            };

            let cached_mappings = self.cached_layout.view_line_mappings.get(split_id).cloned();
            let fallback = self
                .split_view_states
                .get(split_id)
                .map(|vs| vs.viewport.top_byte)
                .unwrap_or(0);
            let compose_width = self
                .split_view_states
                .get(split_id)
                .and_then(|vs| vs.compose_width);

            let target_position = super::click_geometry::screen_to_buffer_position(
                col,
                row,
                *content_rect,
                gutter_width,
                &cached_mappings,
                fallback,
                true,
                compose_width,
            )?;

            let adjusted_rect = super::click_geometry::adjust_content_rect_for_compose(*content_rect, compose_width);
            let content_col = col.saturating_sub(adjusted_rect.x);
            let state = self.buffers.get(buffer_id)?;
            if let Some(byte_pos) = super::click_geometry::fold_toggle_byte_from_position(
                state,
                &collapsed_header_bytes,
                target_position,
                content_col,
                gutter_width,
            ) {
                return Some((*buffer_id, byte_pos));
            }
        }

        None
    }

    /// Handle click in editor content area
    pub(super) fn handle_editor_click(
        &mut self,
        col: u16,
        row: u16,
        split_id: crate::model::event::LeafId,
        buffer_id: BufferId,
        content_rect: ratatui::layout::Rect,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        use crate::model::event::{CursorId, Event};
        use crossterm::event::KeyModifiers;
        // Build modifiers string for plugins
        let modifiers_str = if modifiers.contains(KeyModifiers::SHIFT) {
            "shift".to_string()
        } else {
            String::new()
        };

        // Dispatch MouseClick hook to plugins
        // Plugins can handle clicks on their virtual buffers
        if self.plugin_manager.has_hook_handlers("mouse_click") {
            // Compute buffer-local row/col so plugins can react to clicks
            // on specific rows (e.g. clicking a file header, or a comment
            // in the comments navigation panel) without redoing the math.
            let (hook_buffer_row, hook_buffer_col) = {
                let cached_mappings = self
                    .cached_layout
                    .view_line_mappings
                    .get(&split_id)
                    .cloned();
                let fallback = self
                    .split_view_states
                    .get(&split_id)
                    .map(|vs| vs.viewport.top_byte)
                    .unwrap_or(0);
                let compose_width = self
                    .split_view_states
                    .get(&split_id)
                    .and_then(|vs| vs.compose_width);
                let gutter_width = self
                    .buffers
                    .get(&buffer_id)
                    .map(|s| s.margins.left_total_width() as u16)
                    .unwrap_or(0);
                let target = super::click_geometry::screen_to_buffer_position(
                    col,
                    row,
                    content_rect,
                    gutter_width,
                    &cached_mappings,
                    fallback,
                    true,
                    compose_width,
                );
                match target {
                    Some(byte_pos) => {
                        let state = self.buffers.get(&buffer_id);
                        if let Some(s) = state {
                            let (line, col_b) = s.buffer.position_to_line_col(byte_pos);
                            (
                                Some(line.min(u32::MAX as usize) as u32),
                                Some(col_b.min(u32::MAX as usize) as u32),
                            )
                        } else {
                            (None, None)
                        }
                    }
                    None => (None, None),
                }
            };
            self.plugin_manager.run_hook(
                "mouse_click",
                HookArgs::MouseClick {
                    column: col,
                    row,
                    button: "left".to_string(),
                    modifiers: modifiers_str,
                    content_x: content_rect.x,
                    content_y: content_rect.y,
                    buffer_id: Some(buffer_id.0 as u64),
                    buffer_row: hook_buffer_row,
                    buffer_col: hook_buffer_col,
                },
            );
        }

        // Fixed buffer-group panels (toolbars/headers/footers) aren't
        // interactive targets: focusing them would let arrow keys move an
        // invisible cursor and scroll the pinned content. Swallow the click
        // after the plugin hook has had a chance to observe it. Scrollable
        // group panels still accept the click (focus routes to them) even
        // when their cursor is hidden.
        if self.is_non_scrollable_buffer(buffer_id) {
            return Ok(());
        }

        // Focus this split (handles terminal mode exit, tab state, etc.)
        self.focus_split(split_id, buffer_id);

        // Handle composite buffer clicks specially
        if self.is_composite_buffer(buffer_id) {
            return self.handle_composite_click(col, row, split_id, buffer_id, content_rect);
        }

        // Ensure key context is Normal for non-terminal buffers
        // This handles the edge case where split/buffer don't change but we clicked from FileExplorer
        if !self.is_terminal_buffer(buffer_id) {
            self.key_context = crate::input::keybindings::KeyContext::Normal;
        }

        // Get cached view line mappings for this split (before mutable borrow of buffers)
        let cached_mappings = self
            .cached_layout
            .view_line_mappings
            .get(&split_id)
            .cloned();

        // Get fallback from SplitViewState viewport
        let fallback = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.top_byte)
            .unwrap_or(0);

        // Get compose width for this split (adjusts content rect for centered layout)
        let compose_width = self
            .split_view_states
            .get(&split_id)
            .and_then(|vs| vs.compose_width);

        // Calculate clicked position in buffer
        let (toggle_fold_byte, onclick_action, target_position, cursor_snapshot) =
            if let Some(state) = self.buffers.get(&buffer_id) {
                let gutter_width = state.margins.left_total_width() as u16;

                let Some(target_position) = super::click_geometry::screen_to_buffer_position(
                    col,
                    row,
                    content_rect,
                    gutter_width,
                    &cached_mappings,
                    fallback,
                    true, // Allow gutter clicks - position cursor at start of line
                    compose_width,
                ) else {
                    return Ok(());
                };

                // Toggle fold on gutter click if this line is foldable/collapsed
                let adjusted_rect =
                    super::click_geometry::adjust_content_rect_for_compose(content_rect, compose_width);
                let content_col = col.saturating_sub(adjusted_rect.x);
                let collapsed_header_bytes = self
                    .split_view_states
                    .get(&split_id)
                    .map(|vs| {
                        vs.folds
                            .collapsed_header_bytes(&state.buffer, &state.marker_list)
                    })
                    .unwrap_or_default();
                let toggle_fold_byte = super::click_geometry::fold_toggle_byte_from_position(
                    state,
                    &collapsed_header_bytes,
                    target_position,
                    content_col,
                    gutter_width,
                );

                let cursor_snapshot = self
                    .split_view_states
                    .get(&split_id)
                    .map(|vs| {
                        let cursor = vs.cursors.primary();
                        (
                            vs.cursors.primary_id(),
                            cursor.position,
                            cursor.anchor,
                            cursor.sticky_column,
                            cursor.deselect_on_move,
                        )
                    })
                    .unwrap_or((CursorId(0), 0, None, 0, true));

                // Check for onClick text property at this position
                // This enables clickable UI elements in virtual buffers
                let onclick_action = state
                    .text_properties
                    .get_at(target_position)
                    .iter()
                    .find_map(|prop| {
                        prop.get("onClick")
                            .and_then(|v| v.as_str())
                            .map(|s| s.to_string())
                    });

                (
                    toggle_fold_byte,
                    onclick_action,
                    target_position,
                    cursor_snapshot,
                )
            } else {
                return Ok(());
            };

        if toggle_fold_byte.is_some() {
            self.toggle_fold_at_byte(buffer_id, target_position);
            return Ok(());
        }

        let (primary_cursor_id, old_position, old_anchor, old_sticky_column, deselect_on_move) =
            cursor_snapshot;

        if let Some(action_name) = onclick_action {
            // Execute the action associated with this clickable element
            tracing::debug!(
                "onClick triggered at position {}: action={}",
                target_position,
                action_name
            );
            let empty_args = std::collections::HashMap::new();
            if let Some(action) = Action::from_str(&action_name, &empty_args) {
                return self.handle_action(action);
            }
            return Ok(());
        }

        // Move cursor to clicked position (respect shift for selection)
        // Both modifiers supported since some terminals intercept shift+click.
        let extend_selection =
            modifiers.contains(KeyModifiers::SHIFT) || modifiers.contains(KeyModifiers::CONTROL);
        let new_anchor = if extend_selection {
            Some(old_anchor.unwrap_or(old_position))
        } else if deselect_on_move {
            None
        } else {
            old_anchor
        };

        let new_sticky_column = self
            .buffers
            .get(&buffer_id)
            .and_then(|state| state.buffer.offset_to_position(target_position))
            .map(|pos| pos.column)
            .unwrap_or(0);

        let event = Event::MoveCursor {
            cursor_id: primary_cursor_id,
            old_position,
            new_position: target_position,
            old_anchor,
            new_anchor,
            old_sticky_column,
            new_sticky_column,
        };

        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
        self.track_cursor_movement(&event);

        // Start text selection drag for potential mouse drag
        self.mouse_state.dragging_text_selection = true;
        self.mouse_state.drag_selection_split = Some(split_id);
        self.mouse_state.drag_selection_anchor = Some(new_anchor.unwrap_or(target_position));

        Ok(())
    }

    /// Handle click in file explorer
    pub(super) fn handle_file_explorer_click(
        &mut self,
        col: u16,
        row: u16,
        explorer_area: ratatui::layout::Rect,
    ) -> AnyhowResult<()> {
        // Check if click is on the title bar (first row)
        if row == explorer_area.y {
            // Check if click is on close button (× at right side of title bar)
            // Close button is at position: explorer_area.x + explorer_area.width - 3 to -1
            let close_button_x = explorer_area.x + explorer_area.width.saturating_sub(3);
            if col >= close_button_x && col < explorer_area.x + explorer_area.width {
                self.toggle_file_explorer();
                return Ok(());
            }
        }

        // Focus file explorer
        self.key_context = crate::input::keybindings::KeyContext::FileExplorer;

        // Calculate which item was clicked (accounting for border and title)
        // The file explorer has a 1-line border at top and bottom
        let relative_row = row.saturating_sub(explorer_area.y + 1); // +1 for top border

        if let Some(ref mut explorer) = self.file_explorer {
            let display_nodes = explorer.get_display_nodes();
            let scroll_offset = explorer.get_scroll_offset();
            let clicked_index = (relative_row as usize) + scroll_offset;

            if clicked_index < display_nodes.len() {
                let (node_id, _indent) = display_nodes[clicked_index];

                // Select this node
                explorer.set_selected(Some(node_id));

                // Check if it's a file or directory
                let node = explorer.tree().get_node(node_id);
                if let Some(node) = node {
                    if node.is_dir() {
                        // Toggle expand/collapse using the existing method
                        self.file_explorer_toggle_expand();
                    } else if node.is_file() {
                        // Open the file but keep focus on file explorer (single click).
                        // Double-click or Enter will focus the editor and promote to
                        // a permanent tab. Single-click opens in "preview" mode so a
                        // string of exploratory clicks doesn't accumulate tabs.
                        let path = node.entry.path.clone();
                        let name = node.entry.name.clone();
                        match self.open_file_preview(&path) {
                            Ok(_) => {
                                self.set_status_message(
                                    rust_i18n::t!("explorer.opened_file", name = &name).to_string(),
                                );
                            }
                            Err(e) => {
                                // Check if this is a large file encoding confirmation error
                                if let Some(confirmation) = e.downcast_ref::<
                                    crate::model::buffer::LargeFileEncodingConfirmation,
                                >() {
                                    self.start_large_file_encoding_confirmation(confirmation);
                                } else {
                                    self.set_status_message(
                                        rust_i18n::t!("file.error_opening", error = e.to_string())
                                            .to_string(),
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }


    /// Switch to the previously active tab in the current split.
    /// Handles both buffer tabs and group tabs via the focus-history LRU.
    fn switch_to_previous_tab(&mut self) {
        use crate::view::split::TabTarget;
        let active_split = self.split_manager.active_split();
        let previous_tab = self
            .split_view_states
            .get(&active_split)
            .and_then(|vs| vs.previous_tab());

        match previous_tab {
            Some(TabTarget::Buffer(prev_id)) => {
                let is_valid = self
                    .split_view_states
                    .get(&active_split)
                    .is_some_and(|vs| vs.has_buffer(prev_id));

                if is_valid && prev_id != self.active_buffer() {
                    self.position_history.commit_pending_movement();
                    let cursors = self.active_cursors();
                    let position = cursors.primary().position;
                    let anchor = cursors.primary().anchor;
                    self.position_history
                        .record_movement(self.active_buffer(), position, anchor);
                    self.position_history.commit_pending_movement();
                    self.set_active_buffer(prev_id);
                } else if !is_valid {
                    self.set_status_message(t!("status.previous_tab_closed").to_string());
                }
            }
            Some(TabTarget::Group(leaf_id)) => {
                if self.grouped_subtrees.contains_key(&leaf_id) {
                    self.activate_group_tab(leaf_id);
                } else {
                    self.set_status_message(t!("status.previous_tab_closed").to_string());
                }
            }
            None => {
                self.set_status_message(t!("status.no_previous_tab").to_string());
            }
        }
    }

    /// Start the switch-to-tab-by-name prompt with suggestions from open buffers
    fn start_switch_to_tab_prompt(&mut self) {
        let active_split = self.split_manager.active_split();
        let open_buffers: Vec<BufferId> =
            if let Some(view_state) = self.split_view_states.get(&active_split) {
                view_state.buffer_tab_ids_vec()
            } else {
                return;
            };

        if open_buffers.is_empty() {
            self.set_status_message(t!("status.no_tabs_in_split").to_string());
            return;
        }

        // Find the current buffer's index
        let current_index = open_buffers
            .iter()
            .position(|&id| id == self.active_buffer())
            .unwrap_or(0);

        let suggestions: Vec<crate::input::commands::Suggestion> = open_buffers
            .iter()
            .map(|&buffer_id| {
                let display_name = self
                    .buffer_metadata
                    .get(&buffer_id)
                    .map(|m| m.display_name.clone())
                    .unwrap_or_else(|| format!("Buffer {:?}", buffer_id));

                let is_current = buffer_id == self.active_buffer();
                let is_modified = self
                    .buffers
                    .get(&buffer_id)
                    .is_some_and(|b| b.buffer.is_modified());

                let description = match (is_current, is_modified) {
                    (true, true) => Some("(current, modified)".to_string()),
                    (true, false) => Some("(current)".to_string()),
                    (false, true) => Some("(modified)".to_string()),
                    (false, false) => None,
                };

                crate::input::commands::Suggestion {
                    text: display_name,
                    description,
                    value: Some(buffer_id.0.to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Switch to tab: ".to_string(),
            PromptType::SwitchToTab,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
            }
        }
    }

    /// Switch to a tab by its BufferId
    pub(crate) fn switch_to_tab(&mut self, buffer_id: BufferId) {
        // Verify the buffer exists and is open in the current split
        let active_split = self.split_manager.active_split();
        let is_valid = self
            .split_view_states
            .get(&active_split)
            .is_some_and(|vs| vs.has_buffer(buffer_id));

        if !is_valid {
            self.set_status_message(t!("status.tab_not_found").to_string());
            return;
        }

        if buffer_id != self.active_buffer() {
            // Save current position before switching
            self.position_history.commit_pending_movement();

            let cursors = self.active_cursors();
            let position = cursors.primary().position;
            let anchor = cursors.primary().anchor;
            self.position_history
                .record_movement(self.active_buffer(), position, anchor);
            self.position_history.commit_pending_movement();

            self.set_active_buffer(buffer_id);
        }
    }

    /// Handle character insertion in prompt mode.
    fn handle_insert_char_prompt(&mut self, c: char) -> AnyhowResult<()> {
        // Check if this is the query-replace confirmation prompt
        if let Some(ref prompt) = self.prompt {
            if prompt.prompt_type == PromptType::QueryReplaceConfirm {
                return self.handle_interactive_replace_key(c);
            }
        }

        // Reset history navigation when user starts typing
        // This allows them to press Up to get back to history items
        // Reset history navigation when typing in a prompt
        if let Some(ref prompt) = self.prompt {
            if let Some(key) = Self::prompt_type_to_history_key(&prompt.prompt_type) {
                if let Some(history) = self.prompt_histories.get_mut(&key) {
                    history.reset_navigation();
                }
            }
        }

        if let Some(prompt) = self.prompt_mut() {
            // Use insert_str to properly handle selection deletion
            let s = c.to_string();
            prompt.insert_str(&s);
        }
        self.update_prompt_suggestions();
        Ok(())
    }

    /// Handle character insertion in normal editor mode.
    fn handle_insert_char_editor(&mut self, c: char) -> AnyhowResult<()> {
        // Check if editing is disabled (show_cursors = false)
        if self.is_editing_disabled() {
            self.set_status_message(t!("buffer.editing_disabled").to_string());
            return Ok(());
        }

        // Cancel any pending LSP requests since the text is changing
        self.cancel_pending_lsp_requests();

        if let Some(events) = self.action_to_events(Action::InsertChar(c)) {
            if events.len() > 1 {
                // Multi-cursor: use optimized bulk edit (O(n) instead of O(n²))
                let description = format!("Insert '{}'", c);
                if let Some(bulk_edit) = self.apply_events_as_bulk_edit(events, description.clone())
                {
                    self.active_event_log_mut().append(bulk_edit);
                }
            } else {
                // Single cursor - apply normally
                for event in events {
                    self.active_event_log_mut().append(event.clone());
                    self.apply_event_to_active_buffer(&event);
                }
            }
        }

        // Auto-trigger signature help on '(' and ','
        if c == '(' || c == ',' {
            self.request_signature_help();
        }

        // Auto-trigger completion on trigger characters
        self.maybe_trigger_completion(c);

        Ok(())
    }

    /// Apply an action by converting it to events.
    ///
    /// This is the catch-all handler for actions that can be converted to buffer events
    /// (cursor movements, text edits, etc.). It handles batching for multi-cursor,
    /// position history tracking, and editing permission checks.
    fn apply_action_as_events(&mut self, action: Action) -> AnyhowResult<()> {
        // Check if active buffer is a composite buffer - handle scroll/movement specially
        let buffer_id = self.active_buffer();
        if self.is_composite_buffer(buffer_id) {
            if let Some(_handled) = self.handle_composite_action(buffer_id, &action) {
                return Ok(());
            }
        }

        // Get description before moving action
        let action_description = format!("{:?}", action);

        // Check if this is an editing action and editing is disabled
        let is_editing_action = matches!(
            action,
            Action::InsertNewline
                | Action::InsertTab
                | Action::DeleteForward
                | Action::DeleteWordBackward
                | Action::DeleteWordForward
                | Action::DeleteLine
                | Action::DuplicateLine
                | Action::MoveLineUp
                | Action::MoveLineDown
                | Action::DedentSelection
                | Action::ToggleComment
        );

        if is_editing_action && self.is_editing_disabled() {
            self.set_status_message(t!("buffer.editing_disabled").to_string());
            return Ok(());
        }

        if let Some(events) = self.action_to_events(action) {
            if events.len() > 1 {
                // Check if this batch contains buffer modifications
                let has_buffer_mods = events
                    .iter()
                    .any(|e| matches!(e, Event::Insert { .. } | Event::Delete { .. }));

                if has_buffer_mods {
                    // Multi-cursor buffer edit: use optimized bulk edit (O(n) instead of O(n²))
                    if let Some(bulk_edit) =
                        self.apply_events_as_bulk_edit(events.clone(), action_description)
                    {
                        self.active_event_log_mut().append(bulk_edit);
                    }
                } else {
                    // Multi-cursor non-buffer operation: use Batch for atomic undo
                    let batch = Event::Batch {
                        events: events.clone(),
                        description: action_description,
                    };
                    self.active_event_log_mut().append(batch.clone());
                    self.apply_event_to_active_buffer(&batch);
                }

                // Track position history for all events
                for event in &events {
                    self.track_cursor_movement(event);
                }
            } else {
                // Single cursor - apply normally
                for event in events {
                    self.log_and_apply_event(&event);
                    self.track_cursor_movement(&event);
                }
            }
        }

        Ok(())
    }

    /// Track cursor movement in position history if applicable.
    pub(super) fn track_cursor_movement(&mut self, event: &Event) {
        if self.in_navigation {
            return;
        }

        if let Event::MoveCursor {
            new_position,
            new_anchor,
            ..
        } = event
        {
            self.position_history
                .record_movement(self.active_buffer(), *new_position, *new_anchor);
        }
    }

    /// Route a key event through the CompositeInputRouter for a composite
    /// buffer.  Returns `Some(Ok(()))` if the event was handled (or blocked),
    /// `None` if the router returned `Unhandled` (let fallthrough continue).
    fn try_route_composite_key(
        &mut self,
        split_id: crate::model::event::LeafId,
        buffer_id: crate::model::event::BufferId,
        key_event: &crossterm::event::KeyEvent,
    ) -> Option<AnyhowResult<()>> {
        use crate::input::composite_router::{
            CompositeInputRouter, Direction, RoutedEvent, ScrollAction,
        };

        let composite = self.composite_buffers.get(&buffer_id)?;
        let view_state = self.composite_view_states.get(&(split_id, buffer_id))?;

        match CompositeInputRouter::route_key_event(composite, view_state, key_event) {
            RoutedEvent::Unhandled => None,

            RoutedEvent::CompositeScroll(action) => {
                let delta = match action {
                    ScrollAction::Up(n) => -(n as isize),
                    ScrollAction::Down(n) => n as isize,
                    _ => return Some(Ok(())),
                };
                self.composite_scroll(split_id, buffer_id, delta);
                Some(Ok(()))
            }

            RoutedEvent::SwitchPane(dir) => {
                match dir {
                    Direction::Next => self.composite_focus_next(split_id, buffer_id),
                    Direction::Prev => self.composite_focus_prev(split_id, buffer_id),
                }
                Some(Ok(()))
            }

            // Anything else the router might return — let normal dispatch handle it
            _ => None,
        }
    }
}
