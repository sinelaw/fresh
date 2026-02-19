use super::*;
use crate::model::event::{CursorId, LeafId};
use crate::services::plugins::hooks::HookArgs;
use anyhow::Result as AnyhowResult;
use rust_i18n::t;
impl Editor {
    /// Determine the current keybinding context based on UI state
    pub fn get_key_context(&self) -> crate::input::keybindings::KeyContext {
        use crate::input::keybindings::KeyContext;

        // Priority order: Settings > Menu > Prompt > Popup > Rename > Current context (FileExplorer or Normal)
        if self.settings_state.as_ref().is_some_and(|s| s.visible) {
            KeyContext::Settings
        } else if self.menu_state.active_menu.is_some() {
            KeyContext::Menu
        } else if self.is_prompting() {
            KeyContext::Prompt
        } else if self.active_state().popups.is_visible() {
            KeyContext::Popup
        } else {
            // Use the current context (can be FileExplorer or Normal)
            self.key_context
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

        // Try terminal input dispatch first (handles terminal mode and re-entry)
        if self.dispatch_terminal_input(&key_event).is_some() {
            return Ok(());
        }

        // Clear skip_ensure_visible flag so cursor becomes visible after key press
        // (scroll actions will set it again if needed)
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.viewport.clear_skip_ensure_visible();
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

        // Only check buffer mode keybindings if we're not in a higher-priority context
        // (Menu, Prompt, Popup should take precedence over mode bindings)
        let should_check_mode_bindings = matches!(
            context,
            crate::input::keybindings::KeyContext::Normal
                | crate::input::keybindings::KeyContext::FileExplorer
        );

        if should_check_mode_bindings {
            // If we're in a global editor mode, handle chords and keybindings
            if let Some(ref mode_name) = self.editor_mode {
                // First, try to resolve as a chord (multi-key sequence like "gg")
                if let Some(action_name) = self.mode_registry.resolve_chord_keybinding(
                    mode_name,
                    &self.chord_state,
                    code,
                    modifiers,
                ) {
                    tracing::debug!("Mode chord resolved to action: {}", action_name);
                    self.chord_state.clear();
                    let action = Action::from_str(&action_name, &std::collections::HashMap::new())
                        .unwrap_or(Action::PluginAction(action_name));
                    return self.handle_action(action);
                }

                // Check if this could be the start of a chord sequence
                let is_potential_chord = self.mode_registry.is_chord_prefix(
                    mode_name,
                    &self.chord_state,
                    code,
                    modifiers,
                );

                if is_potential_chord {
                    // This could be the start of a chord - add to state and wait
                    tracing::debug!("Potential chord prefix in editor mode");
                    self.chord_state.push((code, modifiers));
                    return Ok(());
                }

                // Not a chord - clear any pending chord state
                if !self.chord_state.is_empty() {
                    tracing::debug!("Chord sequence abandoned in mode, clearing state");
                    self.chord_state.clear();
                }
            }

            // Check buffer mode keybindings (for virtual buffers with custom modes)
            // Mode keybindings resolve to Action names (see Action::from_str)
            if let Some(action_name) = self.resolve_mode_keybinding(code, modifiers) {
                let action = Action::from_str(&action_name, &std::collections::HashMap::new())
                    .unwrap_or_else(|| Action::PluginAction(action_name.clone()));
                return self.handle_action(action);
            }

            // If we're in a global editor mode, check if we should block unbound keys
            if let Some(ref mode_name) = self.editor_mode {
                // Check if this mode is read-only
                // read_only=true (like vi-normal): unbound keys should be ignored
                // read_only=false (like vi-insert): unbound keys should insert characters
                if self.mode_registry.is_read_only(mode_name) {
                    tracing::debug!(
                        "Ignoring unbound key in read-only mode {:?}",
                        self.editor_mode
                    );
                    return Ok(());
                }
                // Mode is not read-only, fall through to normal key handling
                tracing::debug!(
                    "Mode {:?} is not read-only, allowing key through",
                    self.editor_mode
                );
            }
        }

        // Check for chord sequence matches first
        let key_event = crossterm::event::KeyEvent::new(code, modifiers);
        let chord_result = self
            .keybindings
            .resolve_chord(&self.chord_state, &key_event, context);

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

        // Regular single-key resolution
        let action = self.keybindings.resolve(&key_event, context);

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

    /// Handle an action (for normal mode and command execution)
    pub(super) fn handle_action(&mut self, action: Action) -> AnyhowResult<()> {
        use crate::input::keybindings::Action;

        // Record action to macro if recording
        self.record_macro_action(&action);

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
                } else {
                    self.save()?;
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
            Action::GotoLine => self.start_prompt(
                t!("file.goto_line_prompt").to_string(),
                PromptType::GotoLine,
            ),
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
                // Toggle command palette: close if already open, otherwise open it
                if let Some(prompt) = &self.prompt {
                    if prompt.prompt_type == PromptType::Command {
                        self.cancel_prompt();
                        return Ok(());
                    }
                }

                // Use the current context for filtering commands
                let active_buffer_mode = self
                    .buffer_metadata
                    .get(&self.active_buffer())
                    .and_then(|m| m.virtual_mode());
                let suggestions = self.command_registry.read().unwrap().filter(
                    "",
                    self.key_context,
                    &self.keybindings,
                    self.has_active_selection(),
                    &self.active_custom_contexts,
                    active_buffer_mode,
                );
                self.start_prompt_with_suggestions(
                    t!("file.command_prompt").to_string(),
                    PromptType::Command,
                    suggestions,
                );
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

                // Update all viewports to reflect the new line wrap setting
                for view_state in self.split_view_states.values_mut() {
                    view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
                }

                let state = if self.config.editor.line_wrap {
                    t!("view.state_enabled").to_string()
                } else {
                    t!("view.state_disabled").to_string()
                };
                self.set_status_message(t!("view.line_wrap_state", state = state).to_string());
            }
            Action::ToggleComposeMode => {
                self.handle_toggle_compose_mode();
            }
            Action::SetComposeWidth => {
                let active_split = self.split_manager.active_split();
                let current = self
                    .split_view_states
                    .get(&active_split)
                    .and_then(|v| v.compose_width.map(|w| w.to_string()))
                    .unwrap_or_default();
                self.start_prompt_with_initial_text(
                    "Compose width (empty = viewport): ".to_string(),
                    PromptType::SetComposeWidth,
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
            Action::ToggleTabIndicators => {
                if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
                    state.buffer_settings.show_whitespace_tabs =
                        !state.buffer_settings.show_whitespace_tabs;
                    let status = if state.buffer_settings.show_whitespace_tabs {
                        "Tab indicators: Visible"
                    } else {
                        "Tab indicators: Hidden"
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
                        let primary = *view_state.cursors.primary();
                        view_state
                            .viewport
                            .ensure_visible(&mut state.buffer, &primary);
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
                self.handle_menu_open(&menu_name);
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
                    self.keybindings =
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
                self.smart_home();
            }
            Action::ToggleComment => {
                self.toggle_comment();
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
                if let Some(key) = self.last_macro_register {
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

    /// Handle mouse wheel scroll event
    pub(super) fn handle_mouse_scroll(
        &mut self,
        col: u16,
        row: u16,
        delta: i32,
    ) -> AnyhowResult<()> {
        // Sync viewport from EditorState to SplitViewState before scrolling.
        // This is necessary because rendering updates EditorState.viewport via ensure_visible,
        // but that change isn't automatically synced to SplitViewState. Without this sync,
        // mouse scroll would use a stale viewport position after keyboard navigation.
        // (Bug #248: Mouse wheel stopped working properly after keyboard use)

        // Check if scroll is over the file explorer
        if let Some(explorer_area) = self.cached_layout.file_explorer_area {
            if col >= explorer_area.x
                && col < explorer_area.x + explorer_area.width
                && row >= explorer_area.y
                && row < explorer_area.y + explorer_area.height
            {
                // Scroll the file explorer
                if let Some(explorer) = &mut self.file_explorer {
                    let visible = explorer.tree().get_visible_nodes();
                    if visible.is_empty() {
                        return Ok(());
                    }

                    // Get current selected index
                    let current_index = explorer.get_selected_index().unwrap_or(0);

                    // Calculate new index based on scroll delta
                    let new_index = if delta < 0 {
                        // Scroll up (negative delta)
                        current_index.saturating_sub(delta.unsigned_abs() as usize)
                    } else {
                        // Scroll down (positive delta)
                        (current_index + delta as usize).min(visible.len() - 1)
                    };

                    // Set the new selection
                    if let Some(node_id) = explorer.get_node_at_index(new_index) {
                        explorer.set_selected(Some(node_id));
                        explorer.update_scroll_for_selection();
                    }
                }
                return Ok(());
            }
        }

        // Otherwise, scroll the editor in the active split
        // Use SplitViewState's viewport (View events go to SplitViewState, not EditorState)
        let active_split = self.split_manager.active_split();
        let buffer_id = self.active_buffer();

        // Check if this is a composite buffer - if so, use composite scroll
        if self.is_composite_buffer(buffer_id) {
            let max_row = self
                .composite_buffers
                .get(&buffer_id)
                .map(|c| c.row_count().saturating_sub(1))
                .unwrap_or(0);
            if let Some(view_state) = self
                .composite_view_states
                .get_mut(&(active_split.into(), buffer_id))
            {
                view_state.scroll(delta as isize, max_row);
                tracing::trace!(
                    "handle_mouse_scroll (composite): delta={}, scroll_row={}",
                    delta,
                    view_state.scroll_row
                );
            }
            return Ok(());
        }

        // Get view_transform tokens from SplitViewState (if any)
        let view_transform_tokens = self
            .split_view_states
            .get(&active_split)
            .and_then(|vs| vs.view_transform.as_ref())
            .map(|vt| vt.tokens.clone());

        // Get mutable references to both buffer and view state
        let buffer = self.buffers.get_mut(&buffer_id).map(|s| &mut s.buffer);
        let view_state = self.split_view_states.get_mut(&active_split);

        if let (Some(buffer), Some(view_state)) = (buffer, view_state) {
            let top_byte_before = view_state.viewport.top_byte;
            if let Some(tokens) = view_transform_tokens {
                // Use view-aware scrolling with the transform's tokens
                use crate::view::ui::view_pipeline::ViewLineIterator;
                let tab_size = self.config.editor.tab_size;
                let view_lines: Vec<_> =
                    ViewLineIterator::new(&tokens, false, false, tab_size).collect();
                view_state
                    .viewport
                    .scroll_view_lines(&view_lines, delta as isize);
            } else {
                // No view transform - use traditional buffer-based scrolling
                if delta < 0 {
                    // Scroll up
                    let lines_to_scroll = delta.unsigned_abs() as usize;
                    view_state.viewport.scroll_up(buffer, lines_to_scroll);
                } else {
                    // Scroll down
                    let lines_to_scroll = delta as usize;
                    view_state.viewport.scroll_down(buffer, lines_to_scroll);
                }
            }
            // Skip ensure_visible so the scroll position isn't undone during render
            view_state.viewport.set_skip_ensure_visible();
            tracing::trace!(
                "handle_mouse_scroll: delta={}, top_byte {} -> {}",
                delta,
                top_byte_before,
                view_state.viewport.top_byte
            );
        }

        Ok(())
    }

    /// Handle horizontal scroll (Shift+ScrollWheel or native ScrollLeft/ScrollRight)
    pub(super) fn handle_horizontal_scroll(
        &mut self,
        _col: u16,
        _row: u16,
        delta: i32,
    ) -> AnyhowResult<()> {
        let active_split = self.split_manager.active_split();

        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            // Don't scroll horizontally when line wrap is enabled
            if view_state.viewport.line_wrap_enabled {
                return Ok(());
            }

            let columns_to_scroll = delta.unsigned_abs() as usize;
            if delta < 0 {
                // Scroll left
                view_state.viewport.left_column = view_state
                    .viewport
                    .left_column
                    .saturating_sub(columns_to_scroll);
            } else {
                // Scroll right - clamp to max_line_length_seen
                let visible_width = view_state.viewport.width as usize;
                let max_scroll = view_state
                    .viewport
                    .max_line_length_seen
                    .saturating_sub(visible_width);
                let new_left = view_state
                    .viewport
                    .left_column
                    .saturating_add(columns_to_scroll);
                view_state.viewport.left_column = new_left.min(max_scroll);
            }
            // Skip ensure_visible so the scroll position isn't undone during render
            view_state.viewport.set_skip_ensure_visible();
        }

        Ok(())
    }

    /// Handle scrollbar drag with relative movement (when dragging from thumb)
    pub(super) fn handle_scrollbar_drag_relative(
        &mut self,
        row: u16,
        split_id: LeafId,
        buffer_id: BufferId,
        scrollbar_rect: ratatui::layout::Rect,
    ) -> AnyhowResult<()> {
        let drag_start_row = match self.mouse_state.drag_start_row {
            Some(r) => r,
            None => return Ok(()), // No drag start, shouldn't happen
        };

        let drag_start_top_byte = match self.mouse_state.drag_start_top_byte {
            Some(b) => b,
            None => return Ok(()), // No drag start, shouldn't happen
        };

        let drag_start_view_line_offset = self.mouse_state.drag_start_view_line_offset.unwrap_or(0);

        // Calculate the offset in rows (still used for large files)
        let row_offset = (row as i32) - (drag_start_row as i32);

        // Get viewport height from SplitViewState
        let viewport_height = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.height as usize)
            .unwrap_or(10);

        // Check if line wrapping is enabled
        let line_wrap_enabled = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.line_wrap_enabled)
            .unwrap_or(false);

        let viewport_width = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.width as usize)
            .unwrap_or(80);

        // Get the buffer state and calculate target position using RELATIVE movement
        // Returns (byte_position, view_line_offset) for proper positioning within wrapped lines
        let scroll_position = if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let scrollbar_height = scrollbar_rect.height as usize;
            if scrollbar_height == 0 {
                return Ok(());
            }

            let buffer_len = state.buffer.len();
            let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;

            // Use relative movement: calculate scroll change based on row_offset from drag start
            if buffer_len <= large_file_threshold {
                // When line wrapping is enabled, use visual row calculations
                if line_wrap_enabled {
                    Self::calculate_scrollbar_drag_relative_visual(
                        &mut state.buffer,
                        row,
                        scrollbar_rect.y,
                        scrollbar_height,
                        drag_start_row,
                        drag_start_top_byte,
                        drag_start_view_line_offset,
                        viewport_height,
                        viewport_width,
                    )
                } else {
                    // Small file without line wrap: thumb follows mouse
                    let total_lines = if buffer_len > 0 {
                        state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1
                    } else {
                        1
                    };

                    let max_scroll_line = total_lines.saturating_sub(viewport_height);

                    if max_scroll_line == 0 || scrollbar_height <= 1 {
                        // File fits in viewport, no scrolling
                        (0, 0)
                    } else {
                        // Find the starting line number from drag_start_top_byte
                        let start_line = state.buffer.get_line_number(drag_start_top_byte);

                        // Calculate thumb size (same formula as scrollbar rendering)
                        let thumb_size_raw = (viewport_height as f64 / total_lines as f64
                            * scrollbar_height as f64)
                            .ceil() as usize;
                        let max_thumb_size = (scrollbar_height as f64 * 0.8).floor() as usize;
                        let thumb_size = thumb_size_raw
                            .max(1)
                            .min(max_thumb_size)
                            .min(scrollbar_height);

                        // Calculate max thumb start position (same as scrollbar rendering)
                        let max_thumb_start = scrollbar_height.saturating_sub(thumb_size);

                        if max_thumb_start == 0 {
                            // Thumb fills the track, no dragging possible
                            (0, 0)
                        } else {
                            // Calculate where the thumb was at drag start
                            let start_scroll_ratio =
                                start_line.min(max_scroll_line) as f64 / max_scroll_line as f64;
                            let thumb_row_at_start = scrollbar_rect.y as f64
                                + start_scroll_ratio * max_thumb_start as f64;

                            // Calculate click offset (where on thumb we clicked)
                            let click_offset = drag_start_row as f64 - thumb_row_at_start;

                            // Target thumb position based on current mouse position
                            let target_thumb_row = row as f64 - click_offset;

                            // Map target thumb position to scroll ratio
                            let target_scroll_ratio = ((target_thumb_row
                                - scrollbar_rect.y as f64)
                                / max_thumb_start as f64)
                                .clamp(0.0, 1.0);

                            // Map scroll ratio to target line
                            let target_line =
                                (target_scroll_ratio * max_scroll_line as f64).round() as usize;
                            let target_line = target_line.min(max_scroll_line);

                            // Find byte position of target line
                            let target_byte = state
                                .buffer
                                .line_start_offset(target_line)
                                .unwrap_or(drag_start_top_byte);

                            (target_byte, 0)
                        }
                    }
                }
            } else {
                // Large file: use byte-based relative movement
                let bytes_per_pixel = buffer_len as f64 / scrollbar_height as f64;
                let byte_offset = (row_offset as f64 * bytes_per_pixel) as i64;

                let new_top_byte = if byte_offset >= 0 {
                    drag_start_top_byte.saturating_add(byte_offset as usize)
                } else {
                    drag_start_top_byte.saturating_sub((-byte_offset) as usize)
                };

                // Clamp to valid range using byte-based max (avoid iterating entire buffer)
                let new_top_byte = new_top_byte.min(buffer_len.saturating_sub(1));

                // Find the line start for this byte position
                let iter = state.buffer.line_iterator(new_top_byte, 80);
                (iter.current_position(), 0)
            }
        } else {
            return Ok(());
        };

        // Set viewport top to this position in SplitViewState
        if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
            view_state.viewport.top_byte = scroll_position.0;
            view_state.viewport.top_view_line_offset = scroll_position.1;
            // Skip ensure_visible so the scroll position isn't undone during render
            view_state.viewport.set_skip_ensure_visible();
        }

        // Move cursor to be visible in the new viewport (after releasing the state borrow)
        self.move_cursor_to_visible_area(split_id, buffer_id);

        Ok(())
    }

    /// Handle scrollbar jump (clicking on track or absolute positioning)
    pub(super) fn handle_scrollbar_jump(
        &mut self,
        _col: u16,
        row: u16,
        split_id: LeafId,
        buffer_id: BufferId,
        scrollbar_rect: ratatui::layout::Rect,
    ) -> AnyhowResult<()> {
        // Calculate which line to scroll to based on mouse position
        let scrollbar_height = scrollbar_rect.height as usize;
        if scrollbar_height == 0 {
            return Ok(());
        }

        // Get relative position in scrollbar (0.0 to 1.0)
        // Divide by (height - 1) to map first row to 0.0 and last row to 1.0
        let relative_row = row.saturating_sub(scrollbar_rect.y);
        let ratio = if scrollbar_height > 1 {
            ((relative_row as f64) / ((scrollbar_height - 1) as f64)).clamp(0.0, 1.0)
        } else {
            0.0
        };

        // Get viewport height from SplitViewState
        let viewport_height = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.height as usize)
            .unwrap_or(10);

        // Check if line wrapping is enabled
        let line_wrap_enabled = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.line_wrap_enabled)
            .unwrap_or(false);

        let viewport_width = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.width as usize)
            .unwrap_or(80);

        // Get the buffer state and calculate scroll position
        // Returns (byte_position, view_line_offset) for proper positioning within wrapped lines
        let scroll_position = if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let buffer_len = state.buffer.len();
            let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;

            // For small files, use precise line-based calculations
            // For large files, fall back to byte-based estimation
            if buffer_len <= large_file_threshold {
                // When line wrapping is enabled, use visual row calculations
                if line_wrap_enabled {
                    // calculate_scrollbar_jump_visual already handles max scroll limiting
                    // and returns both byte position and view line offset
                    Self::calculate_scrollbar_jump_visual(
                        &mut state.buffer,
                        ratio,
                        viewport_height,
                        viewport_width,
                    )
                } else {
                    // Small file without line wrap: use line-based calculation for precision
                    let total_lines = if buffer_len > 0 {
                        state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1
                    } else {
                        1
                    };

                    let max_scroll_line = total_lines.saturating_sub(viewport_height);

                    let target_byte = if max_scroll_line == 0 {
                        // File fits in viewport, no scrolling
                        0
                    } else {
                        // Map ratio to target line
                        let target_line = (ratio * max_scroll_line as f64).round() as usize;
                        let target_line = target_line.min(max_scroll_line);

                        // Find byte position of target line
                        // We need to iterate 'target_line' times to skip past lines 0..target_line-1,
                        // then one more time to get the position of line 'target_line'
                        let mut iter = state.buffer.line_iterator(0, 80);
                        let mut line_byte = 0;

                        for _ in 0..target_line {
                            if let Some((pos, _content)) = iter.next_line() {
                                line_byte = pos;
                            } else {
                                break;
                            }
                        }

                        // Get the position of the target line
                        if let Some((pos, _)) = iter.next_line() {
                            pos
                        } else {
                            line_byte // Reached end of buffer
                        }
                    };

                    // Find the line start for this byte position
                    let iter = state.buffer.line_iterator(target_byte, 80);
                    let line_start = iter.current_position();

                    // Apply scroll limiting
                    let max_top_byte =
                        Self::calculate_max_scroll_position(&mut state.buffer, viewport_height);
                    (line_start.min(max_top_byte), 0)
                }
            } else {
                // Large file: use byte-based estimation (original logic)
                let target_byte = (buffer_len as f64 * ratio) as usize;
                let target_byte = target_byte.min(buffer_len.saturating_sub(1));

                // Find the line start for this byte position
                let iter = state.buffer.line_iterator(target_byte, 80);
                let line_start = iter.current_position();

                (line_start.min(buffer_len.saturating_sub(1)), 0)
            }
        } else {
            return Ok(());
        };

        // Set viewport top to this position in SplitViewState
        if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
            view_state.viewport.top_byte = scroll_position.0;
            view_state.viewport.top_view_line_offset = scroll_position.1;
            // Skip ensure_visible so the scroll position isn't undone during render
            view_state.viewport.set_skip_ensure_visible();
        }

        // Move cursor to be visible in the new viewport (after releasing the state borrow)
        self.move_cursor_to_visible_area(split_id, buffer_id);

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

    /// Calculate scrollbar jump position using visual rows (for line-wrapped content)
    /// Returns the byte position to scroll to based on the scroll ratio
    /// Calculate scroll position for visual-row-aware scrollbar jump.
    /// Returns (byte_position, view_line_offset) for proper positioning within wrapped lines.
    fn calculate_scrollbar_jump_visual(
        buffer: &mut crate::model::buffer::Buffer,
        ratio: f64,
        viewport_height: usize,
        viewport_width: usize,
    ) -> (usize, usize) {
        use crate::primitives::line_wrapping::{wrap_line, WrapConfig};

        let buffer_len = buffer.len();
        if buffer_len == 0 || viewport_height == 0 {
            return (0, 0);
        }

        // Calculate gutter width (estimate based on line count)
        let line_count = buffer.line_count().unwrap_or(1);
        let digits = (line_count as f64).log10().floor() as usize + 1;
        let gutter_width = 1 + digits.max(4) + 3; // indicator + digits + separator

        let wrap_config = WrapConfig::new(viewport_width, gutter_width, true);

        // Count total visual rows and build a map of visual row -> (line_byte, offset_in_line)
        let mut total_visual_rows = 0;
        let mut visual_row_positions: Vec<(usize, usize)> = Vec::new(); // (line_start_byte, visual_row_offset)

        let mut iter = buffer.line_iterator(0, 80);
        while let Some((line_start, content)) = iter.next_line() {
            let line_content = content.trim_end_matches(['\n', '\r']).to_string();
            let segments = wrap_line(&line_content, &wrap_config);
            let visual_rows_in_line = segments.len().max(1);

            for offset in 0..visual_rows_in_line {
                visual_row_positions.push((line_start, offset));
            }
            total_visual_rows += visual_rows_in_line;
        }

        if total_visual_rows == 0 {
            return (0, 0);
        }

        // Calculate max scroll visual row (leave viewport_height rows visible at bottom)
        let max_scroll_row = total_visual_rows.saturating_sub(viewport_height);

        if max_scroll_row == 0 {
            // Content fits in viewport, no scrolling needed
            return (0, 0);
        }

        // Map ratio to target visual row
        let target_row = (ratio * max_scroll_row as f64).round() as usize;
        let target_row = target_row.min(max_scroll_row);

        // Get the byte position and offset for this visual row
        if target_row < visual_row_positions.len() {
            visual_row_positions[target_row]
        } else {
            // Fallback to last position
            visual_row_positions.last().copied().unwrap_or((0, 0))
        }
    }

    /// Calculate scroll position for visual-row-aware scrollbar drag.
    /// The thumb follows the mouse position, accounting for where on the thumb the user clicked.
    /// Returns (byte_position, view_line_offset) for proper positioning within wrapped lines.
    fn calculate_scrollbar_drag_relative_visual(
        buffer: &mut crate::model::buffer::Buffer,
        current_row: u16,
        scrollbar_y: u16,
        scrollbar_height: usize,
        drag_start_row: u16,
        drag_start_top_byte: usize,
        drag_start_view_line_offset: usize,
        viewport_height: usize,
        viewport_width: usize,
    ) -> (usize, usize) {
        use crate::primitives::line_wrapping::{wrap_line, WrapConfig};

        let buffer_len = buffer.len();
        if buffer_len == 0 || viewport_height == 0 || scrollbar_height <= 1 {
            return (0, 0);
        }

        // Calculate gutter width (estimate based on line count)
        let line_count = buffer.line_count().unwrap_or(1);
        let digits = (line_count as f64).log10().floor() as usize + 1;
        let gutter_width = 1 + digits.max(4) + 3; // indicator + digits + separator

        let wrap_config = WrapConfig::new(viewport_width, gutter_width, true);

        // Build visual row positions map
        let mut total_visual_rows = 0;
        let mut visual_row_positions: Vec<(usize, usize)> = Vec::new();

        let mut iter = buffer.line_iterator(0, 80);
        while let Some((line_start, content)) = iter.next_line() {
            let line_content = content.trim_end_matches(['\n', '\r']).to_string();
            let segments = wrap_line(&line_content, &wrap_config);
            let visual_rows_in_line = segments.len().max(1);

            for offset in 0..visual_rows_in_line {
                visual_row_positions.push((line_start, offset));
            }
            total_visual_rows += visual_rows_in_line;
        }

        if total_visual_rows == 0 {
            return (0, 0);
        }

        let max_scroll_row = total_visual_rows.saturating_sub(viewport_height);
        if max_scroll_row == 0 {
            return (0, 0);
        }

        // Find the visual row corresponding to drag_start_top_byte + view_line_offset
        // First find the line start, then add the offset for wrapped lines
        let line_start_visual_row = visual_row_positions
            .iter()
            .position(|(byte, _)| *byte >= drag_start_top_byte)
            .unwrap_or(0);
        let start_visual_row =
            (line_start_visual_row + drag_start_view_line_offset).min(max_scroll_row);

        // Calculate thumb size (same formula as scrollbar rendering)
        let thumb_size_raw = (viewport_height as f64 / total_visual_rows as f64
            * scrollbar_height as f64)
            .ceil() as usize;
        let max_thumb_size = (scrollbar_height as f64 * 0.8).floor() as usize;
        let thumb_size = thumb_size_raw
            .max(1)
            .min(max_thumb_size)
            .min(scrollbar_height);

        // Calculate max thumb start position (same as scrollbar rendering)
        let max_thumb_start = scrollbar_height.saturating_sub(thumb_size);

        // Calculate where the thumb was (in scrollbar coordinates) at drag start
        // Using the same formula as scrollbar rendering: thumb_start = scroll_ratio * max_thumb_start
        let start_scroll_ratio = start_visual_row as f64 / max_scroll_row as f64;
        let thumb_row_at_start = scrollbar_y as f64 + start_scroll_ratio * max_thumb_start as f64;

        // Calculate click offset (where on the thumb we clicked)
        let click_offset = drag_start_row as f64 - thumb_row_at_start;

        // Calculate target thumb position based on current mouse position
        let target_thumb_row = current_row as f64 - click_offset;

        // Map target thumb position to scroll ratio (inverse of thumb_start formula)
        let target_scroll_ratio = if max_thumb_start > 0 {
            ((target_thumb_row - scrollbar_y as f64) / max_thumb_start as f64).clamp(0.0, 1.0)
        } else {
            0.0
        };

        // Map scroll ratio to visual row
        let target_row = (target_scroll_ratio * max_scroll_row as f64).round() as usize;
        let target_row = target_row.min(max_scroll_row);

        // Get the byte position and offset for this visual row
        if target_row < visual_row_positions.len() {
            visual_row_positions[target_row]
        } else {
            visual_row_positions.last().copied().unwrap_or((0, 0))
        }
    }

    /// Calculate buffer byte position from screen coordinates
    ///
    /// When `compose_width` is set and narrower than the content area, the
    /// content is centered with left padding.  View-line mappings are built
    /// relative to that compose render area, so the same offset must be
    /// applied here when converting screen coordinates.
    ///
    /// Returns None if the position cannot be determined (e.g., click in gutter for click handler)
    pub(crate) fn screen_to_buffer_position(
        col: u16,
        row: u16,
        content_rect: ratatui::layout::Rect,
        gutter_width: u16,
        cached_mappings: &Option<Vec<crate::app::types::ViewLineMapping>>,
        fallback_position: usize,
        allow_gutter_click: bool,
        compose_width: Option<u16>,
    ) -> Option<usize> {
        // Adjust content_rect for compose layout centering
        let content_rect = if let Some(cw) = compose_width {
            let clamped = cw.min(content_rect.width).max(1);
            if clamped < content_rect.width {
                let pad_total = content_rect.width - clamped;
                let left_pad = pad_total / 2;
                ratatui::layout::Rect::new(
                    content_rect.x + left_pad,
                    content_rect.y,
                    clamped,
                    content_rect.height,
                )
            } else {
                content_rect
            }
        } else {
            content_rect
        };

        // Calculate relative position in content area
        let content_col = col.saturating_sub(content_rect.x);
        let content_row = row.saturating_sub(content_rect.y);

        tracing::trace!(
            col,
            row,
            ?content_rect,
            gutter_width,
            content_col,
            content_row,
            num_mappings = cached_mappings.as_ref().map(|m| m.len()),
            "screen_to_buffer_position"
        );

        // Handle gutter clicks
        let text_col = if content_col < gutter_width {
            if !allow_gutter_click {
                return None; // Click handler skips gutter clicks
            }
            0 // Drag handler uses position 0 of the line
        } else {
            content_col.saturating_sub(gutter_width) as usize
        };

        // Use cached view line mappings for accurate position lookup
        let visual_row = content_row as usize;

        // Helper to get position from a line mapping at a given visual column
        let position_from_mapping =
            |line_mapping: &crate::app::types::ViewLineMapping, col: usize| -> usize {
                if col < line_mapping.visual_to_char.len() {
                    // Use O(1) lookup: visual column -> char index -> source byte
                    if let Some(byte_pos) = line_mapping.source_byte_at_visual_col(col) {
                        return byte_pos;
                    }
                    // Column maps to virtual/injected content - find nearest real position
                    for c in (0..col).rev() {
                        if let Some(byte_pos) = line_mapping.source_byte_at_visual_col(c) {
                            return byte_pos;
                        }
                    }
                    line_mapping.line_end_byte
                } else {
                    // Click is past end of visible content
                    // For empty lines (only a newline), return the line start position
                    // to keep cursor on this line rather than jumping to the next line
                    if line_mapping.visual_to_char.len() <= 1 {
                        // Empty or newline-only line - return first source byte if available
                        if let Some(Some(first_byte)) = line_mapping.char_source_bytes.first() {
                            return *first_byte;
                        }
                    }
                    line_mapping.line_end_byte
                }
            };

        let position = cached_mappings
            .as_ref()
            .and_then(|mappings| {
                if let Some(line_mapping) = mappings.get(visual_row) {
                    // Click is on a visible line
                    Some(position_from_mapping(line_mapping, text_col))
                } else if !mappings.is_empty() {
                    // Click is below last visible line - use the last line at the clicked column
                    let last_mapping = mappings.last().unwrap();
                    Some(position_from_mapping(last_mapping, text_col))
                } else {
                    None
                }
            })
            .unwrap_or(fallback_position);

        Some(position)
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
        use crate::model::event::Event;
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
            self.plugin_manager.run_hook(
                "mouse_click",
                HookArgs::MouseClick {
                    column: col,
                    row,
                    button: "left".to_string(),
                    modifiers: modifiers_str,
                    content_x: content_rect.x,
                    content_y: content_rect.y,
                },
            );
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
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let gutter_width = state.margins.left_total_width() as u16;

            let Some(target_position) = Self::screen_to_buffer_position(
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

            // Move the primary cursor to this position
            // If shift is held, extend selection; otherwise clear it
            let (primary_cursor_id, old_position, old_anchor) = self
                .split_view_states
                .get(&split_id)
                .map(|vs| {
                    let pc = vs.cursors.primary();
                    (vs.cursors.primary_id(), pc.position, pc.anchor)
                })
                .unwrap_or((CursorId(0), 0, None));

            // For shift+click or ctrl+click: extend selection from current anchor (or position if no anchor) to click
            // Both modifiers supported since some terminals intercept shift+click
            let extend_selection = modifiers.contains(KeyModifiers::SHIFT)
                || modifiers.contains(KeyModifiers::CONTROL);
            let new_anchor = if extend_selection {
                // If already selecting, keep the existing anchor; otherwise anchor at current position
                Some(old_anchor.unwrap_or(old_position))
            } else {
                None // Clear selection on normal click
            };

            let event = Event::MoveCursor {
                cursor_id: primary_cursor_id,
                old_position,
                new_position: target_position,
                old_anchor,
                new_anchor,
                old_sticky_column: 0,
                new_sticky_column: 0, // Reset sticky column for goto line
            };

            // Apply the event
            if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
                event_log.append(event.clone());
            }
            if let Some(cursors) = self
                .split_view_states
                .get_mut(&split_id)
                .map(|vs| &mut vs.cursors)
            {
                state.apply(cursors, &event);
            }

            // Track position history
            if !self.in_navigation {
                self.position_history
                    .record_movement(buffer_id, target_position, None);
            }

            // Set up drag selection state for potential text selection
            self.mouse_state.dragging_text_selection = true;
            self.mouse_state.drag_selection_split = Some(split_id);
            // For shift+click, anchor stays at selection start; otherwise anchor at click position
            self.mouse_state.drag_selection_anchor = Some(new_anchor.unwrap_or(target_position));

            // Fire cursor_moved hook so plugins (e.g. markdown compose) can
            // update cursor-aware conceals (auto-expose emphasis markers).
            let line_info = self.calculate_event_line_info(&event);
            self.trigger_plugin_hooks_for_event(&event, line_info);
        }

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
                        // Open the file but keep focus on file explorer (single click)
                        // Double-click or Enter will focus the editor
                        let path = node.entry.path.clone();
                        let name = node.entry.name.clone();
                        match self.open_file(&path) {
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

    /// Start the line ending selection prompt
    fn start_set_line_ending_prompt(&mut self) {
        use crate::model::buffer::LineEnding;

        let current_line_ending = self.active_state().buffer.line_ending();

        let options = [
            (LineEnding::LF, "LF", "Unix/Linux/Mac"),
            (LineEnding::CRLF, "CRLF", "Windows"),
            (LineEnding::CR, "CR", "Classic Mac"),
        ];

        let current_index = options
            .iter()
            .position(|(le, _, _)| *le == current_line_ending)
            .unwrap_or(0);

        let suggestions: Vec<crate::input::commands::Suggestion> = options
            .iter()
            .map(|(le, name, desc)| {
                let is_current = *le == current_line_ending;
                crate::input::commands::Suggestion {
                    text: format!("{} ({})", name, desc),
                    description: if is_current {
                        Some("current".to_string())
                    } else {
                        None
                    },
                    value: Some(name.to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Line ending: ".to_string(),
            PromptType::SetLineEnding,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                let (_, name, desc) = options[current_index];
                prompt.input = format!("{} ({})", name, desc);
                prompt.cursor_pos = prompt.input.len();
            }
        }
    }

    /// Start the encoding selection prompt
    fn start_set_encoding_prompt(&mut self) {
        use crate::model::buffer::Encoding;

        let current_encoding = self.active_state().buffer.encoding();

        let suggestions: Vec<crate::input::commands::Suggestion> = Encoding::all()
            .iter()
            .map(|enc| {
                let is_current = *enc == current_encoding;
                crate::input::commands::Suggestion {
                    text: format!("{} ({})", enc.display_name(), enc.description()),
                    description: if is_current {
                        Some("current".to_string())
                    } else {
                        None
                    },
                    value: Some(enc.display_name().to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        let current_index = Encoding::all()
            .iter()
            .position(|enc| *enc == current_encoding)
            .unwrap_or(0);

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Encoding: ".to_string(),
            PromptType::SetEncoding,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                let enc = Encoding::all()[current_index];
                prompt.input = format!("{} ({})", enc.display_name(), enc.description());
                prompt.cursor_pos = prompt.input.len();
                // Select all text so typing immediately replaces it
                prompt.selection_anchor = Some(0);
            }
        }
    }

    /// Start the reload with encoding prompt
    ///
    /// Prompts user to select an encoding, then reloads the current file with that encoding.
    /// Requires the buffer to have no unsaved modifications.
    fn start_reload_with_encoding_prompt(&mut self) {
        use crate::model::buffer::Encoding;

        // Check if buffer has a file path
        let has_file = self
            .buffers
            .get(&self.active_buffer())
            .and_then(|s| s.buffer.file_path())
            .is_some();

        if !has_file {
            self.set_status_message("Cannot reload: buffer has no file".to_string());
            return;
        }

        // Check for unsaved modifications
        let is_modified = self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.buffer.is_modified())
            .unwrap_or(false);

        if is_modified {
            self.set_status_message(
                "Cannot reload: buffer has unsaved modifications (save first)".to_string(),
            );
            return;
        }

        let current_encoding = self.active_state().buffer.encoding();

        let suggestions: Vec<crate::input::commands::Suggestion> = Encoding::all()
            .iter()
            .map(|enc| {
                let is_current = *enc == current_encoding;
                crate::input::commands::Suggestion {
                    text: format!("{} ({})", enc.display_name(), enc.description()),
                    description: if is_current {
                        Some("current".to_string())
                    } else {
                        None
                    },
                    value: Some(enc.display_name().to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        let current_index = Encoding::all()
            .iter()
            .position(|enc| *enc == current_encoding)
            .unwrap_or(0);

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Reload with encoding: ".to_string(),
            PromptType::ReloadWithEncoding,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                let enc = Encoding::all()[current_index];
                prompt.input = format!("{} ({})", enc.display_name(), enc.description());
                prompt.cursor_pos = prompt.input.len();
            }
        }
    }

    /// Start the language selection prompt
    fn start_set_language_prompt(&mut self) {
        let current_language = self.active_state().language.clone();

        // Build suggestions from all available syntect syntaxes + Plain Text option
        let mut suggestions: Vec<crate::input::commands::Suggestion> = vec![
            // Plain Text option (no syntax highlighting)
            crate::input::commands::Suggestion {
                text: "Plain Text".to_string(),
                description: if current_language == "Plain Text" || current_language == "text" {
                    Some("current".to_string())
                } else {
                    None
                },
                value: Some("Plain Text".to_string()),
                disabled: false,
                keybinding: None,
                source: None,
            },
        ];

        // Add all available syntaxes from the grammar registry (100+ languages)
        let mut syntax_names: Vec<&str> = self.grammar_registry.available_syntaxes();
        // Sort alphabetically for easier navigation
        syntax_names.sort_unstable_by(|a, b| a.to_lowercase().cmp(&b.to_lowercase()));

        for syntax_name in syntax_names {
            // Skip "Plain Text" as we already added it at the top
            if syntax_name == "Plain Text" {
                continue;
            }
            let is_current = syntax_name == current_language;
            suggestions.push(crate::input::commands::Suggestion {
                text: syntax_name.to_string(),
                description: if is_current {
                    Some("current".to_string())
                } else {
                    None
                },
                value: Some(syntax_name.to_string()),
                disabled: false,
                keybinding: None,
                source: None,
            });
        }

        // Find current language index
        let current_index = suggestions
            .iter()
            .position(|s| s.value.as_deref() == Some(&current_language))
            .unwrap_or(0);

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Language: ".to_string(),
            PromptType::SetLanguage,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                // Don't set input - keep it empty so typing filters the list
                // The selected suggestion shows the current language
            }
        }
    }

    /// Start the theme selection prompt with available themes
    fn start_select_theme_prompt(&mut self) {
        let available_themes = self.theme_registry.list();
        let current_theme_name = &self.theme.name;

        // Find the index of the current theme
        let current_index = available_themes
            .iter()
            .position(|info| info.name == *current_theme_name)
            .unwrap_or(0);

        let suggestions: Vec<crate::input::commands::Suggestion> = available_themes
            .iter()
            .map(|info| {
                let is_current = info.name == *current_theme_name;
                let description = match (is_current, info.pack.is_empty()) {
                    (true, true) => Some("(current)".to_string()),
                    (true, false) => Some(format!("{} (current)", info.pack)),
                    (false, true) => None,
                    (false, false) => Some(info.pack.clone()),
                };
                crate::input::commands::Suggestion {
                    text: info.name.clone(),
                    description,
                    value: Some(info.name.clone()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Select theme: ".to_string(),
            PromptType::SelectTheme {
                original_theme: current_theme_name.clone(),
            },
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                // Also set input to match selected theme
                prompt.input = current_theme_name.to_string();
                prompt.cursor_pos = prompt.input.len();
            }
        }
    }

    /// Apply a theme by name and persist it to config
    pub(super) fn apply_theme(&mut self, theme_name: &str) {
        if !theme_name.is_empty() {
            if let Some(theme) = self.theme_registry.get_cloned(theme_name) {
                self.theme = theme;

                // Set terminal cursor color to match theme
                self.theme.set_terminal_cursor_color();

                // Update the config in memory using the normalized registry key,
                // not the JSON name field, so that the config value can be looked
                // up in the registry on restart (fixes #1001).
                let normalized = crate::view::theme::normalize_theme_name(theme_name);
                self.config.theme = normalized.into();

                // Persist to config file
                self.save_theme_to_config();

                self.set_status_message(
                    t!("view.theme_changed", theme = self.theme.name.clone()).to_string(),
                );
            } else {
                self.set_status_message(format!("Theme '{}' not found", theme_name));
            }
        }
    }

    /// Preview a theme by name (without persisting to config)
    /// Used for live preview when navigating theme selection
    pub(super) fn preview_theme(&mut self, theme_name: &str) {
        if !theme_name.is_empty() && theme_name != self.theme.name {
            if let Some(theme) = self.theme_registry.get_cloned(theme_name) {
                self.theme = theme;
                self.theme.set_terminal_cursor_color();
            }
        }
    }

    /// Save the current theme setting to the user's config file
    fn save_theme_to_config(&mut self) {
        // Create the directory if it doesn't exist
        if let Err(e) = self.filesystem.create_dir_all(&self.dir_context.config_dir) {
            tracing::warn!("Failed to create config directory: {}", e);
            return;
        }

        // Save the theme using explicit changes to avoid the issue where
        // changing to the default theme doesn't persist (because save_to_layer
        // computes delta vs defaults and sees no difference).
        let resolver = ConfigResolver::new(self.dir_context.clone(), self.working_dir.clone());
        let config_path = resolver.user_config_path();
        tracing::info!(
            "Saving theme '{}' to user config at {}",
            self.config.theme.0,
            config_path.display()
        );

        let mut changes = std::collections::HashMap::new();
        changes.insert(
            "/theme".to_string(),
            serde_json::Value::String(self.config.theme.0.clone()),
        );

        match resolver.save_changes_to_layer(
            &changes,
            &std::collections::HashSet::new(),
            ConfigLayer::User,
        ) {
            Ok(()) => {
                tracing::info!("Theme saved successfully to {}", config_path.display());
            }
            Err(e) => {
                tracing::warn!("Failed to save theme to config: {}", e);
            }
        }
    }

    /// Start the keybinding map selection prompt with available maps
    fn start_select_keybinding_map_prompt(&mut self) {
        // Built-in keybinding maps
        let builtin_maps = vec!["default", "emacs", "vscode", "macos"];

        // Collect user-defined keybinding maps from config
        let user_maps: Vec<&str> = self
            .config
            .keybinding_maps
            .keys()
            .map(|s| s.as_str())
            .collect();

        // Combine built-in and user maps
        let mut all_maps: Vec<&str> = builtin_maps;
        for map in &user_maps {
            if !all_maps.contains(map) {
                all_maps.push(map);
            }
        }

        let current_map = &self.config.active_keybinding_map;

        // Find the index of the current keybinding map
        let current_index = all_maps
            .iter()
            .position(|name| *name == current_map)
            .unwrap_or(0);

        let suggestions: Vec<crate::input::commands::Suggestion> = all_maps
            .iter()
            .map(|map_name| {
                let is_current = *map_name == current_map;
                crate::input::commands::Suggestion {
                    text: map_name.to_string(),
                    description: if is_current {
                        Some("(current)".to_string())
                    } else {
                        None
                    },
                    value: Some(map_name.to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Select keybinding map: ".to_string(),
            PromptType::SelectKeybindingMap,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                // Also set input to match selected map
                prompt.input = current_map.to_string();
                prompt.cursor_pos = prompt.input.len();
            }
        }
    }

    /// Apply a keybinding map by name and persist it to config
    pub(super) fn apply_keybinding_map(&mut self, map_name: &str) {
        if map_name.is_empty() {
            return;
        }

        // Check if the map exists (either built-in or user-defined)
        let is_builtin = matches!(map_name, "default" | "emacs" | "vscode" | "macos");
        let is_user_defined = self.config.keybinding_maps.contains_key(map_name);

        if is_builtin || is_user_defined {
            // Update the active keybinding map in config
            self.config.active_keybinding_map = map_name.to_string().into();

            // Reload the keybinding resolver with the new map
            self.keybindings = crate::input::keybindings::KeybindingResolver::new(&self.config);

            // Persist to config file
            self.save_keybinding_map_to_config();

            self.set_status_message(t!("view.keybindings_switched", map = map_name).to_string());
        } else {
            self.set_status_message(t!("view.keybindings_unknown", map = map_name).to_string());
        }
    }

    /// Save the current keybinding map setting to the user's config file
    fn save_keybinding_map_to_config(&mut self) {
        // Create the directory if it doesn't exist
        if let Err(e) = self.filesystem.create_dir_all(&self.dir_context.config_dir) {
            tracing::warn!("Failed to create config directory: {}", e);
            return;
        }

        // Save the config using the resolver
        let resolver = ConfigResolver::new(self.dir_context.clone(), self.working_dir.clone());
        if let Err(e) = resolver.save_to_layer(&self.config, ConfigLayer::User) {
            tracing::warn!("Failed to save keybinding map to config: {}", e);
        }
    }

    /// Start the cursor style selection prompt
    fn start_select_cursor_style_prompt(&mut self) {
        use crate::config::CursorStyle;

        let current_style = self.config.editor.cursor_style;

        // Build suggestions from available cursor styles
        let suggestions: Vec<crate::input::commands::Suggestion> = CursorStyle::OPTIONS
            .iter()
            .zip(CursorStyle::DESCRIPTIONS.iter())
            .map(|(style_name, description)| {
                let is_current = *style_name == current_style.as_str();
                crate::input::commands::Suggestion {
                    text: description.to_string(),
                    description: if is_current {
                        Some("(current)".to_string())
                    } else {
                        None
                    },
                    value: Some(style_name.to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        // Find the index of the current cursor style
        let current_index = CursorStyle::OPTIONS
            .iter()
            .position(|s| *s == current_style.as_str())
            .unwrap_or(0);

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Select cursor style: ".to_string(),
            PromptType::SelectCursorStyle,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                prompt.input = CursorStyle::DESCRIPTIONS[current_index].to_string();
                prompt.cursor_pos = prompt.input.len();
            }
        }
    }

    /// Apply a cursor style and persist it to config
    pub(super) fn apply_cursor_style(&mut self, style_name: &str) {
        use crate::config::CursorStyle;

        if let Some(style) = CursorStyle::parse(style_name) {
            // Update the config in memory
            self.config.editor.cursor_style = style;

            // Apply the cursor style to the terminal
            if self.session_mode {
                // In session mode, queue the escape sequence to be sent to the client
                self.queue_escape_sequences(style.to_escape_sequence());
            } else {
                // In normal mode, write directly to stdout
                use std::io::stdout;
                // Best-effort cursor style change to stdout.
                #[allow(clippy::let_underscore_must_use)]
                let _ = crossterm::execute!(stdout(), style.to_crossterm_style());
            }

            // Persist to config file
            self.save_cursor_style_to_config();

            // Find the description for the status message
            let description = CursorStyle::OPTIONS
                .iter()
                .zip(CursorStyle::DESCRIPTIONS.iter())
                .find(|(name, _)| **name == style_name)
                .map(|(_, desc)| *desc)
                .unwrap_or(style_name);

            self.set_status_message(
                t!("view.cursor_style_changed", style = description).to_string(),
            );
        }
    }

    /// Start the remove ruler prompt with current rulers as suggestions
    fn start_remove_ruler_prompt(&mut self) {
        let active_split = self.split_manager.active_split();
        let rulers = self
            .split_view_states
            .get(&active_split)
            .map(|vs| vs.rulers.clone())
            .unwrap_or_default();

        if rulers.is_empty() {
            self.set_status_message(t!("rulers.none_configured").to_string());
            return;
        }

        let suggestions: Vec<crate::input::commands::Suggestion> = rulers
            .iter()
            .map(|&col| crate::input::commands::Suggestion {
                text: format!("Column {}", col),
                description: None,
                value: Some(col.to_string()),
                disabled: false,
                keybinding: None,
                source: None,
            })
            .collect();

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            t!("rulers.remove_prompt").to_string(),
            PromptType::RemoveRuler,
            suggestions,
        ));
    }

    /// Save the current cursor style setting to the user's config file
    fn save_cursor_style_to_config(&mut self) {
        // Create the directory if it doesn't exist
        if let Err(e) = self.filesystem.create_dir_all(&self.dir_context.config_dir) {
            tracing::warn!("Failed to create config directory: {}", e);
            return;
        }

        // Save the config using the resolver
        let resolver = ConfigResolver::new(self.dir_context.clone(), self.working_dir.clone());
        if let Err(e) = resolver.save_to_layer(&self.config, ConfigLayer::User) {
            tracing::warn!("Failed to save cursor style to config: {}", e);
        }
    }

    /// Start the locale selection prompt with available locales
    fn start_select_locale_prompt(&mut self) {
        let available_locales = crate::i18n::available_locales();
        let current_locale = crate::i18n::current_locale();

        // Find the index of the current locale
        let current_index = available_locales
            .iter()
            .position(|name| *name == current_locale)
            .unwrap_or(0);

        let suggestions: Vec<crate::input::commands::Suggestion> = available_locales
            .iter()
            .map(|locale_name| {
                let is_current = *locale_name == current_locale;
                let description = if let Some((english_name, native_name)) =
                    crate::i18n::locale_display_name(locale_name)
                {
                    if english_name == native_name {
                        // Same name (e.g., English/English)
                        if is_current {
                            format!("{} (current)", english_name)
                        } else {
                            english_name.to_string()
                        }
                    } else {
                        // Different names (e.g., German/Deutsch)
                        if is_current {
                            format!("{} / {} (current)", english_name, native_name)
                        } else {
                            format!("{} / {}", english_name, native_name)
                        }
                    }
                } else {
                    // Unknown locale
                    if is_current {
                        "(current)".to_string()
                    } else {
                        String::new()
                    }
                };
                crate::input::commands::Suggestion {
                    text: locale_name.to_string(),
                    description: if description.is_empty() {
                        None
                    } else {
                        Some(description)
                    },
                    value: Some(locale_name.to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            t!("locale.select_prompt").to_string(),
            PromptType::SelectLocale,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                // Start with empty input to show all options initially
                prompt.input = String::new();
                prompt.cursor_pos = 0;
            }
        }
    }

    /// Apply a locale and persist it to config
    pub(super) fn apply_locale(&mut self, locale_name: &str) {
        if !locale_name.is_empty() {
            // Update the locale at runtime
            crate::i18n::set_locale(locale_name);

            // Update the config in memory
            self.config.locale = crate::config::LocaleName(Some(locale_name.to_string()));

            // Regenerate menus with the new locale
            self.menus = crate::config::MenuConfig::translated();

            // Refresh command palette commands with new locale
            if let Ok(mut registry) = self.command_registry.write() {
                registry.refresh_builtin_commands();
            }

            // Persist to config file
            self.save_locale_to_config();

            self.set_status_message(t!("locale.changed", locale_name = locale_name).to_string());
        }
    }

    /// Save the current locale setting to the user's config file
    fn save_locale_to_config(&mut self) {
        // Create the directory if it doesn't exist
        if let Err(e) = self.filesystem.create_dir_all(&self.dir_context.config_dir) {
            tracing::warn!("Failed to create config directory: {}", e);
            return;
        }

        // Save the config using the resolver
        let resolver = ConfigResolver::new(self.dir_context.clone(), self.working_dir.clone());
        if let Err(e) = resolver.save_to_layer(&self.config, ConfigLayer::User) {
            tracing::warn!("Failed to save locale to config: {}", e);
        }
    }

    /// Switch to the previously active tab in the current split
    fn switch_to_previous_tab(&mut self) {
        let active_split = self.split_manager.active_split();
        let previous_buffer = self
            .split_view_states
            .get(&active_split)
            .and_then(|vs| vs.previous_buffer());

        if let Some(prev_id) = previous_buffer {
            // Verify the buffer is still open in this split
            let is_valid = self
                .split_view_states
                .get(&active_split)
                .is_some_and(|vs| vs.open_buffers.contains(&prev_id));

            if is_valid && prev_id != self.active_buffer() {
                // Save current position before switching
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
        } else {
            self.set_status_message(t!("status.no_previous_tab").to_string());
        }
    }

    /// Start the switch-to-tab-by-name prompt with suggestions from open buffers
    fn start_switch_to_tab_prompt(&mut self) {
        let active_split = self.split_manager.active_split();
        let open_buffers = if let Some(view_state) = self.split_view_states.get(&active_split) {
            view_state.open_buffers.clone()
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
            .is_some_and(|vs| vs.open_buffers.contains(&buffer_id));

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
                    self.active_event_log_mut().append(event.clone());
                    self.apply_event_to_active_buffer(&event);
                    self.track_cursor_movement(&event);
                }
            }
        }

        Ok(())
    }

    /// Track cursor movement in position history if applicable.
    fn track_cursor_movement(&mut self, event: &Event) {
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
}
