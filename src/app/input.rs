use super::*;
use crate::services::plugins::hooks::HookArgs;
impl Editor {
    /// Determine the current keybinding context based on UI state
    pub fn get_key_context(&self) -> crate::input::keybindings::KeyContext {
        use crate::input::keybindings::KeyContext;

        // Priority order: Settings > Menu > Prompt > Popup > Rename > Current context (FileExplorer or Normal)
        if self.settings_state.as_ref().map_or(false, |s| s.visible) {
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
    ) -> std::io::Result<()> {
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
        if matches!(context, crate::input::keybindings::KeyContext::Popup) {
            // Check if the current popup is transient (hover, signature help)
            let is_transient_popup = self
                .active_state()
                .popups
                .top()
                .is_some_and(|p| p.transient);

            if is_transient_popup {
                // Dismiss the popup on any key press
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

        // Only check buffer mode keybindings if we're not in a higher-priority context
        // (Menu, Prompt, Popup should take precedence over mode bindings)
        let should_check_mode_bindings = matches!(
            context,
            crate::input::keybindings::KeyContext::Normal
                | crate::input::keybindings::KeyContext::FileExplorer
        );

        if should_check_mode_bindings {
            // Check buffer mode keybindings (for virtual buffers with custom modes)
            // Mode keybindings resolve to Action names (see Action::from_str)
            if let Some(action_name) = self.resolve_mode_keybinding(code, modifiers) {
                tracing::debug!("Mode keybinding resolved to action: {}", action_name);
                let action = Action::from_str(&action_name, &std::collections::HashMap::new())
                    .unwrap_or_else(|| Action::PluginAction(action_name));
                return self.handle_action(action);
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
    pub(super) fn handle_action(&mut self, action: Action) -> std::io::Result<()> {
        use crate::input::keybindings::Action;

        // Record action to macro if recording
        self.record_macro_action(&action);

        match action {
            Action::Quit => self.quit(),
            Action::Save => {
                // Check if buffer has a file path - if not, redirect to SaveAs
                if self.active_state().buffer.file_path().is_none() {
                    self.start_prompt_with_initial_text(
                        "Save as: ".to_string(),
                        PromptType::SaveFileAs,
                        String::new(),
                    );
                } else if self.check_save_conflict().is_some() {
                    // Check if file was modified externally since we opened/saved it
                    self.start_prompt(
                        "File changed on disk. (o)verwrite, (C)ancel? ".to_string(),
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
                    "Save as: ".to_string(),
                    PromptType::SaveFileAs,
                    current_path,
                );
            }
            Action::Open => {
                self.start_prompt("Open file: ".to_string(), PromptType::OpenFile);
                self.prefill_open_file_prompt();
                self.init_file_open_state();
            }
            Action::SwitchProject => {
                self.start_prompt("Switch project: ".to_string(), PromptType::SwitchProject);
                self.init_folder_open_state();
            }
            Action::GotoLine => self.start_prompt("Go to line: ".to_string(), PromptType::GotoLine),
            Action::New => {
                self.new_buffer();
            }
            Action::Close => {
                let buffer_id = self.active_buffer();
                if self.active_state().buffer.is_modified() {
                    // Buffer has unsaved changes - prompt for confirmation
                    let name = self.get_buffer_display_name(buffer_id);
                    self.start_prompt(
                        format!("'{}' modified. (s)ave, (d)iscard, (C)ancel? ", name),
                        PromptType::ConfirmCloseBuffer { buffer_id },
                    );
                } else if let Err(e) = self.close_buffer(buffer_id) {
                    self.set_status_message(format!("Cannot close buffer: {}", e));
                } else {
                    self.set_status_message("Buffer closed".to_string());
                }
            }
            Action::CloseTab => {
                self.close_tab();
            }
            Action::Revert => {
                // Check if buffer has unsaved changes - prompt for confirmation
                if self.active_state().buffer.is_modified() {
                    self.start_prompt(
                        "Buffer has unsaved changes. (r)evert, (C)ancel? ".to_string(),
                        PromptType::ConfirmRevert,
                    );
                } else {
                    // No local changes, just revert
                    if let Err(e) = self.revert_file() {
                        self.set_status_message(format!("Failed to revert: {}", e));
                    }
                }
            }
            Action::ToggleAutoRevert => {
                self.toggle_auto_revert();
            }
            Action::Copy => self.copy_selection(),
            Action::Cut => {
                if self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }
                self.cut_selection()
            }
            Action::Paste => {
                if self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }
                self.paste()
            }
            Action::Undo => {
                if self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }
                let event_log = self.active_event_log_mut();
                let before_idx = event_log.current_index();
                let can_undo = event_log.can_undo();
                let events = event_log.undo();
                let after_idx = self.active_event_log().current_index();
                tracing::debug!(
                    "Undo: before_idx={}, after_idx={}, can_undo={}, events_count={}",
                    before_idx,
                    after_idx,
                    can_undo,
                    events.len()
                );
                // Apply all inverse events collected during undo
                for event in &events {
                    tracing::debug!("Undo applying event: {:?}", event);
                    self.apply_event_to_active_buffer(event);
                }
                // Update modified status based on event log position
                self.update_modified_from_event_log();
            }
            Action::Redo => {
                if self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }
                let events = self.active_event_log_mut().redo();
                // Apply all events collected during redo
                for event in events {
                    self.apply_event_to_active_buffer(&event);
                }
                // Update modified status based on event log position
                self.update_modified_from_event_log();
            }
            Action::ShowHelp => {
                self.open_help_manual();
            }
            Action::ShowKeyboardShortcuts => {
                self.open_keyboard_shortcuts();
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
                let suggestions = self.command_registry.read().unwrap().filter(
                    "",
                    self.key_context,
                    &self.keybindings,
                    self.has_active_selection(),
                    &self.active_custom_contexts,
                );
                self.start_prompt_with_suggestions(
                    "Command: ".to_string(),
                    PromptType::Command,
                    suggestions,
                );
            }
            Action::ToggleLineWrap => {
                self.config.editor.line_wrap = !self.config.editor.line_wrap;

                // Update all viewports to reflect the new line wrap setting
                for view_state in self.split_view_states.values_mut() {
                    view_state.viewport.line_wrap_enabled = self.config.editor.line_wrap;
                }

                let state = if self.config.editor.line_wrap {
                    "enabled"
                } else {
                    "disabled"
                };
                self.set_status_message(format!("Line wrap {}", state));
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
                self.request_completion()?;
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
                self.request_signature_help()?;
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
                    self.start_search_prompt("Search: ".to_string(), PromptType::Search, false);
                }
            }
            Action::Replace => {
                // Use same flow as query-replace, just with confirm_each defaulting to false
                self.start_search_prompt("Replace: ".to_string(), PromptType::ReplaceSearch, false);
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
                self.start_search_prompt("Search: ".to_string(), PromptType::Search, true);
            }
            Action::FindNext => {
                self.find_next();
            }
            Action::FindPrevious => {
                self.find_previous();
            }
            Action::AddCursorNextMatch => self.add_cursor_at_next_match(),
            Action::AddCursorAbove => self.add_cursor_above(),
            Action::AddCursorBelow => self.add_cursor_below(),
            Action::NextBuffer => self.next_buffer(),
            Action::PrevBuffer => self.prev_buffer(),
            Action::SwitchToPreviousTab => self.switch_to_previous_tab(),
            Action::SwitchToTabByName => self.start_switch_to_tab_prompt(),

            // Tab scrolling
            Action::ScrollTabsLeft => {
                let active_split_id = self.split_manager.active_split();
                if let Some(view_state) = self.split_view_states.get_mut(&active_split_id) {
                    view_state.tab_scroll_offset = view_state.tab_scroll_offset.saturating_sub(5);
                    // After manual scroll, re-evaluate to clamp and show indicators
                    self.ensure_active_tab_visible(
                        active_split_id,
                        self.active_buffer(),
                        self.terminal_width,
                    );
                    self.set_status_message("Scrolled tabs left".to_string());
                }
            }
            Action::ScrollTabsRight => {
                let active_split_id = self.split_manager.active_split();
                if let Some(view_state) = self.split_view_states.get_mut(&active_split_id) {
                    view_state.tab_scroll_offset = view_state.tab_scroll_offset.saturating_add(5);
                    // After manual scroll, re-evaluate to clamp and show indicators
                    self.ensure_active_tab_visible(
                        active_split_id,
                        self.active_buffer(),
                        self.terminal_width,
                    );
                    self.set_status_message("Scrolled tabs right".to_string());
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
            Action::ToggleLineNumbers => self.toggle_line_numbers(),
            Action::ToggleMouseCapture => self.toggle_mouse_capture(),
            Action::ToggleMouseHover => self.toggle_mouse_hover(),
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
                        let primary = *state.cursors.primary();
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
                let is_builtin = matches!(map_name.as_str(), "default" | "emacs" | "vscode");
                let is_user_defined = self.config.keybinding_maps.contains_key(&map_name);

                if is_builtin || is_user_defined {
                    // Update the active keybinding map in config
                    self.config.active_keybinding_map = map_name.clone().into();

                    // Reload the keybinding resolver with the new map
                    self.keybindings =
                        crate::input::keybindings::KeybindingResolver::new(&self.config);

                    self.set_status_message(format!("Switched to '{}' keybindings", map_name));
                } else {
                    self.set_status_message(format!("Unknown keybinding map: '{}'", map_name));
                }
            }

            Action::SmartHome => {
                self.smart_home();
            }
            Action::IndentSelection => {
                self.indent_selection();
            }
            Action::DedentSelection => {
                self.dedent_selection();
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
                self.set_status_message(format!("Case-sensitive search {}", state));
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
                self.set_status_message(format!("Whole word search {}", state));
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
                self.set_status_message(format!("Regex search {}", state));
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
                self.set_status_message(format!("Confirm each replacement {}", state));
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
                    self.set_status_message("No macro has been recorded yet".to_string());
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
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }
                // Normal backspace handling
                if let Some(events) = self.action_to_events(Action::DeleteBackward) {
                    if events.len() > 1 {
                        let batch = Event::Batch {
                            events: events.clone(),
                            description: "Delete backward".to_string(),
                        };
                        self.active_event_log_mut().append(batch.clone());
                        self.apply_event_to_active_buffer(&batch);
                        // Note: LSP notifications now handled automatically by apply_event_to_active_buffer
                    } else {
                        for event in events {
                            self.active_event_log_mut().append(event.clone());
                            self.apply_event_to_active_buffer(&event);
                            // Note: LSP notifications now handled automatically by apply_event_to_active_buffer
                        }
                    }
                }
            }
            Action::PluginAction(action_name) => {
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
                            self.set_status_message(format!("Plugin error: {}", e));
                            tracing::error!("Plugin action error: {}", e);
                        }
                    }
                } else {
                    self.set_status_message("Plugin manager not available".to_string());
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
                    self.set_status_message("Terminal mode enabled".to_string());
                }
            }
            Action::TerminalEscape => {
                // Exit terminal mode back to editor
                if self.terminal_mode {
                    self.terminal_mode = false;
                    self.key_context = KeyContext::Normal;
                    self.set_status_message("Terminal mode disabled".to_string());
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
            Action::OpenSettings => {
                self.open_settings();
            }
            Action::CloseSettings => {
                // Check if there are unsaved changes
                let has_changes = self
                    .settings_state
                    .as_ref()
                    .map_or(false, |s| s.has_changes());
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
                } else {
                    self.handle_insert_char_editor(c)?;
                }
            }
            _ => {
                // Convert action to events and apply them
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
                        | Action::IndentSelection
                        | Action::DedentSelection
                        | Action::ToggleComment
                );

                if is_editing_action && self.is_editing_disabled() {
                    self.set_status_message("Editing disabled in this buffer".to_string());
                    return Ok(());
                }

                if let Some(events) = self.action_to_events(action) {
                    // Wrap multiple events (multi-cursor) in a Batch for atomic undo
                    if events.len() > 1 {
                        let batch = Event::Batch {
                            events: events.clone(),
                            description: action_description,
                        };
                        self.active_event_log_mut().append(batch.clone());
                        self.apply_event_to_active_buffer(&batch);
                        // Note: LSP notifications now handled automatically by apply_event_to_active_buffer

                        // Track position history for all events in the batch
                        for event in &events {
                            // Track cursor movements in position history (but not during navigation)
                            if !self.in_navigation {
                                if let Event::MoveCursor {
                                    new_position,
                                    new_anchor,
                                    ..
                                } = event
                                {
                                    self.position_history.record_movement(
                                        self.active_buffer(),
                                        *new_position,
                                        *new_anchor,
                                    );
                                }
                            }
                        }
                    } else {
                        // Single cursor - no need for batch
                        for event in events {
                            self.active_event_log_mut().append(event.clone());
                            self.apply_event_to_active_buffer(&event);
                            // Note: LSP notifications now handled automatically by apply_event_to_active_buffer

                            // Track cursor movements in position history (but not during navigation)
                            if !self.in_navigation {
                                if let Event::MoveCursor {
                                    new_position,
                                    new_anchor,
                                    ..
                                } = event
                                {
                                    self.position_history.record_movement(
                                        self.active_buffer(),
                                        new_position,
                                        new_anchor,
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

    /// Handle mouse wheel scroll event
    pub(super) fn handle_mouse_scroll(
        &mut self,
        col: u16,
        row: u16,
        delta: i32,
    ) -> std::io::Result<()> {
        // Sync viewport from EditorState to SplitViewState before scrolling.
        // This is necessary because rendering updates EditorState.viewport via ensure_visible,
        // but that change isn't automatically synced to SplitViewState. Without this sync,
        // mouse scroll would use a stale viewport position after keyboard navigation.
        // (Bug #248: Mouse wheel stopped working properly after keyboard use)
        self.sync_editor_state_to_split_view_state();

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
                        current_index.saturating_sub(delta.abs() as usize)
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

        // Get view_transform tokens from SplitViewState (if any)
        let view_transform_tokens = self
            .split_view_states
            .get(&active_split)
            .and_then(|vs| vs.view_transform.as_ref())
            .map(|vt| vt.tokens.clone());

        // Get mutable references to both buffer and view state
        let buffer = self
            .buffers
            .get_mut(&self.active_buffer())
            .map(|s| &mut s.buffer);
        let view_state = self.split_view_states.get_mut(&active_split);

        if let (Some(buffer), Some(view_state)) = (buffer, view_state) {
            let top_byte_before = view_state.viewport.top_byte;
            if let Some(tokens) = view_transform_tokens {
                // Use view-aware scrolling with the transform's tokens
                use crate::view::ui::view_pipeline::ViewLineIterator;
                let view_lines: Vec<_> = ViewLineIterator::new(&tokens).collect();
                view_state
                    .viewport
                    .scroll_view_lines(&view_lines, delta as isize);
            } else {
                // No view transform - use traditional buffer-based scrolling
                if delta < 0 {
                    // Scroll up
                    let lines_to_scroll = delta.abs() as usize;
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

    /// Handle scrollbar drag with relative movement (when dragging from thumb)
    pub(super) fn handle_scrollbar_drag_relative(
        &mut self,
        row: u16,
        split_id: SplitId,
        buffer_id: BufferId,
        scrollbar_rect: ratatui::layout::Rect,
    ) -> std::io::Result<()> {
        let drag_start_row = match self.mouse_state.drag_start_row {
            Some(r) => r,
            None => return Ok(()), // No drag start, shouldn't happen
        };

        let drag_start_top_byte = match self.mouse_state.drag_start_top_byte {
            Some(b) => b,
            None => return Ok(()), // No drag start, shouldn't happen
        };

        // Calculate the offset in rows
        let row_offset = (row as i32) - (drag_start_row as i32);

        // Get viewport height from SplitViewState
        let viewport_height = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.height as usize)
            .unwrap_or(10);

        // Get the buffer state and calculate target position
        let line_start = if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let scrollbar_height = scrollbar_rect.height as usize;
            if scrollbar_height == 0 {
                return Ok(());
            }

            let buffer_len = state.buffer.len();
            let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;

            // For small files, use precise line-based calculations
            // For large files, fall back to byte-based estimation
            let new_top_byte = if buffer_len <= large_file_threshold {
                // Small file: use line-based calculation for precision
                // Count total lines
                let total_lines = if buffer_len > 0 {
                    state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1
                } else {
                    1
                };

                // Calculate max scroll line
                let max_scroll_line = total_lines.saturating_sub(viewport_height);

                if max_scroll_line == 0 {
                    // File fits in viewport, no scrolling
                    0
                } else {
                    // Calculate which line the mouse position corresponds to using linear interpolation
                    // Convert absolute mouse row to relative position within scrollbar
                    let relative_mouse_row = row.saturating_sub(scrollbar_rect.y) as usize;
                    // Divide by (height - 1) to map first row to 0.0 and last row to 1.0
                    let scroll_ratio = if scrollbar_height > 1 {
                        (relative_mouse_row as f64 / (scrollbar_height - 1) as f64).clamp(0.0, 1.0)
                    } else {
                        0.0
                    };

                    // Map scroll ratio to target line
                    let target_line = (scroll_ratio * max_scroll_line as f64).round() as usize;
                    let target_line = target_line.min(max_scroll_line);

                    // Find byte position of target line
                    // We need to iterate 'target_line' times to skip past lines 0..target_line-1,
                    // then one more time to get the position of line 'target_line'
                    let mut iter = state.buffer.line_iterator(0, 80);
                    let mut line_byte = 0;

                    for _ in 0..target_line {
                        if let Some((pos, _content)) = iter.next() {
                            line_byte = pos;
                        } else {
                            break;
                        }
                    }

                    // Get the position of the target line
                    if let Some((pos, _)) = iter.next() {
                        pos
                    } else {
                        line_byte // Reached end of buffer
                    }
                }
            } else {
                // Large file: use byte-based estimation (original logic)
                let bytes_per_pixel = buffer_len as f64 / scrollbar_height as f64;
                let byte_offset = (row_offset as f64 * bytes_per_pixel) as i64;

                let new_top_byte = if byte_offset >= 0 {
                    drag_start_top_byte.saturating_add(byte_offset as usize)
                } else {
                    drag_start_top_byte.saturating_sub((-byte_offset) as usize)
                };

                // Clamp to valid range using byte-based max (avoid iterating entire buffer)
                new_top_byte.min(buffer_len.saturating_sub(1))
            };

            // Find the line start for this byte position
            let iter = state.buffer.line_iterator(new_top_byte, 80);
            iter.current_position()
        } else {
            return Ok(());
        };

        // Set viewport top to this position in SplitViewState
        if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
            view_state.viewport.top_byte = line_start;
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
        split_id: SplitId,
        buffer_id: BufferId,
        scrollbar_rect: ratatui::layout::Rect,
    ) -> std::io::Result<()> {
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

        // Get the buffer state and calculate limited_line_start
        let limited_line_start = if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let buffer_len = state.buffer.len();
            let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;

            // For small files, use precise line-based calculations
            // For large files, fall back to byte-based estimation
            let target_byte = if buffer_len <= large_file_threshold {
                // Small file: use line-based calculation for precision
                let total_lines = if buffer_len > 0 {
                    state.buffer.get_line_number(buffer_len.saturating_sub(1)) + 1
                } else {
                    1
                };

                let max_scroll_line = total_lines.saturating_sub(viewport_height);

                if max_scroll_line == 0 {
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
                        if let Some((pos, _content)) = iter.next() {
                            line_byte = pos;
                        } else {
                            break;
                        }
                    }

                    // Get the position of the target line
                    if let Some((pos, _)) = iter.next() {
                        pos
                    } else {
                        line_byte // Reached end of buffer
                    }
                }
            } else {
                // Large file: use byte-based estimation (original logic)
                let target_byte = (buffer_len as f64 * ratio) as usize;
                target_byte.min(buffer_len.saturating_sub(1))
            };

            // Find the line start for this byte position
            let iter = state.buffer.line_iterator(target_byte, 80);
            let line_start = iter.current_position();

            // Apply scroll limiting
            // Use viewport.height (constant allocated rows) not visible_line_count (varies with content)
            // For large files, use byte-based max to avoid iterating entire buffer
            let max_top_byte = if buffer_len <= large_file_threshold {
                Self::calculate_max_scroll_position(&mut state.buffer, viewport_height)
            } else {
                buffer_len.saturating_sub(1)
            };
            line_start.min(max_top_byte)
        } else {
            return Ok(());
        };

        // Set viewport top to this position in SplitViewState
        if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
            view_state.viewport.top_byte = limited_line_start;
            // Skip ensure_visible so the scroll position isn't undone during render
            view_state.viewport.set_skip_ensure_visible();
        }

        // Move cursor to be visible in the new viewport (after releasing the state borrow)
        self.move_cursor_to_visible_area(split_id, buffer_id);

        Ok(())
    }

    /// Move the cursor to a visible position within the current viewport
    /// This is called after scrollbar operations to ensure the cursor is in view
    pub(super) fn move_cursor_to_visible_area(&mut self, split_id: SplitId, buffer_id: BufferId) {
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
                if let Some((pos, line)) = iter.next() {
                    // The bottom of this line is at pos + line.len()
                    bottom_byte = pos + line.len();
                } else {
                    // Reached end of buffer
                    bottom_byte = buffer_len;
                    break;
                }
            }

            // Check if cursor is outside visible range and move it if needed
            let cursor_pos = state.cursors.primary().position;
            if cursor_pos < top_byte || cursor_pos > bottom_byte {
                // Move cursor to the top of the viewport
                let cursor = state.cursors.primary_mut();
                cursor.position = top_byte;
                // Keep the existing sticky_column value so vertical navigation preserves column
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
        while iter.next().is_some() {
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
            if let Some((pos, _content)) = iter.next() {
                max_byte_pos = pos;
                current_line += 1;
            } else {
                break;
            }
        }

        max_byte_pos
    }

    /// Calculate buffer byte position from screen coordinates
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
    ) -> Option<usize> {
        // Calculate relative position in content area
        let content_col = col.saturating_sub(content_rect.x);
        let content_row = row.saturating_sub(content_rect.y);

        tracing::debug!(
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
        split_id: crate::model::event::SplitId,
        buffer_id: BufferId,
        content_rect: ratatui::layout::Rect,
    ) -> std::io::Result<()> {
        use crate::model::event::Event;

        // Dispatch MouseClick hook to plugins
        // Plugins can handle clicks on their virtual buffers
        if self.plugin_manager.has_hook_handlers("mouse_click") {
            self.plugin_manager.run_hook(
                "mouse_click",
                HookArgs::MouseClick {
                    column: col,
                    row,
                    button: "left".to_string(),
                    modifiers: String::new(),
                    content_x: content_rect.x,
                    content_y: content_rect.y,
                },
            );
        }

        // Focus this split (handles terminal mode exit, tab state, etc.)
        self.focus_split(split_id, buffer_id);

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

            // Move the primary cursor to this position and clear selection
            let primary_cursor_id = state.cursors.primary_id();
            let event = Event::MoveCursor {
                cursor_id: primary_cursor_id,
                old_position: 0, // TODO: Get actual old position
                new_position: target_position,
                old_anchor: None, // TODO: Get actual old anchor
                new_anchor: None, // Clear selection on click
                old_sticky_column: 0,
                new_sticky_column: 0, // Reset sticky column for goto line
            };

            // Apply the event
            if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
                event_log.append(event.clone());
            }
            state.apply(&event);

            // Track position history
            if !self.in_navigation {
                self.position_history
                    .record_movement(buffer_id, target_position, None);
            }

            // Set up drag selection state for potential text selection
            self.mouse_state.dragging_text_selection = true;
            self.mouse_state.drag_selection_split = Some(split_id);
            self.mouse_state.drag_selection_anchor = Some(target_position);
        }

        Ok(())
    }

    /// Handle click in file explorer
    pub(super) fn handle_file_explorer_click(
        &mut self,
        col: u16,
        row: u16,
        explorer_area: ratatui::layout::Rect,
    ) -> std::io::Result<()> {
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
                        // Open the file using the existing method
                        self.file_explorer_open_file()?;
                        // Switch focus back to editor after opening file
                        self.key_context = crate::input::keybindings::KeyContext::Normal;
                    }
                }
            }
        }

        Ok(())
    }

    /// Start the theme selection prompt with available themes
    fn start_select_theme_prompt(&mut self) {
        let available_themes = crate::view::theme::Theme::available_themes();
        let current_theme_name = &self.theme.name;

        // Find the index of the current theme
        let current_index = available_themes
            .iter()
            .position(|name| name == current_theme_name)
            .unwrap_or(0);

        let suggestions: Vec<crate::input::commands::Suggestion> = available_themes
            .iter()
            .map(|theme_name| {
                let is_current = theme_name == current_theme_name;
                crate::input::commands::Suggestion {
                    text: theme_name.to_string(),
                    description: if is_current {
                        Some("(current)".to_string())
                    } else {
                        None
                    },
                    value: Some(theme_name.to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Select theme: ".to_string(),
            PromptType::SelectTheme,
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
            self.theme = crate::view::theme::Theme::from_name(theme_name);

            // Update the config in memory
            self.config.theme = self.theme.name.clone().into();

            // Persist to config file
            self.save_theme_to_config();

            self.set_status_message(format!("Theme changed to '{}'", self.theme.name));
        }
    }

    /// Save the current theme setting to the user's config file
    fn save_theme_to_config(&mut self) {
        // Create the directory if it doesn't exist
        if let Err(e) = std::fs::create_dir_all(&self.dir_context.config_dir) {
            tracing::warn!("Failed to create config directory: {}", e);
            return;
        }

        // Save the config
        let config_path = self.dir_context.config_path();
        if let Err(e) = self.config.save_to_file(&config_path) {
            tracing::warn!("Failed to save theme to config: {}", e);
        }
    }

    /// Start the keybinding map selection prompt with available maps
    fn start_select_keybinding_map_prompt(&mut self) {
        // Built-in keybinding maps
        let builtin_maps = vec!["default", "emacs", "vscode"];

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
        let is_builtin = matches!(map_name, "default" | "emacs" | "vscode");
        let is_user_defined = self.config.keybinding_maps.contains_key(map_name);

        if is_builtin || is_user_defined {
            // Update the active keybinding map in config
            self.config.active_keybinding_map = map_name.to_string().into();

            // Reload the keybinding resolver with the new map
            self.keybindings = crate::input::keybindings::KeybindingResolver::new(&self.config);

            // Persist to config file
            self.save_keybinding_map_to_config();

            self.set_status_message(format!("Switched to '{}' keybindings", map_name));
        } else {
            self.set_status_message(format!("Unknown keybinding map: '{}'", map_name));
        }
    }

    /// Save the current keybinding map setting to the user's config file
    fn save_keybinding_map_to_config(&mut self) {
        // Create the directory if it doesn't exist
        if let Err(e) = std::fs::create_dir_all(&self.dir_context.config_dir) {
            tracing::warn!("Failed to create config directory: {}", e);
            return;
        }

        // Save the config
        let config_path = self.dir_context.config_path();
        if let Err(e) = self.config.save_to_file(&config_path) {
            tracing::warn!("Failed to save keybinding map to config: {}", e);
        }
    }

    /// Switch to the previously active tab in the current split
    fn switch_to_previous_tab(&mut self) {
        let active_split = self.split_manager.active_split();
        let previous_buffer = self
            .split_view_states
            .get(&active_split)
            .and_then(|vs| vs.previous_buffer);

        if let Some(prev_id) = previous_buffer {
            // Verify the buffer is still open in this split
            let is_valid = self
                .split_view_states
                .get(&active_split)
                .is_some_and(|vs| vs.open_buffers.contains(&prev_id));

            if is_valid && prev_id != self.active_buffer() {
                // Save current position before switching
                self.position_history.commit_pending_movement();

                let current_state = self.active_state();
                let position = current_state.cursors.primary().position;
                let anchor = current_state.cursors.primary().anchor;
                self.position_history
                    .record_movement(self.active_buffer(), position, anchor);
                self.position_history.commit_pending_movement();

                self.set_active_buffer(prev_id);
            } else if !is_valid {
                self.set_status_message("Previous tab is no longer open".to_string());
            }
        } else {
            self.set_status_message("No previous tab".to_string());
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
            self.set_status_message("No tabs open in current split".to_string());
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
            self.set_status_message("Tab not found in current split".to_string());
            return;
        }

        if buffer_id != self.active_buffer() {
            // Save current position before switching
            self.position_history.commit_pending_movement();

            let current_state = self.active_state();
            let position = current_state.cursors.primary().position;
            let anchor = current_state.cursors.primary().anchor;
            self.position_history
                .record_movement(self.active_buffer(), position, anchor);
            self.position_history.commit_pending_movement();

            self.set_active_buffer(buffer_id);
        }
    }

    /// Handle character insertion in prompt mode.
    fn handle_insert_char_prompt(&mut self, c: char) -> std::io::Result<()> {
        // Check if this is the query-replace confirmation prompt
        if let Some(ref prompt) = self.prompt {
            if prompt.prompt_type == PromptType::QueryReplaceConfirm {
                return self.handle_interactive_replace_key(c);
            }
        }

        // Reset history navigation when user starts typing
        // This allows them to press Up to get back to history items
        if let Some(ref prompt) = self.prompt {
            match &prompt.prompt_type {
                PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch => {
                    self.search_history.reset_navigation();
                }
                PromptType::Replace { .. } | PromptType::QueryReplace { .. } => {
                    self.replace_history.reset_navigation();
                }
                _ => {}
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
    fn handle_insert_char_editor(&mut self, c: char) -> std::io::Result<()> {
        // Check if editing is disabled (show_cursors = false)
        if self.is_editing_disabled() {
            self.set_status_message("Editing disabled in this buffer".to_string());
            return Ok(());
        }

        // Cancel any pending LSP requests since the text is changing
        self.cancel_pending_lsp_requests();

        if let Some(events) = self.action_to_events(Action::InsertChar(c)) {
            // Wrap multiple events (multi-cursor) in a Batch for atomic undo
            if events.len() > 1 {
                let batch = Event::Batch {
                    events: events.clone(),
                    description: format!("Insert '{}'", c),
                };
                self.active_event_log_mut().append(batch.clone());
                self.apply_event_to_active_buffer(&batch);
            } else {
                // Single cursor - no need for batch
                for event in events {
                    self.active_event_log_mut().append(event.clone());
                    self.apply_event_to_active_buffer(&event);
                }
            }
        }

        // Auto-trigger signature help on '(' and ','
        if c == '(' || c == ',' {
            let _ = self.request_signature_help();
        }

        Ok(())
    }
}
