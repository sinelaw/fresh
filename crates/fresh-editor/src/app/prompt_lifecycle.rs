//! Prompt/minibuffer lifecycle on `Editor`.
//!
//! Starting/canceling/confirming prompts, scrolling suggestions,
//! managing prompt history per type, building suggestion lists, plus
//! the file-open/quick-open prompt setup helpers.

use std::path::PathBuf;
use std::sync::{Arc, RwLock};

use rust_i18n::t;

use crate::input::command_registry::CommandRegistry;
use crate::input::commands::Suggestion;
use crate::input::keybindings::KeyContext;
use crate::input::quick_open::{BufferInfo, QuickOpenContext};
use crate::services::async_bridge::AsyncMessage;
use crate::services::plugins::PluginManager;
use crate::view::prompt::{Prompt, PromptType};

use super::file_open;
use super::Editor;

impl Editor {
    // Prompt/Minibuffer control methods

    /// Start a new prompt (enter minibuffer mode)
    pub fn start_prompt(&mut self, message: String, prompt_type: PromptType) {
        self.start_prompt_with_suggestions(message, prompt_type, Vec::new());
    }

    /// Start a search prompt with an optional selection scope
    ///
    /// When `use_selection_range` is true and a single-line selection is present,
    /// the search will be restricted to that range once confirmed.
    pub(super) fn start_search_prompt(
        &mut self,
        message: String,
        prompt_type: PromptType,
        use_selection_range: bool,
    ) {
        // Reset any previously stored selection range
        self.pending_search_range = None;

        let selection_range = self.active_cursors().primary().selection_range();

        let selected_text = if let Some(range) = selection_range.clone() {
            let state = self.active_state_mut();
            let text = state.get_text_range(range.start, range.end);
            if !text.contains('\n') && !text.is_empty() {
                Some(text)
            } else {
                None
            }
        } else {
            None
        };

        if use_selection_range {
            self.pending_search_range = selection_range;
        }

        // Determine the default text: selection > last history > empty
        let from_history = selected_text.is_none();
        let default_text = selected_text.or_else(|| {
            self.get_prompt_history("search")
                .and_then(|h| h.last().map(|s| s.to_string()))
        });

        // Start the prompt
        self.start_prompt(message, prompt_type);

        // Pre-fill with default text if available
        if let Some(text) = default_text {
            if let Some(ref mut prompt) = self.prompt {
                prompt.set_input(text.clone());
                prompt.selection_anchor = Some(0);
                prompt.cursor_pos = text.len();
            }
            if from_history {
                self.get_or_create_prompt_history("search").init_at_last();
            }
            self.update_search_highlights(&text);
        }
    }

    /// Start a new prompt with autocomplete suggestions
    pub fn start_prompt_with_suggestions(
        &mut self,
        message: String,
        prompt_type: PromptType,
        suggestions: Vec<Suggestion>,
    ) {
        // Dismiss transient popups and clear hover state when opening a prompt
        self.on_editor_focus_lost();

        // Clear search highlights when starting a new search prompt
        // This ensures old highlights from previous searches don't persist
        match prompt_type {
            PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch => {
                self.clear_search_highlights();
            }
            _ => {}
        }

        // Check if we need to update suggestions after creating the prompt
        let needs_suggestions = matches!(
            prompt_type,
            PromptType::OpenFile | PromptType::SwitchProject | PromptType::SaveFileAs
        );

        self.prompt = Some(Prompt::with_suggestions(message, prompt_type, suggestions));

        // For file and command prompts, populate initial suggestions
        if needs_suggestions {
            self.update_prompt_suggestions();
        }
    }

    /// Start a new prompt with initial text
    pub fn start_prompt_with_initial_text(
        &mut self,
        message: String,
        prompt_type: PromptType,
        initial_text: String,
    ) {
        // Dismiss transient popups and clear hover state when opening a prompt
        self.on_editor_focus_lost();

        self.prompt = Some(Prompt::with_initial_text(
            message,
            prompt_type,
            initial_text,
        ));
    }

    /// Start Quick Open prompt with command palette as default
    pub fn start_quick_open(&mut self) {
        self.start_quick_open_with_prefix(">");
    }

    /// Start Quick Open prompt with specified prefix
    pub fn start_quick_open_with_prefix(&mut self, prefix: &str) {
        self.on_editor_focus_lost();
        self.status_message = None;
        self.goto_line_preview = None;

        let mut prompt = Prompt::with_suggestions(String::new(), PromptType::QuickOpen, vec![]);
        prompt.input = prefix.to_string();
        prompt.cursor_pos = prefix.len();
        self.prompt = Some(prompt);

        self.update_quick_open_suggestions(prefix);
    }

    /// Build a QuickOpenContext from current editor state
    pub(super) fn build_quick_open_context(&self) -> QuickOpenContext {
        let open_buffers = self
            .buffers
            .iter()
            .filter_map(|(buffer_id, state)| {
                let path = state.buffer.file_path()?;
                let name = path
                    .file_name()
                    .map(|n| n.to_string_lossy().to_string())
                    .unwrap_or_else(|| format!("Buffer {}", buffer_id.0));
                Some(BufferInfo {
                    id: buffer_id.0,
                    path: path.display().to_string(),
                    name,
                    modified: state.buffer.is_modified(),
                })
            })
            .collect();

        let has_lsp_config = {
            let language = self
                .buffers
                .get(&self.active_buffer())
                .map(|s| s.language.as_str());
            language
                .and_then(|lang| self.lsp.as_ref().and_then(|lsp| lsp.get_config(lang)))
                .is_some()
        };

        QuickOpenContext {
            cwd: self.working_dir.display().to_string(),
            open_buffers,
            active_buffer_id: self.active_buffer().0,
            active_buffer_path: self
                .active_state()
                .buffer
                .file_path()
                .map(|p| p.display().to_string()),
            has_selection: self.has_active_selection(),
            key_context: self.key_context.clone(),
            custom_contexts: self.active_custom_contexts.clone(),
            buffer_mode: self
                .buffer_metadata
                .get(&self.active_buffer())
                .and_then(|m| m.virtual_mode())
                .map(|s| s.to_string()),
            has_lsp_config,
            relative_line_numbers: self.config.editor.relative_line_numbers,
        }
    }

    /// Update Quick Open suggestions based on current input, dispatching through the registry
    pub(super) fn update_quick_open_suggestions(&mut self, input: &str) {
        let context = self.build_quick_open_context();
        let suggestions = if let Some((provider, query)) =
            self.quick_open_registry.get_provider_for_input(input)
        {
            provider.suggestions(query, &context)
        } else {
            vec![]
        };

        if let Some(prompt) = &mut self.prompt {
            prompt.suggestions = suggestions;
            prompt.selected_suggestion = if prompt.suggestions.is_empty() {
                None
            } else {
                Some(0)
            };
        }

        // Live preview for the goto-line provider: if the input is ":<N>" for a
        // valid absolute line N, jump there now so the user sees the target as
        // they type (matches VSCode's Ctrl+P :<N> behavior). Otherwise, restore
        // the cursor to its pre-preview position.
        //
        // Relative input (`:+N`/`:-N`) is intentionally not previewed: the
        // target shifts on every digit typed, which is disorienting.
        let input = input.trim();
        let target = Self::parse_quick_open_goto_line_target(input);
        self.apply_goto_line_preview(target);
    }

    /// Parse a Quick Open input string for a `:<N>` goto-line preview target.
    /// Only absolute inputs are previewed; relative inputs return `None`.
    pub(super) fn parse_quick_open_goto_line_target(input: &str) -> Option<usize> {
        let rest = input.strip_prefix(':')?;
        match crate::input::quick_open::parse_goto_line_input(rest) {
            Some(crate::input::quick_open::GotoLineTarget::Absolute(n)) => Some(n),
            _ => None,
        }
    }

    /// Apply a live goto-line preview: jump to `target_line` (saving the
    /// original cursor on the first jump) if `Some`, or restore the saved
    /// cursor if `None`.
    ///
    /// Shared between Quick Open's `:N` syntax and the standalone `Goto Line`
    /// prompt, which differ only in how the target line is parsed from input.
    pub(super) fn apply_goto_line_preview(&mut self, target_line: Option<usize>) {
        if let Some(line) = target_line {
            self.save_goto_line_preview_snapshot();
            self.goto_line_col(line, None);
            // Record where the jump landed so restore can detect if the cursor
            // has since moved (e.g., mouse click, external buffer edit).
            let new_position = self.active_cursors().primary().position;
            if let Some(snap) = self.goto_line_preview.as_mut() {
                snap.last_jump_position = new_position;
            }
        } else {
            self.restore_goto_line_preview_snapshot();
        }
    }

    /// Save a snapshot of the active buffer's cursor and viewport so the
    /// goto-line preview can later restore it. No-op if a snapshot is already
    /// in place (the saved state should always be the pre-preview one).
    pub(super) fn save_goto_line_preview_snapshot(&mut self) {
        if self.goto_line_preview.is_some() {
            return;
        }

        let buffer_id = self.active_buffer();
        let split_id = self.split_manager.active_split();
        let (cursor_id, position, anchor, sticky_column) = {
            let cursors = self.active_cursors();
            let primary = cursors.primary();
            (
                cursors.primary_id(),
                primary.position,
                primary.anchor,
                primary.sticky_column,
            )
        };
        let (viewport_top_byte, viewport_top_view_line_offset, viewport_left_column) = {
            let vp = self.active_viewport();
            (vp.top_byte, vp.top_view_line_offset, vp.left_column)
        };

        self.goto_line_preview = Some(super::GotoLinePreviewSnapshot {
            buffer_id,
            split_id,
            cursor_id,
            position,
            anchor,
            sticky_column,
            viewport_top_byte,
            viewport_top_view_line_offset,
            viewport_left_column,
            // Before the first jump the cursor is still at the pre-preview
            // position; `apply_goto_line_preview` overwrites this with the
            // jump target immediately after calling `goto_line_col`.
            last_jump_position: position,
        });
    }

    /// If a goto-line preview snapshot exists, restore the active split's
    /// cursor and viewport to the saved state and clear the snapshot.
    ///
    /// The snapshot is only applied if the editor is still in exactly the
    /// state the last preview jump left it in: same active buffer, same split,
    /// cursor still at `last_jump_position`. Any deviation (user mouse-clicked,
    /// an async edit shifted the cursor, focus moved elsewhere, …) means the
    /// pre-preview state is stale and we simply discard the snapshot.
    pub(super) fn restore_goto_line_preview_snapshot(&mut self) {
        let Some(snap) = self.goto_line_preview.take() else {
            return;
        };

        // If the active buffer/split has changed (shouldn't happen during a
        // quick-open prompt, but be defensive), just drop the snapshot.
        if self.active_buffer() != snap.buffer_id
            || self.split_manager.active_split() != snap.split_id
        {
            return;
        }

        let cursors = self.active_cursors();
        let current = cursors.primary();

        // Cursor no longer where the preview left it → someone else moved it
        // (mouse click, external edit via `adjust_for_edit`, …). Drop without
        // restoring to avoid rubber-banding over that deliberate state.
        if current.position != snap.last_jump_position {
            return;
        }
        let event = crate::model::event::Event::MoveCursor {
            cursor_id: snap.cursor_id,
            old_position: current.position,
            new_position: snap.position,
            old_anchor: current.anchor,
            new_anchor: snap.anchor,
            old_sticky_column: current.sticky_column,
            new_sticky_column: snap.sticky_column,
        };

        let state = self.buffers.get_mut(&snap.buffer_id).unwrap();
        let view_state = self.split_view_states.get_mut(&snap.split_id).unwrap();
        state.apply(&mut view_state.cursors, &event);

        let vp = &mut view_state.viewport;
        vp.top_byte = snap.viewport_top_byte;
        vp.top_view_line_offset = snap.viewport_top_view_line_offset;
        vp.left_column = snap.viewport_left_column;
        // The cursor we just restored is already consistent with this
        // viewport; don't let ensure_visible re-scroll on the next render.
        vp.set_skip_ensure_visible();
    }

    /// Cancel search/replace prompts if one is active.
    /// Called when focus leaves the editor (e.g., switching buffers, focusing file explorer).
    pub(super) fn cancel_search_prompt_if_active(&mut self) {
        if let Some(ref prompt) = self.prompt {
            if matches!(
                prompt.prompt_type,
                PromptType::Search
                    | PromptType::ReplaceSearch
                    | PromptType::Replace { .. }
                    | PromptType::QueryReplaceSearch
                    | PromptType::QueryReplace { .. }
                    | PromptType::QueryReplaceConfirm
            ) {
                self.prompt = None;
                // Also cancel interactive replace if active
                self.interactive_replace_state = None;
                // Clear search highlights from current buffer
                let ns = self.search_namespace.clone();
                let state = self.active_state_mut();
                state.overlays.clear_namespace(&ns, &mut state.marker_list);
            }
        }
    }

    /// Pre-fill the Open File prompt input with the current buffer directory
    pub(super) fn prefill_open_file_prompt(&mut self) {
        // With the native file browser, the directory is shown from file_open_state.current_dir
        // in the prompt rendering. The prompt.input is just the filter/filename, so we
        // start with an empty input.
        if let Some(prompt) = self.prompt.as_mut() {
            if prompt.prompt_type == PromptType::OpenFile {
                prompt.input.clear();
                prompt.cursor_pos = 0;
                prompt.selection_anchor = None;
            }
        }
    }

    /// Initialize the file open dialog state
    ///
    /// Called when the Open File prompt is started. Determines the initial directory
    /// (from current buffer's directory or working directory) and triggers async
    /// directory loading.
    pub(super) fn init_file_open_state(&mut self) {
        // Determine initial directory
        let buffer_id = self.active_buffer();

        // For terminal buffers, use the terminal's initial CWD or fall back to project root
        // This avoids showing the terminal backing file directory which is confusing for users
        let initial_dir = if self.is_terminal_buffer(buffer_id) {
            self.get_terminal_id(buffer_id)
                .and_then(|tid| self.terminal_manager.get(tid))
                .and_then(|handle| handle.cwd())
                .unwrap_or_else(|| self.working_dir.clone())
        } else {
            self.active_state()
                .buffer
                .file_path()
                .and_then(|path| path.parent())
                .map(|p| p.to_path_buf())
                .unwrap_or_else(|| self.working_dir.clone())
        };

        // Create the file open state with config-based show_hidden setting
        let show_hidden = self.config.file_browser.show_hidden;
        self.file_open_state = Some(file_open::FileOpenState::new(
            initial_dir.clone(),
            show_hidden,
            self.authority.filesystem.clone(),
        ));

        // Start async directory loading and async shortcuts loading in parallel
        self.load_file_open_directory(initial_dir);
        self.load_file_open_shortcuts_async();
    }

    /// Initialize the folder open dialog state
    ///
    /// Called when the Switch Project prompt is started. Starts from the current working
    /// directory and triggers async directory loading.
    pub(super) fn init_folder_open_state(&mut self) {
        // Start from the current working directory
        let initial_dir = self.working_dir.clone();

        // Create the file open state with config-based show_hidden setting
        let show_hidden = self.config.file_browser.show_hidden;
        self.file_open_state = Some(file_open::FileOpenState::new(
            initial_dir.clone(),
            show_hidden,
            self.authority.filesystem.clone(),
        ));

        // Start async directory loading and async shortcuts loading in parallel
        self.load_file_open_directory(initial_dir);
        self.load_file_open_shortcuts_async();
    }

    /// Change the working directory to a new path
    ///
    /// This requests a full editor restart with the new working directory.
    /// The main loop will drop the current editor instance and create a fresh
    /// one pointing to the new directory. This ensures:
    /// - All buffers are cleanly closed
    /// - LSP servers are properly shut down and restarted with new root
    /// - Plugins are cleanly restarted
    /// - No state leaks between projects
    pub fn change_working_dir(&mut self, new_path: PathBuf) {
        // Canonicalize the path to resolve symlinks and normalize
        let new_path = new_path.canonicalize().unwrap_or(new_path);

        // Request a restart with the new working directory
        // The main loop will handle creating a fresh editor instance
        self.request_restart(new_path);
    }

    /// Load directory contents for the file open dialog
    pub(super) fn load_file_open_directory(&mut self, path: PathBuf) {
        // Update state to loading
        if let Some(state) = &mut self.file_open_state {
            state.current_dir = path.clone();
            state.loading = true;
            state.error = None;
            state.update_shortcuts();
        }

        // Use tokio runtime to load directory
        if let Some(ref runtime) = self.tokio_runtime {
            let fs_manager = self.fs_manager.clone();
            let sender = self.async_bridge.as_ref().map(|b| b.sender());

            runtime.spawn(async move {
                let result = fs_manager.list_dir_with_metadata(path).await;
                if let Some(sender) = sender {
                    // Receiver may have been dropped if the dialog was closed.
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = sender.send(AsyncMessage::FileOpenDirectoryLoaded(result));
                }
            });
        } else {
            // No runtime, set error
            if let Some(state) = &mut self.file_open_state {
                state.set_error("Async runtime not available".to_string());
            }
        }
    }

    /// Handle file open directory load result
    pub(super) fn handle_file_open_directory_loaded(
        &mut self,
        result: std::io::Result<Vec<crate::services::fs::DirEntry>>,
    ) {
        match result {
            Ok(entries) => {
                if let Some(state) = &mut self.file_open_state {
                    state.set_entries(entries);
                }
                // Re-apply filter from prompt (entries were just loaded, filter needs to select matching entry)
                let filter = self
                    .prompt
                    .as_ref()
                    .map(|p| p.input.clone())
                    .unwrap_or_default();
                if !filter.is_empty() {
                    if let Some(state) = &mut self.file_open_state {
                        state.apply_filter(&filter);
                    }
                }
            }
            Err(e) => {
                if let Some(state) = &mut self.file_open_state {
                    state.set_error(e.to_string());
                }
            }
        }
    }

    /// Load async shortcuts (documents, downloads, Windows drive letters) in the background.
    /// This prevents the UI from hanging when checking paths that may be slow or unreachable.
    /// See issue #903.
    pub(super) fn load_file_open_shortcuts_async(&mut self) {
        if let Some(ref runtime) = self.tokio_runtime {
            let filesystem = self.authority.filesystem.clone();
            let sender = self.async_bridge.as_ref().map(|b| b.sender());

            runtime.spawn(async move {
                // Run the blocking filesystem checks in a separate thread
                let shortcuts = tokio::task::spawn_blocking(move || {
                    file_open::FileOpenState::build_shortcuts_async(&*filesystem)
                })
                .await
                .unwrap_or_default();

                if let Some(sender) = sender {
                    // Receiver may have been dropped if the dialog was closed.
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = sender.send(AsyncMessage::FileOpenShortcutsLoaded(shortcuts));
                }
            });
        }
    }

    /// Handle async shortcuts load result
    pub(super) fn handle_file_open_shortcuts_loaded(
        &mut self,
        shortcuts: Vec<file_open::NavigationShortcut>,
    ) {
        if let Some(state) = &mut self.file_open_state {
            state.merge_async_shortcuts(shortcuts);
        }
    }

    /// Tear down the standalone preview state used by the Live Grep
    /// floating overlay (issue #1796). Drops the inline view state
    /// and closes any buffers we loaded purely for preview (buffers
    /// the user already had open are left untouched).
    pub(crate) fn cleanup_overlay_preview(&mut self) {
        let to_close: Vec<crate::model::event::BufferId> =
            if let Some(state) = self.overlay_preview_state.take() {
                state.loaded_buffers.into_iter().collect()
            } else {
                Vec::new()
            };
        for buffer_id in to_close {
            // close_buffer is the user-facing close (errors on
            // unsaved changes). Preview-loaded buffers are read-only
            // / unmodified by definition, so this should always
            // succeed. Tolerate failure silently — leaving an extra
            // hidden buffer around is preferable to crashing.
            if let Err(e) = self.close_buffer(buffer_id) {
                tracing::warn!("Failed to close overlay preview buffer: {}", e);
            }
        }
    }

    /// Snapshot the current prompt's suggestions as a list of
    /// `GrepMatch` records, so Resume can re-display them without
    /// re-running ripgrep and Quickfix export can hand them to the
    /// Utility Dock. Parses each suggestion's text as
    /// `path:line[:col]` (the format the live_grep finder emits).
    pub(crate) fn snapshot_prompt_results_for_grep(
        &self,
        prompt: &crate::view::prompt::Prompt,
    ) -> Vec<crate::services::live_grep_state::GrepMatch> {
        use crate::input::quick_open::parse_path_line_col;
        // Suggestions emitted by the Finder library use `value` as an
        // opaque index (`"0"`, `"1"`, …) and put `path:line[:col]` in
        // `text`. Parse `text` first; fall back to `value` only if
        // `text` lacks a path-shaped segment (a Resume-replay where
        // we previously stored `path:line:col` in `value` directly).
        prompt
            .suggestions
            .iter()
            .filter(|s| !s.disabled)
            .filter_map(|s| {
                let from_text = parse_path_line_col(&s.text);
                let (file, line, column) = if !from_text.0.is_empty() && from_text.1.is_some() {
                    from_text
                } else if let Some(v) = s.value.as_deref() {
                    parse_path_line_col(v)
                } else {
                    from_text
                };
                if file.is_empty() {
                    return None;
                }
                Some(crate::services::live_grep_state::GrepMatch {
                    file,
                    line: line.unwrap_or(1),
                    column: column.unwrap_or(1),
                    content: s.description.clone().unwrap_or_default(),
                })
            })
            .collect()
    }

    /// Cancel the current prompt and return to normal mode
    pub fn cancel_prompt(&mut self) {
        // Extract theme to restore if this is a SelectTheme prompt
        let theme_to_restore = if let Some(ref prompt) = self.prompt {
            if let PromptType::SelectTheme { original_theme } = &prompt.prompt_type {
                Some(original_theme.clone())
            } else {
                None
            }
        } else {
            None
        };

        // Determine prompt type and reset appropriate history navigation
        if let Some(ref prompt) = self.prompt {
            // Reset history navigation for this prompt type
            if let Some(key) = Self::prompt_type_to_history_key(&prompt.prompt_type) {
                if let Some(history) = self.prompt_histories.get_mut(&key) {
                    history.reset_navigation();
                }
            }
            match &prompt.prompt_type {
                PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch => {
                    self.clear_search_highlights();
                }
                PromptType::Plugin { custom_type } => {
                    // Fire plugin hook for prompt cancellation
                    use crate::services::plugins::hooks::HookArgs;
                    self.plugin_manager.run_hook(
                        "prompt_cancelled",
                        HookArgs::PromptCancelled {
                            prompt_type: custom_type.clone(),
                            input: prompt.input.clone(),
                        },
                    );
                    // Capture Live Grep state on cancel for Resume
                    // (Action::ResumeLiveGrep) — reads from
                    // editor.live_grep_last_state. Detection by
                    // custom_type rather than dedicated PromptType
                    // because the live_grep plugin drives the prompt.
                    if custom_type == "live-grep" {
                        let cached = self.snapshot_prompt_results_for_grep(prompt);
                        self.live_grep_last_state =
                            Some(crate::services::live_grep_state::LiveGrepLastState {
                                query: prompt.input.clone(),
                                selected_index: prompt.selected_suggestion,
                                cached_results: Some(cached),
                                cached_at: Some(std::time::Instant::now()),
                                last_results_snapshot_id: None,
                            });
                    }
                }
                PromptType::LiveGrep => {
                    let cached = self.snapshot_prompt_results_for_grep(prompt);
                    self.live_grep_last_state =
                        Some(crate::services::live_grep_state::LiveGrepLastState {
                            query: prompt.input.clone(),
                            selected_index: prompt.selected_suggestion,
                            cached_results: Some(cached),
                            cached_at: Some(std::time::Instant::now()),
                            last_results_snapshot_id: None,
                        });
                }
                PromptType::LspRename { overlay_handle, .. } => {
                    // Remove the rename overlay when cancelling
                    let remove_overlay_event = crate::model::event::Event::RemoveOverlay {
                        handle: overlay_handle.clone(),
                    };
                    self.apply_event_to_active_buffer(&remove_overlay_event);
                }
                PromptType::OpenFile | PromptType::SwitchProject | PromptType::SaveFileAs => {
                    // Clear file browser state
                    self.file_open_state = None;
                    self.file_browser_layout = None;
                }
                PromptType::AsyncPrompt => {
                    // Resolve the pending async prompt callback with null (cancelled)
                    if let Some(callback_id) = self.pending_async_prompt_callback.take() {
                        self.plugin_manager
                            .resolve_callback(callback_id, "null".to_string());
                    }
                }
                PromptType::QuickOpen => {
                    // Cancel any in-progress background file loading
                    if let Some((provider, _)) = self.quick_open_registry.get_provider_for_input("")
                    {
                        if let Some(fp) = provider
                            .as_any()
                            .downcast_ref::<crate::input::quick_open::providers::FileProvider>(
                        ) {
                            fp.cancel_loading();
                        }
                    }
                    // Undo any live goto-line preview so the cursor returns to
                    // where it was before the prompt was opened.
                    self.restore_goto_line_preview_snapshot();
                }
                PromptType::GotoLine => {
                    // Undo any live goto-line preview so the cursor returns to
                    // where it was before the prompt was opened.
                    self.restore_goto_line_preview_snapshot();
                }
                _ => {}
            }
        }

        // If we're closing a floating-overlay prompt (Live Grep,
        // issue #1796), tear down the phantom preview leaf and close
        // any buffers we loaded purely to feed the preview pane. The
        // user's split tree and originally-open buffers are
        // untouched.
        let was_overlay = self.prompt.as_ref().is_some_and(|p| p.overlay);
        if was_overlay {
            self.cleanup_overlay_preview();
        }

        self.prompt = None;
        self.pending_search_range = None;
        self.status_message = Some(t!("search.cancelled").to_string());

        // Restore original theme if we were in SelectTheme prompt
        if let Some(original_theme) = theme_to_restore {
            self.preview_theme(&original_theme);
        }
    }

    /// Handle mouse wheel scroll in prompt with suggestions.
    /// Returns true if scroll was handled, false if no prompt is active or has no suggestions.
    pub fn handle_prompt_scroll(&mut self, delta: i32) -> bool {
        if let Some(ref mut prompt) = self.prompt {
            if prompt.suggestions.is_empty() {
                return false;
            }

            let current = prompt.selected_suggestion.unwrap_or(0);
            let len = prompt.suggestions.len();

            // Calculate new position based on scroll direction
            // delta < 0 = scroll up, delta > 0 = scroll down
            let new_selected = if delta < 0 {
                // Scroll up - move selection up (decrease index)
                current.saturating_sub((-delta) as usize)
            } else {
                // Scroll down - move selection down (increase index)
                (current + delta as usize).min(len.saturating_sub(1))
            };

            prompt.selected_suggestion = Some(new_selected);

            // Update input to match selected suggestion for non-plugin prompts
            if !matches!(prompt.prompt_type, PromptType::Plugin { .. }) {
                if let Some(suggestion) = prompt.suggestions.get(new_selected) {
                    prompt.input = suggestion.get_value().to_string();
                    prompt.cursor_pos = prompt.input.len();
                }
            }

            return true;
        }
        false
    }

    /// Get the confirmed input and prompt type, consuming the prompt
    /// For command palette, returns the selected suggestion if available, otherwise the raw input
    /// Returns (input, prompt_type, selected_index)
    /// Returns None if trying to confirm a disabled command
    pub fn confirm_prompt(&mut self) -> Option<(String, PromptType, Option<usize>)> {
        if let Some(prompt) = self.prompt.take() {
            // Tear down the floating-overlay preview state on
            // confirm too — the user is committing to a result and
            // navigating to it, so the preview-only buffers should
            // be cleaned up the same way they are on cancel.
            if prompt.overlay {
                self.cleanup_overlay_preview();
            }
            let selected_index = prompt.selected_suggestion;
            // For prompts with suggestions, prefer the selected suggestion over raw input
            let mut final_input = if prompt.sync_input_on_navigate {
                // When sync_input_on_navigate is set, the input field is kept in sync
                // with the selected suggestion, so always use the input value
                prompt.input.clone()
            } else if matches!(
                prompt.prompt_type,
                PromptType::OpenFile
                    | PromptType::SwitchProject
                    | PromptType::SaveFileAs
                    | PromptType::StopLspServer
                    | PromptType::RestartLspServer
                    | PromptType::SelectTheme { .. }
                    | PromptType::SelectLocale
                    | PromptType::SwitchToTab
                    | PromptType::SetLanguage
                    | PromptType::SetEncoding
                    | PromptType::SetLineEnding
                    | PromptType::Plugin { .. }
            ) {
                // Use the selected suggestion if any
                if let Some(selected_idx) = prompt.selected_suggestion {
                    if let Some(suggestion) = prompt.suggestions.get(selected_idx) {
                        // Don't confirm disabled suggestions
                        if suggestion.disabled {
                            self.set_status_message(
                                t!(
                                    "error.command_not_available",
                                    command = suggestion.text.clone()
                                )
                                .to_string(),
                            );
                            return None;
                        }
                        // Use the selected suggestion value
                        suggestion.get_value().to_string()
                    } else {
                        prompt.input.clone()
                    }
                } else {
                    prompt.input.clone()
                }
            } else {
                prompt.input.clone()
            };

            // For StopLspServer/RestartLspServer, validate that the input matches a suggestion
            if matches!(
                prompt.prompt_type,
                PromptType::StopLspServer | PromptType::RestartLspServer
            ) {
                let is_valid = prompt
                    .suggestions
                    .iter()
                    .any(|s| s.text == final_input || s.get_value() == final_input);
                if !is_valid {
                    // Restore the prompt and don't confirm
                    self.prompt = Some(prompt);
                    self.set_status_message(
                        t!("error.no_lsp_match", input = final_input.clone()).to_string(),
                    );
                    return None;
                }
            }

            // For RemoveRuler, validate input against the suggestion list.
            // If the user typed text, it must match a suggestion value to be accepted.
            // If the input is empty, the pre-selected suggestion is used.
            if matches!(prompt.prompt_type, PromptType::RemoveRuler) {
                if prompt.input.is_empty() {
                    // No typed text — use the selected suggestion
                    if let Some(selected_idx) = prompt.selected_suggestion {
                        if let Some(suggestion) = prompt.suggestions.get(selected_idx) {
                            final_input = suggestion.get_value().to_string();
                        }
                    } else {
                        self.prompt = Some(prompt);
                        return None;
                    }
                } else {
                    // User typed text — it must match a suggestion value
                    let typed = prompt.input.trim().to_string();
                    let matched = prompt.suggestions.iter().find(|s| s.get_value() == typed);
                    if let Some(suggestion) = matched {
                        final_input = suggestion.get_value().to_string();
                    } else {
                        // Typed text doesn't match any ruler — reject
                        self.prompt = Some(prompt);
                        return None;
                    }
                }
            }

            // Add to appropriate history based on prompt type
            if let Some(key) = Self::prompt_type_to_history_key(&prompt.prompt_type) {
                let history = self.get_or_create_prompt_history(&key);
                history.push(final_input.clone());
                history.reset_navigation();
            }

            Some((final_input, prompt.prompt_type, selected_index))
        } else {
            None
        }
    }

    /// Check if currently in prompt mode
    pub fn is_prompting(&self) -> bool {
        self.prompt.is_some()
    }

    /// Get or create a prompt history for the given key
    pub(super) fn get_or_create_prompt_history(
        &mut self,
        key: &str,
    ) -> &mut crate::input::input_history::InputHistory {
        self.prompt_histories.entry(key.to_string()).or_default()
    }

    /// Get a prompt history for the given key (immutable)
    pub(super) fn get_prompt_history(
        &self,
        key: &str,
    ) -> Option<&crate::input::input_history::InputHistory> {
        self.prompt_histories.get(key)
    }

    /// Get the history key for a prompt type
    pub(super) fn prompt_type_to_history_key(
        prompt_type: &crate::view::prompt::PromptType,
    ) -> Option<String> {
        use crate::view::prompt::PromptType;
        match prompt_type {
            PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch => {
                Some("search".to_string())
            }
            PromptType::Replace { .. } | PromptType::QueryReplace { .. } => {
                Some("replace".to_string())
            }
            PromptType::GotoLine => Some("goto_line".to_string()),
            PromptType::Plugin { custom_type } => Some(format!("plugin:{}", custom_type)),
            _ => None,
        }
    }

    /// Get the current global editor mode (e.g., "vi-normal", "vi-insert")
    /// Returns None if no special mode is active
    pub fn editor_mode(&self) -> Option<String> {
        self.editor_mode.clone()
    }

    /// Get access to the command registry
    pub fn command_registry(&self) -> &Arc<RwLock<CommandRegistry>> {
        &self.command_registry
    }

    /// Get access to the plugin manager
    pub fn plugin_manager(&self) -> &PluginManager {
        &self.plugin_manager
    }

    /// Get mutable access to the plugin manager
    pub fn plugin_manager_mut(&mut self) -> &mut PluginManager {
        &mut self.plugin_manager
    }

    /// Check if file explorer has focus
    pub fn file_explorer_is_focused(&self) -> bool {
        self.key_context == KeyContext::FileExplorer
    }

    /// Get current prompt input (for display)
    pub fn prompt_input(&self) -> Option<&str> {
        self.prompt.as_ref().map(|p| p.input.as_str())
    }

    /// Check if the active cursor currently has a selection
    pub fn has_active_selection(&self) -> bool {
        self.active_cursors().primary().selection_range().is_some()
    }

    /// Get mutable reference to prompt (for input handling)
    pub fn prompt_mut(&mut self) -> Option<&mut Prompt> {
        self.prompt.as_mut()
    }

    /// Set a status message to display in the status bar
    pub fn set_status_message(&mut self, message: String) {
        tracing::info!(target: "status", "{}", message);
        self.plugin_status_message = None;
        self.status_message = Some(message);
    }

    /// Get the current status message
    pub fn get_status_message(&self) -> Option<&String> {
        self.plugin_status_message
            .as_ref()
            .or(self.status_message.as_ref())
    }

    /// Get accumulated plugin errors (for test assertions)
    /// Returns all error messages that were detected in plugin status messages
    pub fn get_plugin_errors(&self) -> &[String] {
        &self.plugin_errors
    }

    /// Clear accumulated plugin errors
    pub fn clear_plugin_errors(&mut self) {
        self.plugin_errors.clear();
    }

    /// Update prompt suggestions based on current input
    pub fn update_prompt_suggestions(&mut self) {
        // Extract prompt type and input to avoid borrow checker issues
        let (prompt_type, input) = if let Some(prompt) = &self.prompt {
            (prompt.prompt_type.clone(), prompt.input.clone())
        } else {
            return;
        };

        match prompt_type {
            PromptType::QuickOpen => {
                // Update Quick Open suggestions based on prefix
                self.update_quick_open_suggestions(&input);
            }
            PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch => {
                // Update incremental search highlights as user types
                self.update_search_highlights(&input);
                // Reset history navigation when user types - allows Up to navigate history
                if let Some(history) = self.prompt_histories.get_mut("search") {
                    history.reset_navigation();
                }
            }
            PromptType::Replace { .. } | PromptType::QueryReplace { .. } => {
                // Reset history navigation when user types - allows Up to navigate history
                if let Some(history) = self.prompt_histories.get_mut("replace") {
                    history.reset_navigation();
                }
            }
            PromptType::GotoLine => {
                // Reset history navigation when user types - allows Up to navigate Up arrow history
                if let Some(history) = self.prompt_histories.get_mut("goto_line") {
                    history.reset_navigation();
                }
                // Live preview for absolute line numbers only. Signed
                // (`+N`/`-N`) inputs are relative, and previewing them as the
                // user types each digit is disorienting — preview only on
                // Enter for those.
                let target = match crate::input::quick_open::parse_goto_line_input(input.trim()) {
                    Some(crate::input::quick_open::GotoLineTarget::Absolute(n)) => Some(n),
                    _ => None,
                };
                self.apply_goto_line_preview(target);
            }
            PromptType::OpenFile | PromptType::SwitchProject | PromptType::SaveFileAs => {
                // For OpenFile/SwitchProject/SaveFileAs, update the file browser filter (native implementation)
                self.update_file_open_filter();
            }
            PromptType::Plugin { custom_type } => {
                // Reset history navigation when user types - allows Up to navigate history
                let key = format!("plugin:{}", custom_type);
                if let Some(history) = self.prompt_histories.get_mut(&key) {
                    history.reset_navigation();
                }
                // Fire plugin hook for prompt input change
                use crate::services::plugins::hooks::HookArgs;
                self.plugin_manager.run_hook(
                    "prompt_changed",
                    HookArgs::PromptChanged {
                        prompt_type: custom_type,
                        input,
                    },
                );
                // Apply fuzzy filtering if original_suggestions is set.
                // Note: filter_suggestions checks suggestions_set_for_input to skip
                // filtering if the plugin has already provided filtered results for
                // this input (handles the async race condition with run_hook).
                if let Some(prompt) = &mut self.prompt {
                    prompt.filter_suggestions(false);
                }
            }
            PromptType::SwitchToTab
            | PromptType::SelectTheme { .. }
            | PromptType::StopLspServer
            | PromptType::RestartLspServer
            | PromptType::SetLanguage
            | PromptType::SetEncoding
            | PromptType::SetLineEnding => {
                if let Some(prompt) = &mut self.prompt {
                    prompt.filter_suggestions(false);
                }
            }
            PromptType::SelectLocale => {
                // Locale selection also matches on description (language names)
                if let Some(prompt) = &mut self.prompt {
                    prompt.filter_suggestions(true);
                }
            }
            _ => {}
        }
    }
}
