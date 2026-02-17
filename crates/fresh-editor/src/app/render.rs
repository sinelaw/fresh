use super::*;
use anyhow::Result as AnyhowResult;
use rust_i18n::t;

impl Editor {
    /// Render the editor to the terminal
    pub fn render(&mut self, frame: &mut Frame) {
        let _span = tracing::trace_span!("render").entered();
        let size = frame.area();

        // For scroll sync groups, we need to update the active split's viewport position BEFORE
        // calling sync_scroll_groups, so that the sync reads the correct position.
        // Otherwise, cursor movements like 'G' (go to end) won't sync properly because
        // viewport.top_byte hasn't been updated yet.
        let active_split = self.split_manager.active_split();
        self.pre_sync_ensure_visible(active_split);

        // Synchronize scroll sync groups (anchor-based scroll for side-by-side diffs)
        // This sets viewport positions based on the authoritative scroll_line in each group
        self.sync_scroll_groups();

        // NOTE: Viewport sync with cursor is handled by split_rendering.rs which knows the
        // correct content area dimensions. Don't sync here with incorrect EditorState viewport size.

        // Prepare all buffers for rendering (pre-load viewport data for lazy loading)
        // Each split may have a different viewport position on the same buffer
        let mut semantic_ranges: std::collections::HashMap<BufferId, (usize, usize)> =
            std::collections::HashMap::new();
        for (split_id, view_state) in &self.split_view_states {
            if let Some(buffer_id) = self.split_manager.get_buffer_id(*split_id) {
                if let Some(state) = self.buffers.get(&buffer_id) {
                    let start_line = state.buffer.get_line_number(view_state.viewport.top_byte);
                    let visible_lines = view_state.viewport.visible_line_count().saturating_sub(1);
                    let end_line = start_line.saturating_add(visible_lines);
                    semantic_ranges
                        .entry(buffer_id)
                        .and_modify(|(min_start, max_end)| {
                            *min_start = (*min_start).min(start_line);
                            *max_end = (*max_end).max(end_line);
                        })
                        .or_insert((start_line, end_line));
                }
            }
        }
        for (buffer_id, (start_line, end_line)) in semantic_ranges {
            self.maybe_request_semantic_tokens_range(buffer_id, start_line, end_line);
            self.maybe_request_semantic_tokens_full_debounced(buffer_id);
        }

        for (split_id, view_state) in &self.split_view_states {
            if let Some(buffer_id) = self.split_manager.get_buffer_id(*split_id) {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    let top_byte = view_state.viewport.top_byte;
                    let height = view_state.viewport.height;
                    if let Err(e) = state.prepare_for_render(top_byte, height) {
                        tracing::error!("Failed to prepare buffer for render: {}", e);
                        // Continue with partial rendering
                    }
                }
            }
        }

        // Refresh search highlights only during incremental search (when prompt is active)
        // After search is confirmed, overlays exist for ALL matches and shouldn't be overwritten
        let is_search_prompt_active = self.prompt.as_ref().is_some_and(|p| {
            matches!(
                p.prompt_type,
                PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch
            )
        });
        if is_search_prompt_active {
            if let Some(ref search_state) = self.search_state {
                let query = search_state.query.clone();
                self.update_search_highlights(&query);
            }
        }

        // Determine if we need to show search options bar
        let show_search_options = self.prompt.as_ref().is_some_and(|p| {
            matches!(
                p.prompt_type,
                PromptType::Search
                    | PromptType::ReplaceSearch
                    | PromptType::Replace { .. }
                    | PromptType::QueryReplaceSearch
                    | PromptType::QueryReplace { .. }
            )
        });

        // Hide status bar when suggestions popup or file browser popup is shown
        let has_suggestions = self
            .prompt
            .as_ref()
            .is_some_and(|p| !p.suggestions.is_empty());
        let has_file_browser = self.prompt.as_ref().is_some_and(|p| {
            matches!(
                p.prompt_type,
                PromptType::OpenFile | PromptType::SwitchProject | PromptType::SaveFileAs
            )
        }) && self.file_open_state.is_some();

        // Build main vertical layout: [menu_bar, main_content, status_bar, search_options, prompt_line]
        // Status bar is hidden when suggestions popup is shown
        // Search options bar is shown when in search prompt
        let constraints = vec![
            Constraint::Length(if self.menu_bar_visible { 1 } else { 0 }), // Menu bar
            Constraint::Min(0),                                            // Main content area
            Constraint::Length(if has_suggestions || has_file_browser {
                0
            } else {
                1
            }), // Status bar (hidden with popups)
            Constraint::Length(if show_search_options { 1 } else { 0 }),   // Search options bar
            Constraint::Length(1), // Prompt line (always reserved)
        ];

        let main_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(constraints)
            .split(size);

        let menu_bar_area = main_chunks[0];
        let main_content_area = main_chunks[1];
        let status_bar_idx = 2;
        let search_options_idx = 3;
        let prompt_line_idx = 4;

        // Split main content area based on file explorer visibility
        // Also keep the layout split if a sync is in progress (to avoid flicker)
        let editor_content_area;
        let file_explorer_should_show = self.file_explorer_visible
            && (self.file_explorer.is_some() || self.file_explorer_sync_in_progress);

        if file_explorer_should_show {
            // Split horizontally: [file_explorer | editor]
            tracing::trace!(
                "render: file explorer layout active (present={}, sync_in_progress={})",
                self.file_explorer.is_some(),
                self.file_explorer_sync_in_progress
            );
            // Convert f32 percentage (0.0-1.0) to u16 percentage (0-100)
            let explorer_percent = (self.file_explorer_width_percent * 100.0) as u16;
            let editor_percent = 100 - explorer_percent;
            let horizontal_chunks = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([
                    Constraint::Percentage(explorer_percent), // File explorer
                    Constraint::Percentage(editor_percent),   // Editor area
                ])
                .split(main_content_area);

            self.cached_layout.file_explorer_area = Some(horizontal_chunks[0]);
            editor_content_area = horizontal_chunks[1];

            // Get remote connection info before mutable borrow of file_explorer
            let remote_connection = self.remote_connection_info().map(|s| s.to_string());

            // Render file explorer (only if we have it - during sync we just keep the area reserved)
            if let Some(ref mut explorer) = self.file_explorer {
                let is_focused = self.key_context == KeyContext::FileExplorer;

                // Build set of files with unsaved changes
                let mut files_with_unsaved_changes = std::collections::HashSet::new();
                for (buffer_id, state) in &self.buffers {
                    if state.buffer.is_modified() {
                        if let Some(metadata) = self.buffer_metadata.get(buffer_id) {
                            if let Some(file_path) = metadata.file_path() {
                                files_with_unsaved_changes.insert(file_path.clone());
                            }
                        }
                    }
                }

                let close_button_hovered = matches!(
                    &self.mouse_state.hover_target,
                    Some(HoverTarget::FileExplorerCloseButton)
                );
                FileExplorerRenderer::render(
                    explorer,
                    frame,
                    horizontal_chunks[0],
                    is_focused,
                    &files_with_unsaved_changes,
                    &self.file_explorer_decoration_cache,
                    &self.keybindings,
                    self.key_context,
                    &self.theme,
                    close_button_hovered,
                    remote_connection.as_deref(),
                );
            }
            // Note: if file_explorer is None but sync_in_progress is true,
            // we just leave the area blank (or could render a placeholder)
        } else {
            // No file explorer: use entire main content area for editor
            self.cached_layout.file_explorer_area = None;
            editor_content_area = main_content_area;
        }

        // Note: Tabs are now rendered within each split by SplitRenderer

        // Trigger lines_changed hooks for newly visible lines in all visible buffers
        // This allows plugins to add overlays before rendering
        // Only lines that haven't been seen before are sent (batched for efficiency)
        // Use non-blocking hooks to avoid deadlock when actions are awaiting
        if self.plugin_manager.is_active() {
            let hooks_start = std::time::Instant::now();
            // Get visible buffers and their areas
            let visible_buffers = self.split_manager.get_visible_buffers(editor_content_area);

            let mut total_new_lines = 0usize;
            for (split_id, buffer_id, split_area) in visible_buffers {
                // Get viewport from SplitViewState (the authoritative source)
                let viewport_top_byte = self
                    .split_view_states
                    .get(&split_id)
                    .map(|vs| vs.viewport.top_byte)
                    .unwrap_or(0);

                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    // Fire render_start hook once per buffer
                    self.plugin_manager.run_hook(
                        "render_start",
                        crate::services::plugins::hooks::HookArgs::RenderStart { buffer_id },
                    );

                    // Fire view_transform_request hook with base tokens
                    // This allows plugins to transform the view (e.g., soft breaks for markdown)
                    let visible_count = split_area.height as usize;
                    let is_binary = state.buffer.is_binary();
                    let line_ending = state.buffer.line_ending();
                    let base_tokens =
                        crate::view::ui::split_rendering::SplitRenderer::build_base_tokens_for_hook(
                            &mut state.buffer,
                            viewport_top_byte,
                            self.config.editor.estimated_line_length,
                            visible_count,
                            is_binary,
                            line_ending,
                        );
                    let viewport_start = viewport_top_byte;
                    let viewport_end = base_tokens
                        .last()
                        .and_then(|t| t.source_offset)
                        .unwrap_or(viewport_start);
                    let cursor_positions: Vec<usize> = self
                        .split_view_states
                        .get(&split_id)
                        .map(|vs| vs.cursors.iter().map(|(_, c)| c.position).collect())
                        .unwrap_or_default();
                    self.plugin_manager.run_hook(
                        "view_transform_request",
                        crate::services::plugins::hooks::HookArgs::ViewTransformRequest {
                            buffer_id,
                            split_id,
                            viewport_start,
                            viewport_end,
                            tokens: base_tokens,
                            cursor_positions,
                        },
                    );

                    // We just sent fresh base tokens to the plugin, so any
                    // future SubmitViewTransform from this request will be valid.
                    // Clear the stale flag so the response will be accepted.
                    if let Some(vs) = self.split_view_states.get_mut(&split_id) {
                        vs.view_transform_stale = false;
                    }

                    // Use the split area height as visible line count
                    let visible_count = split_area.height as usize;
                    let top_byte = viewport_top_byte;

                    // Get or create the seen byte ranges set for this buffer
                    let seen_byte_ranges = self.seen_byte_ranges.entry(buffer_id).or_default();

                    // Collect only NEW lines (not seen before based on byte range)
                    let mut new_lines: Vec<crate::services::plugins::hooks::LineInfo> = Vec::new();
                    let mut line_number = state.buffer.get_line_number(top_byte);
                    let mut iter = state
                        .buffer
                        .line_iterator(top_byte, self.config.editor.estimated_line_length);

                    for _ in 0..visible_count {
                        if let Some((line_start, line_content)) = iter.next_line() {
                            let byte_end = line_start + line_content.len();
                            let byte_range = (line_start, byte_end);

                            // Only add if this byte range hasn't been seen before
                            if !seen_byte_ranges.contains(&byte_range) {
                                new_lines.push(crate::services::plugins::hooks::LineInfo {
                                    line_number,
                                    byte_start: line_start,
                                    byte_end,
                                    content: line_content,
                                });
                                seen_byte_ranges.insert(byte_range);
                            }
                            line_number += 1;
                        } else {
                            break;
                        }
                    }

                    // Send batched hook if there are new lines
                    if !new_lines.is_empty() {
                        total_new_lines += new_lines.len();
                        self.plugin_manager.run_hook(
                            "lines_changed",
                            crate::services::plugins::hooks::HookArgs::LinesChanged {
                                buffer_id,
                                lines: new_lines,
                            },
                        );
                    }
                }
            }
            let hooks_elapsed = hooks_start.elapsed();
            tracing::trace!(
                new_lines = total_new_lines,
                elapsed_ms = hooks_elapsed.as_millis(),
                elapsed_us = hooks_elapsed.as_micros(),
                "lines_changed hooks total"
            );

            // Process any plugin commands (like AddOverlay) that resulted from the hooks.
            //
            // This is non-blocking: we collect whatever the plugin has sent so far.
            // The plugin thread runs in parallel, and because we proactively call
            // handle_refresh_lines after cursor_moved (in fire_cursor_hooks), the
            // lines_changed hook fires early in the render cycle. By the time we
            // reach this point, the plugin has typically already processed all hooks
            // and sent back conceal/overlay commands. On rare occasions (high CPU
            // load), the response arrives one frame late, which is imperceptible
            // at 60fps. The plugin's own refreshLines() call from cursor_moved
            // ensures a follow-up render cycle picks up any missed commands.
            let commands = self.plugin_manager.process_commands();
            if !commands.is_empty() {
                let cmd_names: Vec<String> =
                    commands.iter().map(|c| c.debug_variant_name()).collect();
                tracing::info!(count = commands.len(), cmds = ?cmd_names, "process_commands during render");
            }
            for command in commands {
                if let Err(e) = self.handle_plugin_command(command) {
                    tracing::error!("Error handling plugin command: {}", e);
                }
            }
        }

        // Render editor content (same for both layouts)
        let lsp_waiting = self.pending_completion_request.is_some()
            || self.pending_goto_definition_request.is_some();

        // Hide the hardware cursor when menu is open, file explorer is focused, terminal mode,
        // or settings UI is open
        // (the file explorer will set its own cursor position when focused)
        // (terminal mode renders its own cursor via the terminal emulator)
        // (settings UI is a modal that doesn't need the editor cursor)
        // This also causes visual cursor indicators in the editor to be dimmed
        let settings_visible = self.settings_state.as_ref().is_some_and(|s| s.visible);
        let hide_cursor = self.menu_state.active_menu.is_some()
            || self.key_context == KeyContext::FileExplorer
            || self.terminal_mode
            || settings_visible
            || self.keybinding_editor.is_some();

        // Convert HoverTarget to tab hover info for rendering
        let hovered_tab = match &self.mouse_state.hover_target {
            Some(HoverTarget::TabName(buffer_id, split_id)) => Some((*buffer_id, *split_id, false)),
            Some(HoverTarget::TabCloseButton(buffer_id, split_id)) => {
                Some((*buffer_id, *split_id, true))
            }
            _ => None,
        };

        // Get hovered close split button
        let hovered_close_split = match &self.mouse_state.hover_target {
            Some(HoverTarget::CloseSplitButton(split_id)) => Some(*split_id),
            _ => None,
        };

        // Get hovered maximize split button
        let hovered_maximize_split = match &self.mouse_state.hover_target {
            Some(HoverTarget::MaximizeSplitButton(split_id)) => Some(*split_id),
            _ => None,
        };

        let is_maximized = self.split_manager.is_maximized();

        let (
            split_areas,
            tab_layouts,
            close_split_areas,
            maximize_split_areas,
            view_line_mappings,
            horizontal_scrollbar_areas,
        ) = SplitRenderer::render_content(
            frame,
            editor_content_area,
            &self.split_manager,
            &mut self.buffers,
            &self.buffer_metadata,
            &mut self.event_logs,
            &self.composite_buffers,
            &mut self.composite_view_states,
            &self.theme,
            self.ansi_background.as_ref(),
            self.background_fade,
            lsp_waiting,
            self.config.editor.large_file_threshold_bytes,
            self.config.editor.line_wrap,
            self.config.editor.estimated_line_length,
            self.config.editor.highlight_context_bytes,
            Some(&mut self.split_view_states),
            hide_cursor,
            hovered_tab,
            hovered_close_split,
            hovered_maximize_split,
            is_maximized,
            self.config.editor.relative_line_numbers,
            self.tab_bar_visible,
            self.config.editor.use_terminal_bg,
            self.session_mode,
            self.config.editor.show_vertical_scrollbar,
            self.config.editor.show_horizontal_scrollbar,
        );

        // Detect viewport changes and fire hooks
        // Compare against previous frame's viewport state (stored in self.previous_viewports)
        // This correctly detects changes from scroll events that happen before render()
        if self.plugin_manager.is_active() {
            for (split_id, view_state) in &self.split_view_states {
                let current = (
                    view_state.viewport.top_byte,
                    view_state.viewport.width,
                    view_state.viewport.height,
                );
                // Compare against previous frame's state
                // Skip new splits (None case) - only fire hooks for established splits
                // This matches the original behavior where hooks only fire for splits
                // that existed at the start of render
                let (changed, previous) = match self.previous_viewports.get(split_id) {
                    Some(previous) => (*previous != current, Some(*previous)),
                    None => (false, None), // Skip new splits until they're established
                };
                tracing::trace!(
                    "viewport_changed check: split={:?} current={:?} previous={:?} changed={}",
                    split_id,
                    current,
                    previous,
                    changed
                );
                if changed {
                    if let Some(buffer_id) = self.split_manager.get_buffer_id(*split_id) {
                        tracing::debug!(
                            "Firing viewport_changed hook: split={:?} buffer={:?} top_byte={}",
                            split_id,
                            buffer_id,
                            view_state.viewport.top_byte
                        );
                        self.plugin_manager.run_hook(
                            "viewport_changed",
                            crate::services::plugins::hooks::HookArgs::ViewportChanged {
                                split_id: *split_id,
                                buffer_id,
                                top_byte: view_state.viewport.top_byte,
                                width: view_state.viewport.width,
                                height: view_state.viewport.height,
                            },
                        );
                    }
                }
            }
        }

        // Update previous_viewports for next frame's comparison
        self.previous_viewports.clear();
        for (split_id, view_state) in &self.split_view_states {
            self.previous_viewports.insert(
                *split_id,
                (
                    view_state.viewport.top_byte,
                    view_state.viewport.width,
                    view_state.viewport.height,
                ),
            );
        }

        // Render terminal content on top of split content for terminal buffers
        self.render_terminal_splits(frame, &split_areas);

        self.cached_layout.split_areas = split_areas;
        self.cached_layout.horizontal_scrollbar_areas = horizontal_scrollbar_areas;
        self.cached_layout.tab_layouts = tab_layouts;
        self.cached_layout.close_split_areas = close_split_areas;
        self.cached_layout.maximize_split_areas = maximize_split_areas;
        self.cached_layout.view_line_mappings = view_line_mappings;
        self.cached_layout.separator_areas = self
            .split_manager
            .get_separators_with_ids(editor_content_area);
        self.cached_layout.editor_content_area = Some(editor_content_area);

        // Render hover highlights for separators and scrollbars
        self.render_hover_highlights(frame);

        // Render file browser popup for OpenFile prompt, or suggestions for other prompts
        self.cached_layout.suggestions_area = None;
        self.file_browser_layout = None;
        if let Some(prompt) = &self.prompt {
            // For OpenFile/SwitchProject/SaveFileAs prompt, render the file browser popup
            if matches!(
                prompt.prompt_type,
                PromptType::OpenFile | PromptType::SwitchProject | PromptType::SaveFileAs
            ) {
                if let Some(file_open_state) = &self.file_open_state {
                    // Calculate popup area: position above prompt line, covering status bar
                    let max_height = main_chunks[prompt_line_idx].y.saturating_sub(1).min(20);
                    let popup_area = ratatui::layout::Rect {
                        x: 0,
                        y: main_chunks[prompt_line_idx].y.saturating_sub(max_height),
                        width: size.width,
                        height: max_height,
                    };

                    self.file_browser_layout = crate::view::ui::FileBrowserRenderer::render(
                        frame,
                        popup_area,
                        file_open_state,
                        &self.theme,
                        &self.mouse_state.hover_target,
                        Some(&self.keybindings),
                    );
                }
            } else if !prompt.suggestions.is_empty() {
                // For other prompts, render suggestions as before
                // Calculate overlay area: position above prompt line (which is below status bar)
                let suggestion_count = prompt.suggestions.len().min(10);
                let is_quick_open =
                    prompt.prompt_type == crate::view::prompt::PromptType::QuickOpen;
                let hints_height: u16 = if is_quick_open { 1 } else { 0 };
                let height = suggestion_count as u16 + 2 + hints_height; // +2 for borders, +1 for hints if QuickOpen

                // Position suggestions above the prompt line (and hints line if present)
                // The prompt line is at main_chunks[prompt_line_idx], so suggestions go above it
                let suggestions_area = ratatui::layout::Rect {
                    x: 0,
                    y: main_chunks[prompt_line_idx].y.saturating_sub(height),
                    width: size.width,
                    height: height - hints_height,
                };

                // Clear the area behind the suggestions to obscure underlying text
                frame.render_widget(ratatui::widgets::Clear, suggestions_area);

                self.cached_layout.suggestions_area = SuggestionsRenderer::render_with_hover(
                    frame,
                    suggestions_area,
                    prompt,
                    &self.theme,
                    self.mouse_state.hover_target.as_ref(),
                );

                // Render hints line for QuickOpen between suggestions and prompt
                if is_quick_open {
                    let hints_area = ratatui::layout::Rect {
                        x: 0,
                        y: main_chunks[prompt_line_idx].y.saturating_sub(hints_height),
                        width: size.width,
                        height: hints_height,
                    };
                    frame.render_widget(ratatui::widgets::Clear, hints_area);
                    Self::render_quick_open_hints(frame, hints_area, &self.theme);
                }
            }
        }

        // Clone all immutable values before the mutable borrow
        let display_name = self
            .buffer_metadata
            .get(&self.active_buffer())
            .map(|m| m.display_name.clone())
            .unwrap_or_else(|| "[No Name]".to_string());
        let status_message = self.status_message.clone();
        let plugin_status_message = self.plugin_status_message.clone();
        let prompt = self.prompt.clone();
        let lsp_status = self.lsp_status.clone();
        let theme = self.theme.clone();
        let keybindings_cloned = self.keybindings.clone(); // Clone the keybindings
        let chord_state_cloned = self.chord_state.clone(); // Clone the chord state

        // Get update availability info
        let update_available = self.latest_version().map(|v| v.to_string());

        // Render status bar (hidden when suggestions or file browser popup is shown)
        if !has_suggestions && !has_file_browser {
            // Get warning level for colored indicator (respects config setting)
            let (warning_level, general_warning_count) =
                if self.config.warnings.show_status_indicator {
                    (
                        self.get_effective_warning_level(),
                        self.get_general_warning_count(),
                    )
                } else {
                    (WarningLevel::None, 0)
                };

            // Compute status bar hover state for styling
            use crate::view::ui::status_bar::StatusBarHover;
            let status_bar_hover = match &self.mouse_state.hover_target {
                Some(HoverTarget::StatusBarLspIndicator) => StatusBarHover::LspIndicator,
                Some(HoverTarget::StatusBarWarningBadge) => StatusBarHover::WarningBadge,
                Some(HoverTarget::StatusBarLineEndingIndicator) => {
                    StatusBarHover::LineEndingIndicator
                }
                Some(HoverTarget::StatusBarEncodingIndicator) => StatusBarHover::EncodingIndicator,
                Some(HoverTarget::StatusBarLanguageIndicator) => StatusBarHover::LanguageIndicator,
                _ => StatusBarHover::None,
            };

            // Get remote connection info if editing remote files
            let remote_connection = self.remote_connection_info().map(|s| s.to_string());

            // Get session name for display (only in session mode)
            let session_name = self.session_name().map(|s| s.to_string());

            let active_split = self.split_manager.active_split();
            let active_buf = self.active_buffer();
            let default_cursors = crate::model::cursor::Cursors::new();
            let status_cursors = self
                .split_view_states
                .get(&active_split)
                .map(|vs| &vs.cursors)
                .unwrap_or(&default_cursors);
            let status_bar_layout = StatusBarRenderer::render_status_bar(
                frame,
                main_chunks[status_bar_idx],
                self.buffers.get_mut(&active_buf).unwrap(),
                status_cursors,
                &status_message,
                &plugin_status_message,
                &lsp_status,
                &theme,
                &display_name,
                &keybindings_cloned,          // Pass the cloned keybindings
                &chord_state_cloned,          // Pass the cloned chord state
                update_available.as_deref(),  // Pass update availability
                warning_level,                // Pass warning level for colored indicator
                general_warning_count,        // Pass general warning count for badge
                status_bar_hover,             // Pass hover state for indicator styling
                remote_connection.as_deref(), // Pass remote connection info
                session_name.as_deref(),      // Pass session name for status bar display
            );

            // Store status bar layout for click detection
            let status_bar_area = main_chunks[status_bar_idx];
            self.cached_layout.status_bar_area =
                Some((status_bar_area.y, status_bar_area.x, status_bar_area.width));
            self.cached_layout.status_bar_lsp_area = status_bar_layout.lsp_indicator;
            self.cached_layout.status_bar_warning_area = status_bar_layout.warning_badge;
            self.cached_layout.status_bar_line_ending_area =
                status_bar_layout.line_ending_indicator;
            self.cached_layout.status_bar_encoding_area = status_bar_layout.encoding_indicator;
            self.cached_layout.status_bar_language_area = status_bar_layout.language_indicator;
            self.cached_layout.status_bar_message_area = status_bar_layout.message_area;
        }

        // Render search options bar when in search prompt
        if show_search_options {
            // Show "Confirm" option only in replace modes
            let confirm_each = self.prompt.as_ref().and_then(|p| {
                if matches!(
                    p.prompt_type,
                    PromptType::ReplaceSearch
                        | PromptType::Replace { .. }
                        | PromptType::QueryReplaceSearch
                        | PromptType::QueryReplace { .. }
                ) {
                    Some(self.search_confirm_each)
                } else {
                    None
                }
            });

            // Determine hover state for search options
            use crate::view::ui::status_bar::SearchOptionsHover;
            let search_options_hover = match &self.mouse_state.hover_target {
                Some(HoverTarget::SearchOptionCaseSensitive) => SearchOptionsHover::CaseSensitive,
                Some(HoverTarget::SearchOptionWholeWord) => SearchOptionsHover::WholeWord,
                Some(HoverTarget::SearchOptionRegex) => SearchOptionsHover::Regex,
                Some(HoverTarget::SearchOptionConfirmEach) => SearchOptionsHover::ConfirmEach,
                _ => SearchOptionsHover::None,
            };

            let search_options_layout = StatusBarRenderer::render_search_options(
                frame,
                main_chunks[search_options_idx],
                self.search_case_sensitive,
                self.search_whole_word,
                self.search_use_regex,
                confirm_each,
                &theme,
                &keybindings_cloned,
                search_options_hover,
            );
            self.cached_layout.search_options_layout = Some(search_options_layout);
        } else {
            self.cached_layout.search_options_layout = None;
        }

        // Render prompt line if active
        if let Some(prompt) = &prompt {
            // Use specialized renderer for file/folder open prompt to show colorized path
            if matches!(
                prompt.prompt_type,
                crate::view::prompt::PromptType::OpenFile
                    | crate::view::prompt::PromptType::SwitchProject
            ) {
                if let Some(file_open_state) = &self.file_open_state {
                    StatusBarRenderer::render_file_open_prompt(
                        frame,
                        main_chunks[prompt_line_idx],
                        prompt,
                        file_open_state,
                        &theme,
                    );
                } else {
                    StatusBarRenderer::render_prompt(
                        frame,
                        main_chunks[prompt_line_idx],
                        prompt,
                        &theme,
                    );
                }
            } else {
                StatusBarRenderer::render_prompt(
                    frame,
                    main_chunks[prompt_line_idx],
                    prompt,
                    &theme,
                );
            }
        }

        // Render popups from the active buffer state
        // Clone theme to avoid borrow checker issues with active_state_mut()
        let theme_clone = self.theme.clone();
        let hover_target = self.mouse_state.hover_target.clone();

        // Clear popup areas and recalculate
        self.cached_layout.popup_areas.clear();

        // Collect popup information without holding a mutable borrow
        let popup_info: Vec<_> = {
            // Get viewport from active split's SplitViewState
            let active_split = self.split_manager.active_split();
            let viewport = self
                .split_view_states
                .get(&active_split)
                .map(|vs| vs.viewport.clone());

            // Get file explorer width offset for popup positioning (Bug #898)
            let file_explorer_offset = self
                .cached_layout
                .file_explorer_area
                .map(|area| area.width)
                .unwrap_or(0);

            let primary_cursor = self
                .split_view_states
                .get(&active_split)
                .map(|vs| *vs.cursors.primary());
            let state = self.active_state_mut();
            if state.popups.is_visible() {
                // Get the primary cursor position for popup positioning
                let primary_cursor =
                    primary_cursor.unwrap_or_else(|| crate::model::cursor::Cursor::new(0));
                let cursor_screen_pos = viewport
                    .as_ref()
                    .map(|vp| vp.cursor_screen_position(&mut state.buffer, &primary_cursor))
                    .unwrap_or((0, 0));

                // Adjust cursor position to account for tab bar (1 line offset)
                // and file explorer width (Bug #898)
                let cursor_screen_pos = (
                    cursor_screen_pos.0 + file_explorer_offset,
                    cursor_screen_pos.1 + 1,
                );

                // Collect popup data
                state
                    .popups
                    .all()
                    .iter()
                    .enumerate()
                    .map(|(popup_idx, popup)| {
                        let popup_area = popup.calculate_area(size, Some(cursor_screen_pos));

                        // Track popup area for mouse hit testing
                        // Account for description height when calculating the list item area
                        let desc_height = popup.description_height();
                        let inner_area = if popup.bordered {
                            ratatui::layout::Rect {
                                x: popup_area.x + 1,
                                y: popup_area.y + 1 + desc_height,
                                width: popup_area.width.saturating_sub(2),
                                height: popup_area.height.saturating_sub(2 + desc_height),
                            }
                        } else {
                            ratatui::layout::Rect {
                                x: popup_area.x,
                                y: popup_area.y + desc_height,
                                width: popup_area.width,
                                height: popup_area.height.saturating_sub(desc_height),
                            }
                        };

                        let num_items = match &popup.content {
                            crate::view::popup::PopupContent::List { items, .. } => items.len(),
                            _ => 0,
                        };

                        // Calculate total content lines and scrollbar rect
                        let total_lines = popup.item_count();
                        let visible_lines = inner_area.height as usize;
                        let scrollbar_rect = if total_lines > visible_lines && inner_area.width > 2
                        {
                            Some(ratatui::layout::Rect {
                                x: inner_area.x + inner_area.width - 1,
                                y: inner_area.y,
                                width: 1,
                                height: inner_area.height,
                            })
                        } else {
                            None
                        };

                        (
                            popup_idx,
                            popup_area,
                            inner_area,
                            popup.scroll_offset,
                            num_items,
                            scrollbar_rect,
                            total_lines,
                        )
                    })
                    .collect()
            } else {
                Vec::new()
            }
        };

        // Store popup areas for mouse hit testing
        self.cached_layout.popup_areas = popup_info.clone();

        // Now render popups
        let state = self.active_state_mut();
        if state.popups.is_visible() {
            for (popup_idx, popup) in state.popups.all().iter().enumerate() {
                if let Some((_, popup_area, _, _, _, _, _)) = popup_info.get(popup_idx) {
                    popup.render_with_hover(
                        frame,
                        *popup_area,
                        &theme_clone,
                        hover_target.as_ref(),
                    );
                }
            }
        }

        // Render menu bar last so dropdown appears on top of all other content
        // Update menu context with current editor state
        self.update_menu_context();

        // Render settings modal (before menu bar so menus can overlay)
        // Check visibility first to avoid borrow conflict with dimming
        let settings_visible = self
            .settings_state
            .as_ref()
            .map(|s| s.visible)
            .unwrap_or(false);
        if settings_visible {
            // Dim the editor content behind the settings modal
            crate::view::dimming::apply_dimming(frame, size);
        }
        if let Some(ref mut settings_state) = self.settings_state {
            if settings_state.visible {
                settings_state.update_focus_states();
                let settings_layout = crate::view::settings::render_settings(
                    frame,
                    size,
                    settings_state,
                    &self.theme,
                );
                self.cached_layout.settings_layout = Some(settings_layout);
            }
        }

        // Render calibration wizard if active
        if let Some(ref wizard) = self.calibration_wizard {
            // Dim the editor content behind the wizard modal
            crate::view::dimming::apply_dimming(frame, size);
            crate::view::calibration_wizard::render_calibration_wizard(
                frame,
                size,
                wizard,
                &self.theme,
            );
        }

        // Render keybinding editor if active
        if let Some(ref mut kb_editor) = self.keybinding_editor {
            crate::view::dimming::apply_dimming(frame, size);
            crate::view::keybinding_editor::render_keybinding_editor(
                frame,
                size,
                kb_editor,
                &self.theme,
            );
        }

        // Render event debug dialog if active
        if let Some(ref debug) = self.event_debug {
            // Dim the editor content behind the dialog modal
            crate::view::dimming::apply_dimming(frame, size);
            crate::view::event_debug::render_event_debug(frame, size, debug, &self.theme);
        }

        if self.menu_bar_visible {
            self.cached_layout.menu_layout = Some(crate::view::ui::MenuRenderer::render(
                frame,
                menu_bar_area,
                &self.menus,
                &self.menu_state,
                &self.keybindings,
                &self.theme,
                self.mouse_state.hover_target.as_ref(),
            ));
        } else {
            self.cached_layout.menu_layout = None;
        }

        // Render tab context menu if open
        if let Some(ref menu) = self.tab_context_menu {
            self.render_tab_context_menu(frame, menu);
        }

        // Render tab drag drop zone overlay if dragging a tab
        if let Some(ref drag_state) = self.mouse_state.dragging_tab {
            if drag_state.is_dragging() {
                self.render_tab_drop_zone(frame, drag_state);
            }
        }

        // Render software mouse cursor when GPM is active
        // GPM can't draw its cursor on the alternate screen buffer used by TUI apps,
        // so we draw our own cursor at the tracked mouse position.
        // This must happen LAST in the render flow so we can read the already-rendered
        // cell content and invert it.
        if self.gpm_active {
            if let Some((col, row)) = self.mouse_cursor_position {
                use ratatui::style::Modifier;

                // Only render if within screen bounds
                if col < size.width && row < size.height {
                    // Get the cell at this position and add REVERSED modifier to invert colors
                    let buf = frame.buffer_mut();
                    if let Some(cell) = buf.cell_mut((col, row)) {
                        cell.set_style(cell.style().add_modifier(Modifier::REVERSED));
                    }
                }
            }
        }

        // When keyboard capture mode is active, dim all UI elements outside the terminal
        // to visually indicate that focus is exclusively on the terminal
        if self.keyboard_capture && self.terminal_mode {
            // Find the active split's content area
            let active_split = self.split_manager.active_split();
            let active_split_area = self
                .cached_layout
                .split_areas
                .iter()
                .find(|(split_id, _, _, _, _, _)| *split_id == active_split)
                .map(|(_, _, content_rect, _, _, _)| *content_rect);

            if let Some(terminal_area) = active_split_area {
                self.apply_keyboard_capture_dimming(frame, terminal_area);
            }
        }

        // Convert all colors for terminal capability (256/16 color fallback)
        crate::view::color_support::convert_buffer_colors(
            frame.buffer_mut(),
            self.color_capability,
        );
    }

    /// Render the Quick Open hints line showing available mode prefixes
    fn render_quick_open_hints(
        frame: &mut Frame,
        area: ratatui::layout::Rect,
        theme: &crate::view::theme::Theme,
    ) {
        use ratatui::style::{Modifier, Style};
        use ratatui::text::{Line, Span};
        use ratatui::widgets::Paragraph;
        use rust_i18n::t;

        let hints_style = Style::default()
            .fg(theme.line_number_fg)
            .bg(theme.suggestion_selected_bg)
            .add_modifier(Modifier::DIM);
        let hints_text = t!("quick_open.mode_hints");
        // Left-align with small margin
        let left_margin = 2;
        let hints_width = crate::primitives::display_width::str_width(&hints_text);
        let mut spans = Vec::new();
        spans.push(Span::styled(" ".repeat(left_margin), hints_style));
        spans.push(Span::styled(hints_text.to_string(), hints_style));
        let remaining = (area.width as usize).saturating_sub(left_margin + hints_width);
        spans.push(Span::styled(" ".repeat(remaining), hints_style));

        let paragraph = Paragraph::new(Line::from(spans));
        frame.render_widget(paragraph, area);
    }

    /// Apply dimming effect to UI elements outside the focused terminal area
    /// This visually indicates that keyboard capture mode is active
    fn apply_keyboard_capture_dimming(
        &self,
        frame: &mut Frame,
        terminal_area: ratatui::layout::Rect,
    ) {
        let size = frame.area();
        crate::view::dimming::apply_dimming_excluding(frame, size, Some(terminal_area));
    }

    /// Render hover highlights for interactive elements (separators, scrollbars)
    pub(super) fn render_hover_highlights(&self, frame: &mut Frame) {
        use ratatui::style::Style;
        use ratatui::text::Span;
        use ratatui::widgets::Paragraph;

        match &self.mouse_state.hover_target {
            Some(HoverTarget::SplitSeparator(split_id, direction)) => {
                // Highlight the separator with hover color
                for (sid, dir, x, y, length) in &self.cached_layout.separator_areas {
                    if sid == split_id && dir == direction {
                        let hover_style = Style::default().fg(self.theme.split_separator_hover_fg);
                        match dir {
                            SplitDirection::Horizontal => {
                                let line_text = "─".repeat(*length as usize);
                                let paragraph =
                                    Paragraph::new(Span::styled(line_text, hover_style));
                                frame.render_widget(
                                    paragraph,
                                    ratatui::layout::Rect::new(*x, *y, *length, 1),
                                );
                            }
                            SplitDirection::Vertical => {
                                for offset in 0..*length {
                                    let paragraph = Paragraph::new(Span::styled("│", hover_style));
                                    frame.render_widget(
                                        paragraph,
                                        ratatui::layout::Rect::new(*x, y + offset, 1, 1),
                                    );
                                }
                            }
                        }
                    }
                }
            }
            Some(HoverTarget::ScrollbarThumb(split_id)) => {
                // Highlight scrollbar thumb
                for (sid, _buffer_id, _content_rect, scrollbar_rect, thumb_start, thumb_end) in
                    &self.cached_layout.split_areas
                {
                    if sid == split_id {
                        let hover_style = Style::default().bg(self.theme.scrollbar_thumb_hover_fg);
                        for row_offset in *thumb_start..*thumb_end {
                            let paragraph = Paragraph::new(Span::styled(" ", hover_style));
                            frame.render_widget(
                                paragraph,
                                ratatui::layout::Rect::new(
                                    scrollbar_rect.x,
                                    scrollbar_rect.y + row_offset as u16,
                                    1,
                                    1,
                                ),
                            );
                        }
                    }
                }
            }
            Some(HoverTarget::ScrollbarTrack(split_id)) => {
                // Highlight scrollbar track but preserve the thumb
                for (sid, _buffer_id, _content_rect, scrollbar_rect, thumb_start, thumb_end) in
                    &self.cached_layout.split_areas
                {
                    if sid == split_id {
                        let track_hover_style =
                            Style::default().bg(self.theme.scrollbar_track_hover_fg);
                        let thumb_style = Style::default().bg(self.theme.scrollbar_thumb_fg);
                        for row_offset in 0..scrollbar_rect.height {
                            let is_thumb = (row_offset as usize) >= *thumb_start
                                && (row_offset as usize) < *thumb_end;
                            let style = if is_thumb {
                                thumb_style
                            } else {
                                track_hover_style
                            };
                            let paragraph = Paragraph::new(Span::styled(" ", style));
                            frame.render_widget(
                                paragraph,
                                ratatui::layout::Rect::new(
                                    scrollbar_rect.x,
                                    scrollbar_rect.y + row_offset,
                                    1,
                                    1,
                                ),
                            );
                        }
                    }
                }
            }
            Some(HoverTarget::FileExplorerBorder) => {
                // Highlight the file explorer border for resize
                if let Some(explorer_area) = self.cached_layout.file_explorer_area {
                    let hover_style = Style::default().fg(self.theme.split_separator_hover_fg);
                    let border_x = explorer_area.x + explorer_area.width;
                    for row_offset in 0..explorer_area.height {
                        let paragraph = Paragraph::new(Span::styled("│", hover_style));
                        frame.render_widget(
                            paragraph,
                            ratatui::layout::Rect::new(
                                border_x,
                                explorer_area.y + row_offset,
                                1,
                                1,
                            ),
                        );
                    }
                }
            }
            // Menu hover is handled by MenuRenderer
            _ => {}
        }
    }

    /// Render the tab context menu
    fn render_tab_context_menu(&self, frame: &mut Frame, menu: &TabContextMenu) {
        use ratatui::style::Style;
        use ratatui::text::{Line, Span};
        use ratatui::widgets::{Block, Borders, Clear, Paragraph};

        let items = super::types::TabContextMenuItem::all();
        let menu_width = 22u16; // "Close to the Right" + padding
        let menu_height = items.len() as u16 + 2; // items + borders

        // Adjust position to stay within screen bounds
        let screen_width = frame.area().width;
        let screen_height = frame.area().height;

        let menu_x = if menu.position.0 + menu_width > screen_width {
            screen_width.saturating_sub(menu_width)
        } else {
            menu.position.0
        };

        let menu_y = if menu.position.1 + menu_height > screen_height {
            screen_height.saturating_sub(menu_height)
        } else {
            menu.position.1
        };

        let area = ratatui::layout::Rect::new(menu_x, menu_y, menu_width, menu_height);

        // Clear the area first
        frame.render_widget(Clear, area);

        // Build the menu lines
        let mut lines = Vec::new();
        for (idx, item) in items.iter().enumerate() {
            let is_highlighted = idx == menu.highlighted;

            let style = if is_highlighted {
                Style::default()
                    .fg(self.theme.menu_highlight_fg)
                    .bg(self.theme.menu_highlight_bg)
            } else {
                Style::default()
                    .fg(self.theme.menu_dropdown_fg)
                    .bg(self.theme.menu_dropdown_bg)
            };

            // Pad the label to fill the menu width
            let label = item.label();
            let content_width = (menu_width as usize).saturating_sub(2); // -2 for borders
            let padded_label = format!(" {:<width$}", label, width = content_width - 1);

            lines.push(Line::from(vec![Span::styled(padded_label, style)]));
        }

        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(self.theme.menu_border_fg))
            .style(Style::default().bg(self.theme.menu_dropdown_bg));

        let paragraph = Paragraph::new(lines).block(block);
        frame.render_widget(paragraph, area);
    }

    /// Render the tab drag drop zone overlay
    fn render_tab_drop_zone(&self, frame: &mut Frame, drag_state: &super::types::TabDragState) {
        use ratatui::style::Modifier;

        let Some(ref drop_zone) = drag_state.drop_zone else {
            return;
        };

        let split_id = drop_zone.split_id();

        // Find the content area for the target split
        let split_area = self
            .cached_layout
            .split_areas
            .iter()
            .find(|(sid, _, _, _, _, _)| *sid == split_id)
            .map(|(_, _, content_rect, _, _, _)| *content_rect);

        let Some(content_rect) = split_area else {
            return;
        };

        // Determine the highlight area based on drop zone type
        use super::types::TabDropZone;

        let highlight_area = match drop_zone {
            TabDropZone::TabBar(_, _) | TabDropZone::SplitCenter(_) => {
                // For tab bar and center drops, highlight the entire split area
                // This indicates the tab will be added to this split's tab bar
                content_rect
            }
            TabDropZone::SplitLeft(_) => {
                // Left 50% of the split (matches the actual split size created)
                let width = (content_rect.width / 2).max(3);
                ratatui::layout::Rect::new(
                    content_rect.x,
                    content_rect.y,
                    width,
                    content_rect.height,
                )
            }
            TabDropZone::SplitRight(_) => {
                // Right 50% of the split (matches the actual split size created)
                let width = (content_rect.width / 2).max(3);
                let x = content_rect.x + content_rect.width - width;
                ratatui::layout::Rect::new(x, content_rect.y, width, content_rect.height)
            }
            TabDropZone::SplitTop(_) => {
                // Top 50% of the split (matches the actual split size created)
                let height = (content_rect.height / 2).max(2);
                ratatui::layout::Rect::new(
                    content_rect.x,
                    content_rect.y,
                    content_rect.width,
                    height,
                )
            }
            TabDropZone::SplitBottom(_) => {
                // Bottom 50% of the split (matches the actual split size created)
                let height = (content_rect.height / 2).max(2);
                let y = content_rect.y + content_rect.height - height;
                ratatui::layout::Rect::new(content_rect.x, y, content_rect.width, height)
            }
        };

        // Draw the overlay with the drop zone color
        // We apply a semi-transparent effect by modifying existing cells
        let buf = frame.buffer_mut();
        let drop_zone_bg = self.theme.tab_drop_zone_bg;
        let drop_zone_border = self.theme.tab_drop_zone_border;

        // Fill the highlight area with a semi-transparent overlay
        for y in highlight_area.y..highlight_area.y + highlight_area.height {
            for x in highlight_area.x..highlight_area.x + highlight_area.width {
                if let Some(cell) = buf.cell_mut((x, y)) {
                    // Blend the drop zone color with the existing background
                    // For a simple effect, we just set the background
                    cell.set_bg(drop_zone_bg);

                    // Draw border on edges
                    let is_border = x == highlight_area.x
                        || x == highlight_area.x + highlight_area.width - 1
                        || y == highlight_area.y
                        || y == highlight_area.y + highlight_area.height - 1;

                    if is_border {
                        cell.set_fg(drop_zone_border);
                        cell.set_style(cell.style().add_modifier(Modifier::BOLD));
                    }
                }
            }
        }

        // Draw a border indicator based on the zone type
        match drop_zone {
            TabDropZone::SplitLeft(_) => {
                // Draw vertical indicator on left edge
                for y in highlight_area.y..highlight_area.y + highlight_area.height {
                    if let Some(cell) = buf.cell_mut((highlight_area.x, y)) {
                        cell.set_symbol("▌");
                        cell.set_fg(drop_zone_border);
                    }
                }
            }
            TabDropZone::SplitRight(_) => {
                // Draw vertical indicator on right edge
                let x = highlight_area.x + highlight_area.width - 1;
                for y in highlight_area.y..highlight_area.y + highlight_area.height {
                    if let Some(cell) = buf.cell_mut((x, y)) {
                        cell.set_symbol("▐");
                        cell.set_fg(drop_zone_border);
                    }
                }
            }
            TabDropZone::SplitTop(_) => {
                // Draw horizontal indicator on top edge
                for x in highlight_area.x..highlight_area.x + highlight_area.width {
                    if let Some(cell) = buf.cell_mut((x, highlight_area.y)) {
                        cell.set_symbol("▀");
                        cell.set_fg(drop_zone_border);
                    }
                }
            }
            TabDropZone::SplitBottom(_) => {
                // Draw horizontal indicator on bottom edge
                let y = highlight_area.y + highlight_area.height - 1;
                for x in highlight_area.x..highlight_area.x + highlight_area.width {
                    if let Some(cell) = buf.cell_mut((x, y)) {
                        cell.set_symbol("▄");
                        cell.set_fg(drop_zone_border);
                    }
                }
            }
            TabDropZone::SplitCenter(_) | TabDropZone::TabBar(_, _) => {
                // For center and tab bar, the filled background is sufficient
            }
        }
    }

    // === Overlay Management (Event-Driven) ===

    /// Add an overlay for decorations (underlines, highlights, etc.)
    pub fn add_overlay(
        &mut self,
        namespace: Option<crate::view::overlay::OverlayNamespace>,
        range: Range<usize>,
        face: crate::model::event::OverlayFace,
        priority: i32,
        message: Option<String>,
    ) -> crate::view::overlay::OverlayHandle {
        let event = Event::AddOverlay {
            namespace,
            range,
            face,
            priority,
            message,
            extend_to_line_end: false,
            url: None,
        };
        self.apply_event_to_active_buffer(&event);
        // Return the handle of the last added overlay
        let state = self.active_state();
        state
            .overlays
            .all()
            .last()
            .map(|o| o.handle.clone())
            .unwrap_or_default()
    }

    /// Remove an overlay by handle
    pub fn remove_overlay(&mut self, handle: crate::view::overlay::OverlayHandle) {
        let event = Event::RemoveOverlay { handle };
        self.apply_event_to_active_buffer(&event);
    }

    /// Remove all overlays in a range
    pub fn remove_overlays_in_range(&mut self, range: Range<usize>) {
        let event = Event::RemoveOverlaysInRange { range };
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Clear all overlays
    pub fn clear_overlays(&mut self) {
        let event = Event::ClearOverlays;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    // === Popup Management (Event-Driven) ===

    /// Show a popup window
    pub fn show_popup(&mut self, popup: crate::model::event::PopupData) {
        let event = Event::ShowPopup { popup };
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Hide the topmost popup
    pub fn hide_popup(&mut self) {
        let event = Event::HidePopup;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);

        // Clear hover symbol highlight if present
        if let Some(handle) = self.hover_symbol_overlay.take() {
            let remove_overlay_event = crate::model::event::Event::RemoveOverlay { handle };
            self.apply_event_to_active_buffer(&remove_overlay_event);
        }
        self.hover_symbol_range = None;
    }

    /// Dismiss transient popups if present
    /// These popups should be dismissed on scroll or other user actions
    pub(super) fn dismiss_transient_popups(&mut self) {
        let is_transient_popup = self
            .active_state()
            .popups
            .top()
            .is_some_and(|p| p.transient);

        if is_transient_popup {
            self.hide_popup();
            tracing::trace!("Dismissed transient popup");
        }
    }

    /// Scroll any popup content by delta lines
    /// Positive delta scrolls down, negative scrolls up
    pub(super) fn scroll_popup(&mut self, delta: i32) {
        if let Some(popup) = self.active_state_mut().popups.top_mut() {
            popup.scroll_by(delta);
            tracing::debug!(
                "Scrolled popup by {}, new offset: {}",
                delta,
                popup.scroll_offset
            );
        }
    }

    /// Called when the editor buffer loses focus (e.g., switching buffers,
    /// opening prompts/menus, focusing file explorer, etc.)
    ///
    /// This is the central handler for focus loss that:
    /// - Dismisses transient popups (Hover, Signature Help)
    /// - Clears LSP hover state and pending requests
    /// - Removes hover symbol highlighting
    pub(super) fn on_editor_focus_lost(&mut self) {
        // Dismiss transient popups via EditorState
        self.active_state_mut().on_focus_lost();

        // Clear hover state
        self.mouse_state.lsp_hover_state = None;
        self.mouse_state.lsp_hover_request_sent = false;
        self.pending_hover_request = None;

        // Clear hover symbol highlight if present
        if let Some(handle) = self.hover_symbol_overlay.take() {
            let remove_overlay_event = crate::model::event::Event::RemoveOverlay { handle };
            self.apply_event_to_active_buffer(&remove_overlay_event);
        }
        self.hover_symbol_range = None;
    }

    /// Clear all popups
    pub fn clear_popups(&mut self) {
        let event = Event::ClearPopups;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    // === LSP Confirmation Popup ===

    /// Show the LSP confirmation popup for a language server
    ///
    /// This displays a centered popup asking the user to confirm whether
    /// they want to start the LSP server for the given language.
    pub fn show_lsp_confirmation_popup(&mut self, language: &str) {
        use crate::model::event::{
            PopupContentData, PopupData, PopupKindHint, PopupListItemData, PopupPositionData,
        };

        // Store the pending confirmation
        self.pending_lsp_confirmation = Some(language.to_string());

        // Get the server command for display
        let server_info = if let Some(lsp) = &self.lsp {
            if let Some(config) = lsp.get_config(language) {
                if !config.command.is_empty() {
                    format!("{} ({})", language, config.command)
                } else {
                    language.to_string()
                }
            } else {
                language.to_string()
            }
        } else {
            language.to_string()
        };

        let popup = PopupData {
            kind: PopupKindHint::List,
            title: Some(format!("Start LSP Server: {}?", server_info)),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![
                    PopupListItemData {
                        text: "Allow this time".to_string(),
                        detail: Some("Start the LSP server for this session".to_string()),
                        icon: None,
                        data: Some("allow_once".to_string()),
                    },
                    PopupListItemData {
                        text: "Always allow".to_string(),
                        detail: Some("Always start this LSP server automatically".to_string()),
                        icon: None,
                        data: Some("allow_always".to_string()),
                    },
                    PopupListItemData {
                        text: "Don't start".to_string(),
                        detail: Some("Cancel LSP server startup".to_string()),
                        icon: None,
                        data: Some("deny".to_string()),
                    },
                ],
                selected: 0,
            },
            position: PopupPositionData::Centered,
            width: 50,
            max_height: 8,
            bordered: true,
        };

        self.show_popup(popup);
    }

    /// Handle the LSP confirmation popup response
    ///
    /// This is called when the user confirms their selection in the LSP
    /// confirmation popup. It processes the response and starts the LSP
    /// server if approved.
    ///
    /// Returns true if a response was handled, false if there was no pending confirmation.
    pub fn handle_lsp_confirmation_response(&mut self, action: &str) -> bool {
        let Some(language) = self.pending_lsp_confirmation.take() else {
            return false;
        };

        match action {
            "allow_once" => {
                // Spawn the LSP server just this once (don't add to always-allowed)
                if let Some(lsp) = &mut self.lsp {
                    // Temporarily allow this language for spawning
                    lsp.allow_language(&language);
                    // Use force_spawn since user explicitly confirmed
                    if lsp.force_spawn(&language).is_some() {
                        tracing::info!("LSP server for {} started (allowed once)", language);
                        self.set_status_message(
                            t!("lsp.server_started", language = language).to_string(),
                        );
                    } else {
                        self.set_status_message(
                            t!("lsp.failed_to_start", language = language).to_string(),
                        );
                    }
                }
                // Notify LSP about the current file
                self.notify_lsp_current_file_opened(&language);
            }
            "allow_always" => {
                // Spawn the LSP server and remember the preference
                if let Some(lsp) = &mut self.lsp {
                    lsp.allow_language(&language);
                    // Use force_spawn since user explicitly confirmed
                    if lsp.force_spawn(&language).is_some() {
                        tracing::info!("LSP server for {} started (always allowed)", language);
                        self.set_status_message(
                            t!("lsp.server_started_auto", language = language).to_string(),
                        );
                    } else {
                        self.set_status_message(
                            t!("lsp.failed_to_start", language = language).to_string(),
                        );
                    }
                }
                // Notify LSP about the current file
                self.notify_lsp_current_file_opened(&language);
            }
            _ => {
                // User declined - don't start the server
                tracing::info!("LSP server for {} startup declined by user", language);
                self.set_status_message(
                    t!("lsp.startup_cancelled", language = language).to_string(),
                );
            }
        }

        true
    }

    /// Notify LSP about the currently open file
    ///
    /// This is called after an LSP server is started to notify it about
    /// the current file so it can provide features like diagnostics.
    fn notify_lsp_current_file_opened(&mut self, language: &str) {
        // Get buffer metadata for the active buffer
        let metadata = match self.buffer_metadata.get(&self.active_buffer()) {
            Some(m) => m,
            None => {
                tracing::debug!(
                    "notify_lsp_current_file_opened: no metadata for buffer {:?}",
                    self.active_buffer()
                );
                return;
            }
        };

        if !metadata.lsp_enabled {
            tracing::debug!("notify_lsp_current_file_opened: LSP disabled for this buffer");
            return;
        }

        // Get the URI (computed once in with_file)
        let uri = match metadata.file_uri() {
            Some(u) => u.clone(),
            None => {
                tracing::debug!(
                    "notify_lsp_current_file_opened: no URI for buffer (not a file or URI creation failed)"
                );
                return;
            }
        };

        // Get the buffer text and line count before borrowing lsp
        let active_buffer = self.active_buffer();

        // Use buffer's stored language to verify it matches the LSP server
        let file_language = match self.buffers.get(&active_buffer).map(|s| s.language.clone()) {
            Some(l) => l,
            None => {
                tracing::debug!("notify_lsp_current_file_opened: no buffer state");
                return;
            }
        };

        // Only notify if the file's language matches the LSP server we just started
        if file_language != language {
            tracing::debug!(
                "notify_lsp_current_file_opened: file language {} doesn't match server {}",
                file_language,
                language
            );
            return;
        }
        let (text, line_count) = if let Some(state) = self.buffers.get(&active_buffer) {
            let text = match state.buffer.to_string() {
                Some(t) => t,
                None => {
                    tracing::debug!("notify_lsp_current_file_opened: buffer not fully loaded");
                    return;
                }
            };
            let line_count = state.buffer.line_count().unwrap_or(1000);
            (text, line_count)
        } else {
            tracing::debug!("notify_lsp_current_file_opened: no buffer state");
            return;
        };

        // Send didOpen to LSP (use force_spawn since this is called after user confirmation)
        if let Some(lsp) = &mut self.lsp {
            if let Some(client) = lsp.force_spawn(language) {
                tracing::info!("Sending didOpen to newly started LSP for: {}", uri.as_str());
                if let Err(e) = client.did_open(uri.clone(), text, file_language) {
                    tracing::warn!("Failed to send didOpen to LSP: {}", e);
                } else {
                    tracing::info!("Successfully sent didOpen to LSP after confirmation");

                    // Request pull diagnostics
                    let previous_result_id = self.diagnostic_result_ids.get(uri.as_str()).cloned();
                    let request_id = self.next_lsp_request_id;
                    self.next_lsp_request_id += 1;

                    if let Err(e) =
                        client.document_diagnostic(request_id, uri.clone(), previous_result_id)
                    {
                        tracing::debug!(
                            "Failed to request pull diagnostics (server may not support): {}",
                            e
                        );
                    }

                    // Request inlay hints if enabled
                    if self.config.editor.enable_inlay_hints {
                        let request_id = self.next_lsp_request_id;
                        self.next_lsp_request_id += 1;
                        self.pending_inlay_hints_request = Some(request_id);

                        let last_line = line_count.saturating_sub(1) as u32;
                        let last_char = 10000u32;

                        if let Err(e) =
                            client.inlay_hints(request_id, uri.clone(), 0, 0, last_line, last_char)
                        {
                            tracing::debug!(
                                "Failed to request inlay hints (server may not support): {}",
                                e
                            );
                            self.pending_inlay_hints_request = None;
                        }
                    }
                }
            }
        }
    }

    /// Check if there's a pending LSP confirmation
    pub fn has_pending_lsp_confirmation(&self) -> bool {
        self.pending_lsp_confirmation.is_some()
    }

    /// Navigate popup selection (next item)
    pub fn popup_select_next(&mut self) {
        let event = Event::PopupSelectNext;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Navigate popup selection (previous item)
    pub fn popup_select_prev(&mut self) {
        let event = Event::PopupSelectPrev;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Navigate popup (page down)
    pub fn popup_page_down(&mut self) {
        let event = Event::PopupPageDown;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    /// Navigate popup (page up)
    pub fn popup_page_up(&mut self) {
        let event = Event::PopupPageUp;
        self.active_event_log_mut().append(event.clone());
        self.apply_event_to_active_buffer(&event);
    }

    // === LSP Diagnostics Display ===
    // NOTE: Diagnostics are now applied automatically via process_async_messages()
    // when received from the LSP server asynchronously. No manual polling needed!

    /// Collect all LSP text document changes from an event (recursively for batches)
    pub(super) fn collect_lsp_changes(&self, event: &Event) -> Vec<TextDocumentContentChangeEvent> {
        match event {
            Event::Insert { position, text, .. } => {
                tracing::trace!(
                    "collect_lsp_changes: processing Insert at position {}",
                    position
                );
                // For insert: create a zero-width range at the insertion point
                let (line, character) = self
                    .active_state()
                    .buffer
                    .position_to_lsp_position(*position);
                let lsp_pos = Position::new(line as u32, character as u32);
                let lsp_range = LspRange::new(lsp_pos, lsp_pos);
                vec![TextDocumentContentChangeEvent {
                    range: Some(lsp_range),
                    range_length: None,
                    text: text.clone(),
                }]
            }
            Event::Delete { range, .. } => {
                tracing::trace!("collect_lsp_changes: processing Delete range {:?}", range);
                // For delete: create a range from start to end, send empty string
                let (start_line, start_char) = self
                    .active_state()
                    .buffer
                    .position_to_lsp_position(range.start);
                let (end_line, end_char) = self
                    .active_state()
                    .buffer
                    .position_to_lsp_position(range.end);
                let lsp_range = LspRange::new(
                    Position::new(start_line as u32, start_char as u32),
                    Position::new(end_line as u32, end_char as u32),
                );
                vec![TextDocumentContentChangeEvent {
                    range: Some(lsp_range),
                    range_length: None,
                    text: String::new(),
                }]
            }
            Event::Batch { events, .. } => {
                // Collect all changes from sub-events into a single vector
                // This allows sending all changes in one didChange notification
                tracing::trace!(
                    "collect_lsp_changes: processing Batch with {} events",
                    events.len()
                );
                let mut all_changes = Vec::new();
                for sub_event in events {
                    all_changes.extend(self.collect_lsp_changes(sub_event));
                }
                all_changes
            }
            _ => Vec::new(), // Ignore cursor movements and other events
        }
    }

    /// Calculate line information for an event (before buffer modification)
    /// This provides accurate line numbers for plugin hooks to track changes.
    ///
    /// ## Design Alternatives for Line Tracking
    ///
    /// **Approach 1: Re-diff on every edit (VSCode style)**
    /// - Store original file content, re-run diff algorithm after each edit
    /// - Simpler conceptually, but O(n) per edit for diff computation
    /// - Better for complex scenarios (multi-cursor, large batch edits)
    ///
    /// **Approach 2: Track line shifts (our approach)**
    /// - Calculate line info BEFORE applying edit (like LSP does)
    /// - Pass `lines_added`/`lines_removed` to plugins via hooks
    /// - Plugins shift their stored line numbers accordingly
    /// - O(1) per edit, but requires careful bookkeeping
    ///
    /// We use Approach 2 because:
    /// - Matches existing LSP infrastructure (`collect_lsp_changes`)
    /// - More efficient for typical editing patterns
    /// - Plugins can choose to re-diff if they need more accuracy
    ///
    pub(super) fn calculate_event_line_info(&self, event: &Event) -> super::types::EventLineInfo {
        match event {
            Event::Insert { position, text, .. } => {
                // Get line number at insert position (from original buffer)
                let start_line = self.active_state().buffer.get_line_number(*position);

                // Count newlines in inserted text to determine lines added
                let lines_added = text.matches('\n').count();
                let end_line = start_line + lines_added;

                super::types::EventLineInfo {
                    start_line,
                    end_line,
                    line_delta: lines_added as i32,
                }
            }
            Event::Delete {
                range,
                deleted_text,
                ..
            } => {
                // Get line numbers for the deleted range (from original buffer)
                let start_line = self.active_state().buffer.get_line_number(range.start);
                let end_line = self.active_state().buffer.get_line_number(range.end);

                // Count newlines in deleted text to determine lines removed
                let lines_removed = deleted_text.matches('\n').count();

                super::types::EventLineInfo {
                    start_line,
                    end_line,
                    line_delta: -(lines_removed as i32),
                }
            }
            Event::Batch { events, .. } => {
                // For batches, compute cumulative line info
                // This is a simplification - we report the range covering all changes
                let mut min_line = usize::MAX;
                let mut max_line = 0usize;
                let mut total_delta = 0i32;

                for sub_event in events {
                    let info = self.calculate_event_line_info(sub_event);
                    min_line = min_line.min(info.start_line);
                    max_line = max_line.max(info.end_line);
                    total_delta += info.line_delta;
                }

                if min_line == usize::MAX {
                    min_line = 0;
                }

                super::types::EventLineInfo {
                    start_line: min_line,
                    end_line: max_line,
                    line_delta: total_delta,
                }
            }
            _ => super::types::EventLineInfo::default(),
        }
    }

    /// Notify LSP of a file save
    pub(super) fn notify_lsp_save(&mut self) {
        // Check if LSP is enabled for this buffer
        let metadata = match self.buffer_metadata.get(&self.active_buffer()) {
            Some(m) => m,
            None => {
                tracing::debug!(
                    "notify_lsp_save: no metadata for buffer {:?}",
                    self.active_buffer()
                );
                return;
            }
        };

        if !metadata.lsp_enabled {
            tracing::debug!("notify_lsp_save: LSP disabled for this buffer");
            return;
        }

        // Get the URI
        let uri = match metadata.file_uri() {
            Some(u) => u.clone(),
            None => {
                tracing::debug!("notify_lsp_save: no URI for buffer");
                return;
            }
        };

        // Get the file path for language detection
        // Use buffer's stored language
        let language = match self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
        {
            Some(l) => l,
            None => {
                tracing::debug!("notify_lsp_save: no buffer state");
                return;
            }
        };

        // Get the full text to send with didSave
        let full_text = match self.active_state().buffer.to_string() {
            Some(t) => t,
            None => {
                tracing::debug!("notify_lsp_save: buffer not fully loaded");
                return;
            }
        };
        tracing::debug!(
            "notify_lsp_save: sending didSave to {} (text length: {} bytes)",
            uri.as_str(),
            full_text.len()
        );

        // Only send didSave if LSP is already running (respect auto_start setting)
        if let Some(lsp) = &mut self.lsp {
            use crate::services::lsp::manager::LspSpawnResult;
            if lsp.try_spawn(&language) != LspSpawnResult::Spawned {
                tracing::debug!(
                    "notify_lsp_save: LSP not running for {} (auto_start disabled)",
                    language
                );
                return;
            }
            if let Some(client) = lsp.get_handle_mut(&language) {
                // Send didSave with the full text content
                if let Err(e) = client.did_save(uri, Some(full_text)) {
                    tracing::warn!("Failed to send didSave to LSP: {}", e);
                } else {
                    tracing::info!("Successfully sent didSave to LSP");
                }
            } else {
                tracing::warn!("notify_lsp_save: failed to get LSP client for {}", language);
            }
        } else {
            tracing::debug!("notify_lsp_save: no LSP manager available");
        }
    }

    /// Convert an action into a list of events to apply to the active buffer
    /// Returns None for actions that don't generate events (like Quit)
    pub fn action_to_events(&mut self, action: Action) -> Option<Vec<Event>> {
        let tab_size = self.config.editor.tab_size;
        let auto_indent = self.config.editor.auto_indent;
        let estimated_line_length = self.config.editor.estimated_line_length;

        // Get viewport height from SplitViewState (the authoritative source)
        let active_split = self.split_manager.active_split();
        let viewport_height = self
            .split_view_states
            .get(&active_split)
            .map(|vs| vs.viewport.height)
            .unwrap_or(24);

        // Always try visual line movement first — it uses the cached layout to
        // move through soft-wrapped rows.  Returns None when the layout can't
        // resolve the movement, falling through to logical movement below.
        if let Some(events) =
            self.handle_visual_line_movement(&action, active_split, estimated_line_length)
        {
            return Some(events);
        }

        let buffer_id = self.active_buffer();
        let state = self.buffers.get_mut(&buffer_id).unwrap();
        let cursors = &mut self
            .split_view_states
            .get_mut(&active_split)
            .unwrap()
            .cursors;
        convert_action_to_events(
            state,
            cursors,
            action,
            tab_size,
            auto_indent,
            estimated_line_length,
            viewport_height,
        )
    }

    /// Handle visual line movement actions using the cached layout
    /// Returns Some(events) if the action was handled, None if it should fall through
    fn handle_visual_line_movement(
        &mut self,
        action: &Action,
        split_id: SplitId,
        _estimated_line_length: usize,
    ) -> Option<Vec<Event>> {
        // Classify the action
        enum VisualAction {
            UpDown { direction: i8, is_select: bool },
            LineEnd { is_select: bool },
            LineStart { is_select: bool },
        }

        // Note: We don't intercept BlockSelectUp/Down because block selection has
        // special semantics (setting block_anchor) that require the default handler
        let visual_action = match action {
            Action::MoveUp => VisualAction::UpDown {
                direction: -1,
                is_select: false,
            },
            Action::MoveDown => VisualAction::UpDown {
                direction: 1,
                is_select: false,
            },
            Action::SelectUp => VisualAction::UpDown {
                direction: -1,
                is_select: true,
            },
            Action::SelectDown => VisualAction::UpDown {
                direction: 1,
                is_select: true,
            },
            // When line wrapping is off, Home/End should move to the physical line
            // start/end, not the visual (horizontally-scrolled) row boundary.
            // Fall through to the standard handler which uses line_iterator.
            Action::MoveLineEnd if self.config.editor.line_wrap => {
                VisualAction::LineEnd { is_select: false }
            }
            Action::SelectLineEnd if self.config.editor.line_wrap => {
                VisualAction::LineEnd { is_select: true }
            }
            Action::MoveLineStart if self.config.editor.line_wrap => {
                VisualAction::LineStart { is_select: false }
            }
            Action::SelectLineStart if self.config.editor.line_wrap => {
                VisualAction::LineStart { is_select: true }
            }
            _ => return None, // Not a visual line action
        };

        // First, collect cursor data we need (to avoid borrow conflicts)
        let cursor_data: Vec<_> = {
            let active_split = self.split_manager.active_split();
            let active_buffer = self.split_manager.active_buffer_id().unwrap();
            let cursors = &self.split_view_states.get(&active_split).unwrap().cursors;
            let state = self.buffers.get(&active_buffer).unwrap();
            cursors
                .iter()
                .map(|(cursor_id, cursor)| {
                    // Check if cursor is at a physical line boundary:
                    // - at_line_ending: byte at cursor position is a newline or at buffer end
                    // - at_line_start: cursor is at position 0 or preceded by a newline
                    let at_line_ending = if cursor.position < state.buffer.len() {
                        let bytes = state
                            .buffer
                            .slice_bytes(cursor.position..cursor.position + 1);
                        bytes.first() == Some(&b'\n') || bytes.first() == Some(&b'\r')
                    } else {
                        true // end of buffer is a boundary
                    };
                    let at_line_start = if cursor.position == 0 {
                        true
                    } else {
                        let prev = state
                            .buffer
                            .slice_bytes(cursor.position - 1..cursor.position);
                        prev.first() == Some(&b'\n')
                    };
                    (
                        cursor_id,
                        cursor.position,
                        cursor.anchor,
                        cursor.sticky_column,
                        cursor.deselect_on_move,
                        at_line_ending,
                        at_line_start,
                    )
                })
                .collect()
        };

        let mut events = Vec::new();

        for (
            cursor_id,
            position,
            anchor,
            sticky_column,
            deselect_on_move,
            at_line_ending,
            at_line_start,
        ) in cursor_data
        {
            let (new_pos, new_sticky) = match &visual_action {
                VisualAction::UpDown { direction, .. } => {
                    // Calculate current visual column from cached layout
                    let current_visual_col =
                        match self.cached_layout.byte_to_visual_column(split_id, position) {
                            Some(col) => col,
                            None => return None,
                        };

                    let goal_visual_col = if sticky_column > 0 {
                        sticky_column
                    } else {
                        current_visual_col
                    };

                    match self.cached_layout.move_visual_line(
                        split_id,
                        position,
                        goal_visual_col,
                        *direction,
                    ) {
                        Some(result) => result,
                        None => continue, // At boundary, skip this cursor
                    }
                }
                VisualAction::LineEnd { .. } => {
                    // Allow advancing to next visual segment only if not at a physical line ending
                    let allow_advance = !at_line_ending;
                    match self
                        .cached_layout
                        .visual_line_end(split_id, position, allow_advance)
                    {
                        Some(end_pos) => (end_pos, 0),
                        None => return None,
                    }
                }
                VisualAction::LineStart { .. } => {
                    // Allow advancing to previous visual segment only if not at a physical line start
                    let allow_advance = !at_line_start;
                    match self
                        .cached_layout
                        .visual_line_start(split_id, position, allow_advance)
                    {
                        Some(start_pos) => (start_pos, 0),
                        None => return None,
                    }
                }
            };

            let is_select = match &visual_action {
                VisualAction::UpDown { is_select, .. } => *is_select,
                VisualAction::LineEnd { is_select } => *is_select,
                VisualAction::LineStart { is_select } => *is_select,
            };

            let new_anchor = if is_select {
                Some(anchor.unwrap_or(position))
            } else if deselect_on_move {
                None
            } else {
                anchor
            };

            events.push(Event::MoveCursor {
                cursor_id,
                old_position: position,
                new_position: new_pos,
                old_anchor: anchor,
                new_anchor,
                old_sticky_column: sticky_column,
                new_sticky_column: new_sticky,
            });
        }

        if events.is_empty() {
            None // Let the default handler deal with it
        } else {
            Some(events)
        }
    }

    // === Search and Replace Methods ===

    /// Clear all search highlights from the active buffer and reset search state
    pub(super) fn clear_search_highlights(&mut self) {
        self.clear_search_overlays();
        // Also clear search state
        self.search_state = None;
    }

    /// Clear only the visual search overlays, preserving search state for F3/Shift+F3
    /// This is used when the buffer is modified - highlights become stale but F3 should still work
    pub(super) fn clear_search_overlays(&mut self) {
        let ns = self.search_namespace.clone();
        let state = self.active_state_mut();
        state.overlays.clear_namespace(&ns, &mut state.marker_list);
    }

    /// Update search highlights in visible viewport only (for incremental search)
    /// This is called as the user types in the search prompt for real-time feedback
    pub(super) fn update_search_highlights(&mut self, query: &str) {
        // If query is empty, clear highlights and return
        if query.is_empty() {
            self.clear_search_highlights();
            return;
        }

        // Get theme colors and search settings before borrowing state
        let search_bg = self.theme.search_match_bg;
        let search_fg = self.theme.search_match_fg;
        let case_sensitive = self.search_case_sensitive;
        let whole_word = self.search_whole_word;
        let use_regex = self.search_use_regex;
        let ns = self.search_namespace.clone();

        // Build regex pattern if regex mode is enabled, or escape for literal search
        let regex_pattern = if use_regex {
            if whole_word {
                format!(r"\b{}\b", query)
            } else {
                query.to_string()
            }
        } else {
            let escaped = regex::escape(query);
            if whole_word {
                format!(r"\b{}\b", escaped)
            } else {
                escaped
            }
        };

        // Build regex with case sensitivity
        let regex = regex::RegexBuilder::new(&regex_pattern)
            .case_insensitive(!case_sensitive)
            .build();

        let regex = match regex {
            Ok(r) => r,
            Err(_) => {
                // Invalid regex, clear highlights and return
                self.clear_search_highlights();
                return;
            }
        };

        // Get viewport from active split's SplitViewState
        let active_split = self.split_manager.active_split();
        let (top_byte, visible_height) = self
            .split_view_states
            .get(&active_split)
            .map(|vs| (vs.viewport.top_byte, vs.viewport.height.saturating_sub(2)))
            .unwrap_or((0, 20));

        let state = self.active_state_mut();

        // Clear any existing search highlights
        state.overlays.clear_namespace(&ns, &mut state.marker_list);

        // Get the visible content by iterating through visible lines
        let visible_start = top_byte;
        let mut visible_end = top_byte;

        {
            let mut line_iter = state.buffer.line_iterator(top_byte, 80);
            for _ in 0..visible_height {
                if let Some((line_start, line_content)) = line_iter.next_line() {
                    visible_end = line_start + line_content.len();
                } else {
                    break;
                }
            }
        }

        // Ensure we don't go past buffer end
        visible_end = visible_end.min(state.buffer.len());

        // Get the visible text
        let visible_text = state.get_text_range(visible_start, visible_end);

        // Find all matches using regex
        for mat in regex.find_iter(&visible_text) {
            let absolute_pos = visible_start + mat.start();
            let match_len = mat.end() - mat.start();

            // Add overlay for this match
            let search_style = ratatui::style::Style::default().fg(search_fg).bg(search_bg);
            let overlay = crate::view::overlay::Overlay::with_namespace(
                &mut state.marker_list,
                absolute_pos..(absolute_pos + match_len),
                crate::view::overlay::OverlayFace::Style {
                    style: search_style,
                },
                ns.clone(),
            )
            .with_priority_value(10); // Priority - above syntax highlighting

            state.overlays.add(overlay);
        }
    }

    /// Perform a search and update search state
    pub(super) fn perform_search(&mut self, query: &str) {
        // Don't clear search highlights here - keep them from incremental search
        // They will be cleared when:
        // 1. User cancels search (Escape)
        // 2. User makes an edit to the buffer
        // 3. User starts a new search (update_search_highlights clears old ones)

        if query.is_empty() {
            self.search_state = None;
            self.set_status_message(t!("search.cancelled").to_string());
            return;
        }

        let search_range = self.pending_search_range.take();

        // For large files with lazy loading, we need to load the entire buffer
        // before searching. This ensures the search can access all content.
        // (Issue #657: Search on large plain text files)
        let buffer_content = {
            let state = self.active_state_mut();
            let total_bytes = state.buffer.len();

            // Force-load the entire buffer if not already loaded
            // get_text_range_mut() handles lazy loading and returns the content
            match state.buffer.get_text_range_mut(0, total_bytes) {
                Ok(bytes) => String::from_utf8_lossy(&bytes).into_owned(),
                Err(e) => {
                    tracing::warn!("Failed to load buffer for search: {}", e);
                    self.set_status_message(t!("error.buffer_not_loaded").to_string());
                    return;
                }
            }
        };

        // Get search settings
        let case_sensitive = self.search_case_sensitive;
        let whole_word = self.search_whole_word;
        let use_regex = self.search_use_regex;

        // Determine search boundaries
        let (search_start, search_end) = if let Some(ref range) = search_range {
            (range.start, range.end)
        } else {
            (0, buffer_content.len())
        };

        // Build regex pattern
        let regex_pattern = if use_regex {
            if whole_word {
                format!(r"\b{}\b", query)
            } else {
                query.to_string()
            }
        } else {
            let escaped = regex::escape(query);
            if whole_word {
                format!(r"\b{}\b", escaped)
            } else {
                escaped
            }
        };

        // Build regex with case sensitivity
        let regex = match regex::RegexBuilder::new(&regex_pattern)
            .case_insensitive(!case_sensitive)
            .build()
        {
            Ok(r) => r,
            Err(e) => {
                self.search_state = None;
                self.set_status_message(
                    t!("error.invalid_regex", error = e.to_string()).to_string(),
                );
                return;
            }
        };

        // Find all matches within the search range (store position and length for overlays)
        let search_slice = &buffer_content[search_start..search_end];
        let match_ranges: Vec<(usize, usize)> = regex
            .find_iter(search_slice)
            .map(|m| (search_start + m.start(), m.end() - m.start()))
            .collect();

        if match_ranges.is_empty() {
            self.search_state = None;
            let msg = if search_range.is_some() {
                format!("No matches found for '{}' in selection", query)
            } else {
                format!("No matches found for '{}'", query)
            };
            self.set_status_message(msg);
            return;
        }

        // Extract just positions for search_state.matches
        let matches: Vec<usize> = match_ranges.iter().map(|(pos, _)| *pos).collect();

        // Create overlays for ALL matches (not just visible ones)
        // This ensures F3 can find matches outside viewport and markers track through edits
        {
            let search_bg = self.theme.search_match_bg;
            let search_fg = self.theme.search_match_fg;
            let ns = self.search_namespace.clone();
            let state = self.active_state_mut();

            // Clear existing (visible-only) overlays from incremental search
            state.overlays.clear_namespace(&ns, &mut state.marker_list);

            // Create overlays for all matches
            for &(match_pos, match_len) in &match_ranges {
                let search_style = ratatui::style::Style::default().fg(search_fg).bg(search_bg);
                let overlay = crate::view::overlay::Overlay::with_namespace(
                    &mut state.marker_list,
                    match_pos..(match_pos + match_len),
                    crate::view::overlay::OverlayFace::Style {
                        style: search_style,
                    },
                    ns.clone(),
                )
                .with_priority_value(10);
                state.overlays.add(overlay);
            }
        }

        // Find the first match at or after the current cursor position
        let cursor_pos = self.active_cursors().primary().position;
        let current_match_index = matches
            .iter()
            .position(|&pos| pos >= cursor_pos)
            .unwrap_or(0);

        // Move cursor to the first match
        let match_pos = matches[current_match_index];
        {
            let active_split = self.split_manager.active_split();
            let active_buffer = self.active_buffer();
            if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                view_state.cursors.primary_mut().position = match_pos;
                view_state.cursors.primary_mut().anchor = None;
                // Ensure cursor is visible
                let cursor = *view_state.cursors.primary();
                let state = self.buffers.get_mut(&active_buffer).unwrap();
                view_state
                    .viewport
                    .ensure_visible(&mut state.buffer, &cursor);
            }
        }

        let num_matches = matches.len();

        // Update search state
        self.search_state = Some(SearchState {
            query: query.to_string(),
            matches,
            current_match_index: Some(current_match_index),
            wrap_search: search_range.is_none(), // Only wrap if not searching in selection
            search_range,
        });

        let msg = if self.search_state.as_ref().unwrap().search_range.is_some() {
            format!(
                "Found {} match{} for '{}' in selection",
                num_matches,
                if num_matches == 1 { "" } else { "es" },
                query
            )
        } else {
            format!(
                "Found {} match{} for '{}'",
                num_matches,
                if num_matches == 1 { "" } else { "es" },
                query
            )
        };
        self.set_status_message(msg);
    }

    /// Get current match positions from search overlays (which use markers that track edits)
    /// This ensures positions are always up-to-date even after buffer modifications
    fn get_search_match_positions(&self) -> Vec<usize> {
        let ns = &self.search_namespace;
        let state = self.active_state();

        // Get positions from search overlay markers
        let mut positions: Vec<usize> = state
            .overlays
            .all()
            .iter()
            .filter(|o| o.namespace.as_ref() == Some(ns))
            .filter_map(|o| state.marker_list.get_position(o.start_marker))
            .collect();

        // Sort positions for consistent ordering
        positions.sort_unstable();
        positions.dedup(); // Remove any duplicates

        positions
    }

    /// Find the next match
    pub(super) fn find_next(&mut self) {
        // Get current positions from overlay markers (auto-updated with buffer edits)
        // Fall back to search_state.matches if no overlays exist (e.g., find_selection_next)
        let overlay_positions = self.get_search_match_positions();

        if let Some(ref mut search_state) = self.search_state {
            // Use overlay positions if they exist and there's no search_range
            // (selection-based search uses cached matches to respect range)
            let match_positions =
                if !overlay_positions.is_empty() && search_state.search_range.is_none() {
                    overlay_positions
                } else {
                    search_state.matches.clone()
                };

            if match_positions.is_empty() {
                return;
            }

            let current_index = search_state.current_match_index.unwrap_or(0);
            let next_index = if current_index + 1 < match_positions.len() {
                current_index + 1
            } else if search_state.wrap_search {
                0 // Wrap to beginning
            } else {
                self.set_status_message(t!("search.no_matches").to_string());
                return;
            };

            search_state.current_match_index = Some(next_index);
            let match_pos = match_positions[next_index];
            let matches_len = match_positions.len();

            {
                let active_split = self.split_manager.active_split();
                let active_buffer = self.active_buffer();
                if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                    view_state.cursors.primary_mut().position = match_pos;
                    view_state.cursors.primary_mut().anchor = None;
                    // Ensure cursor is visible
                    let cursor = *view_state.cursors.primary();
                    let state = self.buffers.get_mut(&active_buffer).unwrap();
                    view_state
                        .viewport
                        .ensure_visible(&mut state.buffer, &cursor);
                }
            }

            self.set_status_message(
                t!(
                    "search.match_of",
                    current = next_index + 1,
                    total = matches_len
                )
                .to_string(),
            );
        } else {
            let find_key = self
                .get_keybinding_for_action("find")
                .unwrap_or_else(|| "Ctrl+F".to_string());
            self.set_status_message(t!("search.no_active", find_key = find_key).to_string());
        }
    }

    /// Find the previous match
    pub(super) fn find_previous(&mut self) {
        // Get current positions from overlay markers first (auto-updated with buffer edits)
        // Fall back to search_state.matches if no overlays exist (e.g., find_selection_previous)
        let overlay_positions = self.get_search_match_positions();

        if let Some(ref mut search_state) = self.search_state {
            // Use overlay positions if:
            // 1. They exist (overlays were created)
            // 2. There's no search_range (selection-based search uses cached matches to respect range)
            let match_positions =
                if !overlay_positions.is_empty() && search_state.search_range.is_none() {
                    overlay_positions
                } else {
                    search_state.matches.clone()
                };

            if match_positions.is_empty() {
                return;
            }

            let current_index = search_state.current_match_index.unwrap_or(0);
            let prev_index = if current_index > 0 {
                current_index - 1
            } else if search_state.wrap_search {
                match_positions.len() - 1 // Wrap to end
            } else {
                self.set_status_message(t!("search.no_matches").to_string());
                return;
            };

            search_state.current_match_index = Some(prev_index);
            let match_pos = match_positions[prev_index];
            let matches_len = match_positions.len();

            {
                let active_split = self.split_manager.active_split();
                let active_buffer = self.active_buffer();
                if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                    view_state.cursors.primary_mut().position = match_pos;
                    view_state.cursors.primary_mut().anchor = None;
                    // Ensure cursor is visible
                    let cursor = *view_state.cursors.primary();
                    let state = self.buffers.get_mut(&active_buffer).unwrap();
                    view_state
                        .viewport
                        .ensure_visible(&mut state.buffer, &cursor);
                }
            }

            self.set_status_message(
                t!(
                    "search.match_of",
                    current = prev_index + 1,
                    total = matches_len
                )
                .to_string(),
            );
        } else {
            let find_key = self
                .get_keybinding_for_action("find")
                .unwrap_or_else(|| "Ctrl+F".to_string());
            self.set_status_message(t!("search.no_active", find_key = find_key).to_string());
        }
    }

    /// Find the next occurrence of the current selection (or word under cursor).
    /// This is a "quick find" that doesn't require opening the search panel.
    /// The search term is stored so subsequent Alt+N/Alt+P/F3 navigation works.
    ///
    /// If there's already an active search, this continues with the same search term.
    /// Otherwise, it starts a new search with the current selection or word under cursor.
    pub(super) fn find_selection_next(&mut self) {
        // If there's already a search active AND cursor is at a match position,
        // just continue to next match. Otherwise, clear and start fresh.
        if let Some(ref search_state) = self.search_state {
            let cursor_pos = self.active_cursors().primary().position;
            if search_state.matches.contains(&cursor_pos) {
                self.find_next();
                return;
            }
            // Cursor moved away from a match - clear search state
        }
        self.search_state = None;

        // No active search - start a new one with selection or word under cursor
        let (search_text, selection_start) = self.get_selection_or_word_for_search_with_pos();

        match search_text {
            Some(text) if !text.is_empty() => {
                // Record cursor position before search
                let cursor_before = self.active_cursors().primary().position;

                // Perform the search to set up search state
                self.perform_search(&text);

                // Check if we need to move to next match
                if let Some(ref search_state) = self.search_state {
                    let cursor_after = self.active_cursors().primary().position;

                    // If we started at a match (selection_start matches a search result),
                    // and perform_search didn't move us (or moved us to the same match),
                    // then we need to find_next
                    let started_at_match = selection_start
                        .map(|start| search_state.matches.contains(&start))
                        .unwrap_or(false);

                    let landed_at_start = selection_start
                        .map(|start| cursor_after == start)
                        .unwrap_or(false);

                    // Only call find_next if:
                    // 1. We started at a match AND landed back at it, OR
                    // 2. We didn't move at all
                    if ((started_at_match && landed_at_start) || cursor_before == cursor_after)
                        && search_state.matches.len() > 1
                    {
                        self.find_next();
                    }
                }
            }
            _ => {
                self.set_status_message(t!("search.no_text").to_string());
            }
        }
    }

    /// Find the previous occurrence of the current selection (or word under cursor).
    /// This is a "quick find" that doesn't require opening the search panel.
    ///
    /// If there's already an active search, this continues with the same search term.
    /// Otherwise, it starts a new search with the current selection or word under cursor.
    pub(super) fn find_selection_previous(&mut self) {
        // If there's already a search active AND cursor is at a match position,
        // just continue to previous match. Otherwise, clear and start fresh.
        if let Some(ref search_state) = self.search_state {
            let cursor_pos = self.active_cursors().primary().position;
            if search_state.matches.contains(&cursor_pos) {
                self.find_previous();
                return;
            }
            // Cursor moved away from a match - clear search state
        }
        self.search_state = None;

        // No active search - start a new one with selection or word under cursor
        let (search_text, selection_start) = self.get_selection_or_word_for_search_with_pos();

        match search_text {
            Some(text) if !text.is_empty() => {
                // Record cursor position before search
                let cursor_before = self.active_cursors().primary().position;

                // Perform the search to set up search state
                self.perform_search(&text);

                // If we found matches, navigate to previous
                if let Some(ref search_state) = self.search_state {
                    let cursor_after = self.active_cursors().primary().position;

                    // Check if we started at a match
                    let started_at_match = selection_start
                        .map(|start| search_state.matches.contains(&start))
                        .unwrap_or(false);

                    let landed_at_start = selection_start
                        .map(|start| cursor_after == start)
                        .unwrap_or(false);

                    // For find previous, we always need to call find_previous at least once.
                    // If we landed at our starting match, we need to go back once to get previous.
                    // If we landed at a different match (because cursor was past start of selection),
                    // we still want to find_previous to get to where we should be.
                    if started_at_match && landed_at_start {
                        // We're at the same match we started at, go to previous
                        self.find_previous();
                    } else if cursor_before != cursor_after {
                        // perform_search moved us, now go back to find the actual previous
                        // from our original position (which is before where we landed)
                        self.find_previous();
                    } else {
                        // Cursor didn't move, just find previous
                        self.find_previous();
                    }
                }
            }
            _ => {
                self.set_status_message(t!("search.no_text").to_string());
            }
        }
    }

    /// Get the text to search for from selection or word under cursor,
    /// along with the start position of that text (for determining if we're at a match).
    fn get_selection_or_word_for_search_with_pos(&mut self) -> (Option<String>, Option<usize>) {
        use crate::primitives::word_navigation::{find_word_end, find_word_start};

        // First get selection range and cursor position with immutable borrow
        let (selection_range, cursor_pos) = {
            let primary = self.active_cursors().primary();
            (primary.selection_range(), primary.position)
        };

        // Check if there's a selection
        if let Some(range) = selection_range {
            let state = self.active_state_mut();
            let text = state.get_text_range(range.start, range.end);
            if !text.is_empty() {
                return (Some(text), Some(range.start));
            }
        }

        // No selection - try to get word under cursor
        let (word_start, word_end) = {
            let state = self.active_state();
            let word_start = find_word_start(&state.buffer, cursor_pos);
            let word_end = find_word_end(&state.buffer, cursor_pos);
            (word_start, word_end)
        };

        if word_start < word_end {
            let state = self.active_state_mut();
            (
                Some(state.get_text_range(word_start, word_end)),
                Some(word_start),
            )
        } else {
            (None, None)
        }
    }

    /// Perform a replace-all operation
    /// Build a compiled byte-regex for replace operations using current search settings.
    /// Returns None when regex mode is off (plain text matching should be used).
    fn build_replace_regex(&self, search: &str) -> Option<regex::bytes::Regex> {
        super::regex_replace::build_regex(
            search,
            self.search_use_regex,
            self.search_whole_word,
            self.search_case_sensitive,
        )
    }

    /// Get the length of a regex match at a given position in the buffer.
    fn get_regex_match_len(&mut self, regex: &regex::bytes::Regex, pos: usize) -> Option<usize> {
        let state = self.active_state_mut();
        let remaining = state.buffer.len().saturating_sub(pos);
        if remaining == 0 {
            return None;
        }
        let bytes = state.buffer.get_text_range_mut(pos, remaining).ok()?;
        regex.find(&bytes).map(|m| m.len())
    }

    /// Expand capture group references (e.g. $1, $2, ${name}) in the replacement string
    /// for a regex match at the given buffer position. Returns the expanded replacement.
    fn expand_regex_replacement(
        &mut self,
        regex: &regex::bytes::Regex,
        pos: usize,
        match_len: usize,
        replacement: &str,
    ) -> String {
        let state = self.active_state_mut();
        if let Ok(bytes) = state.buffer.get_text_range_mut(pos, match_len) {
            return super::regex_replace::expand_replacement(regex, &bytes, replacement);
        }
        replacement.to_string()
    }

    /// Replaces all occurrences of the search query with the replacement text
    ///
    /// OPTIMIZATION: Uses BulkEdit for O(n) tree operations instead of O(n²)
    /// This directly edits the piece tree without loading the entire buffer into memory
    pub(super) fn perform_replace(&mut self, search: &str, replacement: &str) {
        if search.is_empty() {
            self.set_status_message(t!("replace.empty_query").to_string());
            return;
        }

        let compiled_regex = self.build_replace_regex(search);

        // Find all matches first (before making any modifications)
        // Each match is (position, length, expanded_replacement)
        let matches: Vec<(usize, usize, String)> = if let Some(ref regex) = compiled_regex {
            // Regex mode: load buffer content as bytes and find all matches
            // with capture group expansion in the replacement template
            let buffer_bytes = {
                let state = self.active_state_mut();
                let total_bytes = state.buffer.len();
                match state.buffer.get_text_range_mut(0, total_bytes) {
                    Ok(bytes) => bytes,
                    Err(e) => {
                        tracing::warn!("Failed to load buffer for replace: {}", e);
                        self.set_status_message(t!("error.buffer_not_loaded").to_string());
                        return;
                    }
                }
            };
            super::regex_replace::collect_regex_matches(regex, &buffer_bytes, replacement)
                .into_iter()
                .map(|m| (m.offset, m.len, m.replacement))
                .collect()
        } else {
            // Plain text mode - replacement is used literally
            let state = self.active_state();
            let buffer_len = state.buffer.len();
            let mut matches = Vec::new();
            let mut current_pos = 0;

            while current_pos < buffer_len {
                if let Some(offset) = state.buffer.find_next_in_range(
                    search,
                    current_pos,
                    Some(current_pos..buffer_len),
                ) {
                    matches.push((offset, search.len(), replacement.to_string()));
                    current_pos = offset + search.len();
                } else {
                    break;
                }
            }
            matches
        };

        let count = matches.len();

        if count == 0 {
            self.set_status_message(t!("search.no_occurrences", search = search).to_string());
            return;
        }

        // Get cursor info for the event
        let cursor_id = self.active_cursors().primary_id();

        // Create Delete+Insert events for each match
        // Events will be processed in reverse order by apply_events_as_bulk_edit
        let mut events = Vec::with_capacity(count * 2);
        for (match_pos, match_len, expanded_replacement) in &matches {
            // Get the actual matched text for the delete event
            let deleted_text = self
                .active_state_mut()
                .get_text_range(*match_pos, match_pos + match_len);
            // Delete the matched text
            events.push(Event::Delete {
                range: *match_pos..match_pos + match_len,
                deleted_text,
                cursor_id,
            });
            // Insert the replacement (with capture groups expanded)
            events.push(Event::Insert {
                position: *match_pos,
                text: expanded_replacement.clone(),
                cursor_id,
            });
        }

        // Apply all replacements using BulkEdit for O(n) performance
        let description = format!("Replace all '{}' with '{}'", search, replacement);
        if let Some(bulk_edit) = self.apply_events_as_bulk_edit(events, description) {
            self.active_event_log_mut().append(bulk_edit);
        }

        // Clear search state since positions are now invalid
        self.search_state = None;

        // Clear any search highlight overlays
        let ns = self.search_namespace.clone();
        let state = self.active_state_mut();
        state.overlays.clear_namespace(&ns, &mut state.marker_list);

        // Set status message
        self.set_status_message(
            t!(
                "search.replaced",
                count = count,
                search = search,
                replace = replacement
            )
            .to_string(),
        );
    }

    /// Start interactive replace mode (query-replace)
    pub(super) fn start_interactive_replace(&mut self, search: &str, replacement: &str) {
        if search.is_empty() {
            self.set_status_message(t!("replace.query_empty").to_string());
            return;
        }

        let compiled_regex = self.build_replace_regex(search);

        // Find the first match lazily (don't find all matches upfront)
        let start_pos = self.active_cursors().primary().position;
        let (first_match_pos, first_match_len) = if let Some(ref regex) = compiled_regex {
            let state = self.active_state();
            let buffer_len = state.buffer.len();
            // Try from cursor to end, then wrap from beginning
            let found = state
                .buffer
                .find_next_regex_in_range(regex, start_pos, Some(start_pos..buffer_len))
                .or_else(|| {
                    if start_pos > 0 {
                        state
                            .buffer
                            .find_next_regex_in_range(regex, 0, Some(0..start_pos))
                    } else {
                        None
                    }
                });
            let Some(pos) = found else {
                self.set_status_message(t!("search.no_occurrences", search = search).to_string());
                return;
            };
            // Determine the match length by re-matching at the found position
            let match_len = self.get_regex_match_len(regex, pos).unwrap_or(search.len());
            (pos, match_len)
        } else {
            let state = self.active_state();
            let Some(pos) = state.buffer.find_next(search, start_pos) else {
                self.set_status_message(t!("search.no_occurrences", search = search).to_string());
                return;
            };
            (pos, search.len())
        };

        // Initialize interactive replace state with just the current match
        self.interactive_replace_state = Some(InteractiveReplaceState {
            search: search.to_string(),
            replacement: replacement.to_string(),
            current_match_pos: first_match_pos,
            current_match_len: first_match_len,
            start_pos: first_match_pos,
            has_wrapped: false,
            replacements_made: 0,
            regex: compiled_regex,
        });

        // Move cursor to first match
        let active_split = self.split_manager.active_split();
        let active_buffer = self.active_buffer();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.cursors.primary_mut().position = first_match_pos;
            view_state.cursors.primary_mut().anchor = None;
            // Ensure cursor is visible
            let cursor = *view_state.cursors.primary();
            let state = self.buffers.get_mut(&active_buffer).unwrap();
            view_state
                .viewport
                .ensure_visible(&mut state.buffer, &cursor);
        }

        // Show the query-replace prompt
        self.prompt = Some(Prompt::new(
            "Replace? (y)es (n)o (a)ll (c)ancel: ".to_string(),
            PromptType::QueryReplaceConfirm,
        ));
    }

    /// Handle interactive replace key press (y/n/a/c)
    pub(super) fn handle_interactive_replace_key(&mut self, c: char) -> AnyhowResult<()> {
        let state = self.interactive_replace_state.clone();
        let Some(mut ir_state) = state else {
            return Ok(());
        };

        match c {
            'y' | 'Y' => {
                // Replace current match
                self.replace_current_match(&ir_state)?;
                ir_state.replacements_made += 1;

                // Find next match lazily (after the replacement)
                let search_pos = ir_state.current_match_pos + ir_state.replacement.len();
                if let Some((next_match, match_len, wrapped)) =
                    self.find_next_match_for_replace(&ir_state, search_pos)
                {
                    ir_state.current_match_pos = next_match;
                    ir_state.current_match_len = match_len;
                    if wrapped {
                        ir_state.has_wrapped = true;
                    }
                    self.interactive_replace_state = Some(ir_state.clone());
                    self.move_to_current_match(&ir_state);
                } else {
                    self.finish_interactive_replace(ir_state.replacements_made);
                }
            }
            'n' | 'N' => {
                // Skip current match and find next
                let search_pos = ir_state.current_match_pos + ir_state.current_match_len;
                if let Some((next_match, match_len, wrapped)) =
                    self.find_next_match_for_replace(&ir_state, search_pos)
                {
                    ir_state.current_match_pos = next_match;
                    ir_state.current_match_len = match_len;
                    if wrapped {
                        ir_state.has_wrapped = true;
                    }
                    self.interactive_replace_state = Some(ir_state.clone());
                    self.move_to_current_match(&ir_state);
                } else {
                    self.finish_interactive_replace(ir_state.replacements_made);
                }
            }
            'a' | 'A' | '!' => {
                // Replace all remaining matches with SINGLE confirmation
                // Undo behavior: ONE undo step undoes ALL remaining replacements
                //
                // OPTIMIZATION: Uses BulkEdit for O(n) tree operations instead of O(n²)
                // This directly edits the piece tree without loading the entire buffer

                // Collect ALL match positions and lengths including the current match
                // Start from the current match position
                let all_matches: Vec<(usize, usize)> = {
                    let mut matches = Vec::new();
                    let mut temp_state = ir_state.clone();
                    temp_state.has_wrapped = false; // Reset wrap state to find current match

                    // First, include the current match
                    matches.push((ir_state.current_match_pos, ir_state.current_match_len));
                    let mut current_pos = ir_state.current_match_pos + ir_state.current_match_len;

                    // Find all remaining matches
                    while let Some((next_match, match_len, wrapped)) =
                        self.find_next_match_for_replace(&temp_state, current_pos)
                    {
                        matches.push((next_match, match_len));
                        current_pos = next_match + match_len;
                        if wrapped {
                            temp_state.has_wrapped = true;
                        }
                    }
                    matches
                };

                let total_count = all_matches.len();

                if total_count > 0 {
                    // Get cursor info for the event
                    let cursor_id = self.active_cursors().primary_id();

                    // Create Delete+Insert events for each match
                    let mut events = Vec::with_capacity(total_count * 2);
                    for &(match_pos, match_len) in &all_matches {
                        let deleted_text = self
                            .active_state_mut()
                            .get_text_range(match_pos, match_pos + match_len);
                        // Expand capture group references if in regex mode
                        let replacement_text = if let Some(ref regex) = ir_state.regex {
                            self.expand_regex_replacement(
                                regex,
                                match_pos,
                                match_len,
                                &ir_state.replacement,
                            )
                        } else {
                            ir_state.replacement.clone()
                        };
                        events.push(Event::Delete {
                            range: match_pos..match_pos + match_len,
                            deleted_text,
                            cursor_id,
                        });
                        events.push(Event::Insert {
                            position: match_pos,
                            text: replacement_text,
                            cursor_id,
                        });
                    }

                    // Apply all replacements using BulkEdit for O(n) performance
                    let description = format!(
                        "Replace all {} occurrences of '{}' with '{}'",
                        total_count, ir_state.search, ir_state.replacement
                    );
                    if let Some(bulk_edit) = self.apply_events_as_bulk_edit(events, description) {
                        self.active_event_log_mut().append(bulk_edit);
                    }

                    ir_state.replacements_made += total_count;
                }

                self.finish_interactive_replace(ir_state.replacements_made);
            }
            'c' | 'C' | 'q' | 'Q' | '\x1b' => {
                // Cancel/quit interactive replace
                self.finish_interactive_replace(ir_state.replacements_made);
            }
            _ => {
                // Unknown key - ignored (prompt shows valid options)
            }
        }

        Ok(())
    }

    /// Find the next match for interactive replace (lazy search with wrap-around)
    /// Returns (match_position, match_length, wrapped)
    pub(super) fn find_next_match_for_replace(
        &mut self,
        ir_state: &InteractiveReplaceState,
        start_pos: usize,
    ) -> Option<(usize, usize, bool)> {
        if let Some(ref regex) = ir_state.regex {
            // Regex mode
            let regex = regex.clone();
            let state = self.active_state();
            let buffer_len = state.buffer.len();

            if ir_state.has_wrapped {
                let search_range = Some(start_pos..ir_state.start_pos);
                if let Some(match_pos) =
                    state
                        .buffer
                        .find_next_regex_in_range(&regex, start_pos, search_range)
                {
                    let match_len = self.get_regex_match_len(&regex, match_pos).unwrap_or(0);
                    return Some((match_pos, match_len, true));
                }
                None
            } else {
                let search_range = Some(start_pos..buffer_len);
                if let Some(match_pos) =
                    state
                        .buffer
                        .find_next_regex_in_range(&regex, start_pos, search_range)
                {
                    let match_len = self.get_regex_match_len(&regex, match_pos).unwrap_or(0);
                    return Some((match_pos, match_len, false));
                }

                // Wrap to beginning
                let wrap_range = Some(0..ir_state.start_pos);
                let state = self.active_state();
                if let Some(match_pos) =
                    state.buffer.find_next_regex_in_range(&regex, 0, wrap_range)
                {
                    let match_len = self.get_regex_match_len(&regex, match_pos).unwrap_or(0);
                    return Some((match_pos, match_len, true));
                }

                None
            }
        } else {
            // Plain text mode
            let search_len = ir_state.search.len();
            let state = self.active_state();

            if ir_state.has_wrapped {
                let search_range = Some(start_pos..ir_state.start_pos);
                if let Some(match_pos) =
                    state
                        .buffer
                        .find_next_in_range(&ir_state.search, start_pos, search_range)
                {
                    return Some((match_pos, search_len, true));
                }
                None
            } else {
                let buffer_len = state.buffer.len();
                let search_range = Some(start_pos..buffer_len);
                if let Some(match_pos) =
                    state
                        .buffer
                        .find_next_in_range(&ir_state.search, start_pos, search_range)
                {
                    return Some((match_pos, search_len, false));
                }

                let wrap_range = Some(0..ir_state.start_pos);
                if let Some(match_pos) =
                    state
                        .buffer
                        .find_next_in_range(&ir_state.search, 0, wrap_range)
                {
                    return Some((match_pos, search_len, true));
                }

                None
            }
        }
    }

    /// Replace the current match in interactive replace mode
    pub(super) fn replace_current_match(
        &mut self,
        ir_state: &InteractiveReplaceState,
    ) -> AnyhowResult<()> {
        let match_pos = ir_state.current_match_pos;
        let match_len = ir_state.current_match_len;
        let range = match_pos..(match_pos + match_len);

        // Expand capture group references if in regex mode
        let replacement_text = if let Some(ref regex) = ir_state.regex {
            self.expand_regex_replacement(regex, match_pos, match_len, &ir_state.replacement)
        } else {
            ir_state.replacement.clone()
        };

        // Get the deleted text for the event
        let deleted_text = self
            .active_state_mut()
            .get_text_range(range.start, range.end);

        // Capture current cursor state for undo
        let cursor_id = self.active_cursors().primary_id();
        let cursor = *self.active_cursors().primary();
        let old_position = cursor.position;
        let old_anchor = cursor.anchor;
        let old_sticky_column = cursor.sticky_column;

        // Create events: MoveCursor, Delete, Insert
        // The MoveCursor saves the cursor position so undo can restore it
        let events = vec![
            Event::MoveCursor {
                cursor_id,
                old_position,
                new_position: match_pos,
                old_anchor,
                new_anchor: None,
                old_sticky_column,
                new_sticky_column: 0,
            },
            Event::Delete {
                range: range.clone(),
                deleted_text,
                cursor_id,
            },
            Event::Insert {
                position: match_pos,
                text: replacement_text,
                cursor_id,
            },
        ];

        // Wrap in batch for atomic undo
        let batch = Event::Batch {
            events,
            description: format!(
                "Query replace '{}' with '{}'",
                ir_state.search, ir_state.replacement
            ),
        };

        // Apply the batch through the event log
        self.active_event_log_mut().append(batch.clone());
        self.apply_event_to_active_buffer(&batch);

        Ok(())
    }

    /// Move cursor to the current match in interactive replace
    pub(super) fn move_to_current_match(&mut self, ir_state: &InteractiveReplaceState) {
        let match_pos = ir_state.current_match_pos;
        let active_split = self.split_manager.active_split();
        let active_buffer = self.active_buffer();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.cursors.primary_mut().position = match_pos;
            view_state.cursors.primary_mut().anchor = None;
            // Ensure cursor is visible
            let cursor = *view_state.cursors.primary();
            let state = self.buffers.get_mut(&active_buffer).unwrap();
            view_state
                .viewport
                .ensure_visible(&mut state.buffer, &cursor);
        }

        // Update the prompt message (show [Wrapped] if we've wrapped around)
        let msg = if ir_state.has_wrapped {
            "[Wrapped] Replace? (y)es (n)o (a)ll (c)ancel: ".to_string()
        } else {
            "Replace? (y)es (n)o (a)ll (c)ancel: ".to_string()
        };
        if let Some(ref mut prompt) = self.prompt {
            if prompt.prompt_type == PromptType::QueryReplaceConfirm {
                prompt.message = msg;
                prompt.input.clear();
                prompt.cursor_pos = 0;
            }
        }
    }

    /// Finish interactive replace and show summary
    pub(super) fn finish_interactive_replace(&mut self, replacements_made: usize) {
        self.interactive_replace_state = None;
        self.prompt = None; // Clear the query-replace prompt

        // Clear search highlights
        let ns = self.search_namespace.clone();
        let state = self.active_state_mut();
        state.overlays.clear_namespace(&ns, &mut state.marker_list);

        self.set_status_message(t!("search.replaced_count", count = replacements_made).to_string());
    }

    /// Smart home: toggle between line start and first non-whitespace character
    pub(super) fn smart_home(&mut self) {
        let estimated_line_length = self.config.editor.estimated_line_length;
        let cursor = *self.active_cursors().primary();
        let cursor_id = self.active_cursors().primary_id();
        let state = self.active_state_mut();

        // Get line information
        let mut iter = state
            .buffer
            .line_iterator(cursor.position, estimated_line_length);
        if let Some((line_start, line_content)) = iter.next_line() {
            // Find first non-whitespace character
            let first_non_ws = line_content
                .chars()
                .take_while(|c| *c != '\n')
                .position(|c| !c.is_whitespace())
                .map(|offset| line_start + offset)
                .unwrap_or(line_start);

            // Toggle: if at first non-ws, go to line start; otherwise go to first non-ws
            let new_pos = if cursor.position == first_non_ws {
                line_start
            } else {
                first_non_ws
            };

            let event = Event::MoveCursor {
                cursor_id,
                old_position: cursor.position,
                new_position: new_pos,
                old_anchor: cursor.anchor,
                new_anchor: None,
                old_sticky_column: cursor.sticky_column,
                new_sticky_column: 0,
            };

            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);
        }
    }

    /// Toggle comment on the current line or selection
    pub(super) fn toggle_comment(&mut self) {
        // Determine comment prefix from language config
        // If no language detected or no comment prefix configured, do nothing
        let language = &self.active_state().language;
        let comment_prefix = self
            .config
            .languages
            .get(language)
            .and_then(|lang_config| lang_config.comment_prefix.clone());

        let comment_prefix: String = match comment_prefix {
            Some(prefix) => {
                // Ensure there's a trailing space for consistent formatting
                if prefix.ends_with(' ') {
                    prefix
                } else {
                    format!("{} ", prefix)
                }
            }
            None => return, // No comment prefix for this language, do nothing
        };

        let estimated_line_length = self.config.editor.estimated_line_length;

        let cursor = *self.active_cursors().primary();
        let cursor_id = self.active_cursors().primary_id();
        let state = self.active_state_mut();

        // Save original selection info to restore after edit
        let original_anchor = cursor.anchor;
        let original_position = cursor.position;
        let had_selection = original_anchor.is_some();

        let (start_pos, end_pos) = if let Some(range) = cursor.selection_range() {
            (range.start, range.end)
        } else {
            let iter = state
                .buffer
                .line_iterator(cursor.position, estimated_line_length);
            let line_start = iter.current_position();
            (line_start, cursor.position)
        };

        // Find all line starts in the range
        let buffer_len = state.buffer.len();
        let mut line_starts = Vec::new();
        let mut iter = state.buffer.line_iterator(start_pos, estimated_line_length);
        let mut current_pos = iter.current_position();
        line_starts.push(current_pos);

        while let Some((_, content)) = iter.next_line() {
            current_pos += content.len();
            if current_pos >= end_pos || current_pos >= buffer_len {
                break;
            }
            let next_iter = state
                .buffer
                .line_iterator(current_pos, estimated_line_length);
            let next_start = next_iter.current_position();
            if next_start != *line_starts.last().unwrap() {
                line_starts.push(next_start);
            }
            iter = state
                .buffer
                .line_iterator(current_pos, estimated_line_length);
        }

        // Determine if we should comment or uncomment
        // If all lines are commented, uncomment; otherwise comment
        let all_commented = line_starts.iter().all(|&line_start| {
            let line_bytes = state
                .buffer
                .slice_bytes(line_start..buffer_len.min(line_start + comment_prefix.len() + 10));
            let line_str = String::from_utf8_lossy(&line_bytes);
            let trimmed = line_str.trim_start();
            trimmed.starts_with(comment_prefix.trim())
        });

        let mut events = Vec::new();
        // Track (edit_position, byte_delta) for calculating new cursor positions
        // delta is positive for insertions, negative for deletions
        let mut position_deltas: Vec<(usize, isize)> = Vec::new();

        if all_commented {
            // Uncomment: remove comment prefix from each line
            for &line_start in line_starts.iter().rev() {
                let line_bytes = state
                    .buffer
                    .slice_bytes(line_start..buffer_len.min(line_start + 100));
                let line_str = String::from_utf8_lossy(&line_bytes);

                // Find where the comment prefix starts (after leading whitespace)
                let leading_ws: usize = line_str
                    .chars()
                    .take_while(|c| c.is_whitespace() && *c != '\n')
                    .map(|c| c.len_utf8())
                    .sum();
                let rest = &line_str[leading_ws..];

                if rest.starts_with(comment_prefix.trim()) {
                    let remove_len = if rest.starts_with(&comment_prefix) {
                        comment_prefix.len()
                    } else {
                        comment_prefix.trim().len()
                    };
                    let deleted_text = String::from_utf8_lossy(&state.buffer.slice_bytes(
                        line_start + leading_ws..line_start + leading_ws + remove_len,
                    ))
                    .to_string();
                    events.push(Event::Delete {
                        range: (line_start + leading_ws)..(line_start + leading_ws + remove_len),
                        deleted_text,
                        cursor_id,
                    });
                    position_deltas.push((line_start, -(remove_len as isize)));
                }
            }
        } else {
            // Comment: add comment prefix to each line
            let prefix_len = comment_prefix.len();
            for &line_start in line_starts.iter().rev() {
                events.push(Event::Insert {
                    position: line_start,
                    text: comment_prefix.to_string(),
                    cursor_id,
                });
                position_deltas.push((line_start, prefix_len as isize));
            }
        }

        if events.is_empty() {
            return;
        }

        let action_desc = if all_commented {
            "Uncomment"
        } else {
            "Comment"
        };

        // If there was a selection, add a MoveCursor event to restore it
        if had_selection {
            // Sort deltas by position ascending for calculation
            position_deltas.sort_by_key(|(pos, _)| *pos);

            // Calculate cumulative shift for a position based on edits at or before that position
            let calc_shift = |original_pos: usize| -> isize {
                let mut shift: isize = 0;
                for (edit_pos, delta) in &position_deltas {
                    if *edit_pos < original_pos {
                        shift += delta;
                    }
                }
                shift
            };

            let anchor_shift = calc_shift(original_anchor.unwrap_or(0));
            let position_shift = calc_shift(original_position);

            let new_anchor = (original_anchor.unwrap_or(0) as isize + anchor_shift).max(0) as usize;
            let new_position = (original_position as isize + position_shift).max(0) as usize;

            events.push(Event::MoveCursor {
                cursor_id,
                old_position: original_position,
                new_position,
                old_anchor: original_anchor,
                new_anchor: Some(new_anchor),
                old_sticky_column: 0,
                new_sticky_column: 0,
            });
        }

        // Use optimized bulk edit for multi-line comment toggle
        let description = format!("{} lines", action_desc);
        if let Some(bulk_edit) = self.apply_events_as_bulk_edit(events, description) {
            self.active_event_log_mut().append(bulk_edit);
        }

        self.set_status_message(
            t!(
                "lines.action",
                action = action_desc,
                count = line_starts.len()
            )
            .to_string(),
        );
    }

    /// Go to matching bracket
    pub(super) fn goto_matching_bracket(&mut self) {
        let cursor = *self.active_cursors().primary();
        let cursor_id = self.active_cursors().primary_id();
        let state = self.active_state_mut();

        let pos = cursor.position;
        if pos >= state.buffer.len() {
            self.set_status_message(t!("diagnostics.bracket_none").to_string());
            return;
        }

        let bytes = state.buffer.slice_bytes(pos..pos + 1);
        if bytes.is_empty() {
            self.set_status_message(t!("diagnostics.bracket_none").to_string());
            return;
        }

        let ch = bytes[0] as char;
        let (opening, closing, forward) = match ch {
            '(' => ('(', ')', true),
            ')' => ('(', ')', false),
            '[' => ('[', ']', true),
            ']' => ('[', ']', false),
            '{' => ('{', '}', true),
            '}' => ('{', '}', false),
            '<' => ('<', '>', true),
            '>' => ('<', '>', false),
            _ => {
                self.set_status_message(t!("diagnostics.bracket_none").to_string());
                return;
            }
        };

        // Find matching bracket
        let buffer_len = state.buffer.len();
        let mut depth = 1;
        let matching_pos = if forward {
            let mut search_pos = pos + 1;
            let mut found = None;
            while search_pos < buffer_len && depth > 0 {
                let b = state.buffer.slice_bytes(search_pos..search_pos + 1);
                if !b.is_empty() {
                    let c = b[0] as char;
                    if c == opening {
                        depth += 1;
                    } else if c == closing {
                        depth -= 1;
                        if depth == 0 {
                            found = Some(search_pos);
                        }
                    }
                }
                search_pos += 1;
            }
            found
        } else {
            let mut search_pos = pos.saturating_sub(1);
            let mut found = None;
            loop {
                let b = state.buffer.slice_bytes(search_pos..search_pos + 1);
                if !b.is_empty() {
                    let c = b[0] as char;
                    if c == closing {
                        depth += 1;
                    } else if c == opening {
                        depth -= 1;
                        if depth == 0 {
                            found = Some(search_pos);
                            break;
                        }
                    }
                }
                if search_pos == 0 {
                    break;
                }
                search_pos -= 1;
            }
            found
        };

        if let Some(new_pos) = matching_pos {
            let event = Event::MoveCursor {
                cursor_id,
                old_position: cursor.position,
                new_position: new_pos,
                old_anchor: cursor.anchor,
                new_anchor: None,
                old_sticky_column: cursor.sticky_column,
                new_sticky_column: 0,
            };
            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);
        } else {
            self.set_status_message(t!("diagnostics.bracket_no_match").to_string());
        }
    }

    /// Jump to next error/diagnostic
    pub(super) fn jump_to_next_error(&mut self) {
        let diagnostic_ns = self.lsp_diagnostic_namespace.clone();
        let cursor_pos = self.active_cursors().primary().position;
        let cursor_id = self.active_cursors().primary_id();
        let cursor = *self.active_cursors().primary();
        let state = self.active_state_mut();

        // Get all diagnostic overlay positions
        let mut diagnostic_positions: Vec<usize> = state
            .overlays
            .all()
            .iter()
            .filter_map(|overlay| {
                // Only consider LSP diagnostics (those in the diagnostic namespace)
                if overlay.namespace.as_ref() == Some(&diagnostic_ns) {
                    Some(overlay.range(&state.marker_list).start)
                } else {
                    None
                }
            })
            .collect();

        if diagnostic_positions.is_empty() {
            self.set_status_message(t!("diagnostics.none").to_string());
            return;
        }

        // Sort positions
        diagnostic_positions.sort_unstable();
        diagnostic_positions.dedup();

        // Find next diagnostic after cursor position
        let next_pos = diagnostic_positions
            .iter()
            .find(|&&pos| pos > cursor_pos)
            .or_else(|| diagnostic_positions.first()) // Wrap around
            .copied();

        if let Some(new_pos) = next_pos {
            let event = Event::MoveCursor {
                cursor_id,
                old_position: cursor.position,
                new_position: new_pos,
                old_anchor: cursor.anchor,
                new_anchor: None,
                old_sticky_column: cursor.sticky_column,
                new_sticky_column: 0,
            };
            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);

            // Show diagnostic message in status bar
            let state = self.active_state();
            if let Some(msg) = state.overlays.all().iter().find_map(|overlay| {
                let range = overlay.range(&state.marker_list);
                if range.start == new_pos && overlay.namespace.as_ref() == Some(&diagnostic_ns) {
                    overlay.message.clone()
                } else {
                    None
                }
            }) {
                self.set_status_message(msg);
            }
        }
    }

    /// Jump to previous error/diagnostic
    pub(super) fn jump_to_previous_error(&mut self) {
        let diagnostic_ns = self.lsp_diagnostic_namespace.clone();
        let cursor_pos = self.active_cursors().primary().position;
        let cursor_id = self.active_cursors().primary_id();
        let cursor = *self.active_cursors().primary();
        let state = self.active_state_mut();

        // Get all diagnostic overlay positions
        let mut diagnostic_positions: Vec<usize> = state
            .overlays
            .all()
            .iter()
            .filter_map(|overlay| {
                // Only consider LSP diagnostics (those in the diagnostic namespace)
                if overlay.namespace.as_ref() == Some(&diagnostic_ns) {
                    Some(overlay.range(&state.marker_list).start)
                } else {
                    None
                }
            })
            .collect();

        if diagnostic_positions.is_empty() {
            self.set_status_message(t!("diagnostics.none").to_string());
            return;
        }

        // Sort positions
        diagnostic_positions.sort_unstable();
        diagnostic_positions.dedup();

        // Find previous diagnostic before cursor position
        let prev_pos = diagnostic_positions
            .iter()
            .rev()
            .find(|&&pos| pos < cursor_pos)
            .or_else(|| diagnostic_positions.last()) // Wrap around
            .copied();

        if let Some(new_pos) = prev_pos {
            let event = Event::MoveCursor {
                cursor_id,
                old_position: cursor.position,
                new_position: new_pos,
                old_anchor: cursor.anchor,
                new_anchor: None,
                old_sticky_column: cursor.sticky_column,
                new_sticky_column: 0,
            };
            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);

            // Show diagnostic message in status bar
            let state = self.active_state();
            if let Some(msg) = state.overlays.all().iter().find_map(|overlay| {
                let range = overlay.range(&state.marker_list);
                if range.start == new_pos && overlay.namespace.as_ref() == Some(&diagnostic_ns) {
                    overlay.message.clone()
                } else {
                    None
                }
            }) {
                self.set_status_message(msg);
            }
        }
    }

    /// Toggle macro recording for the given register
    pub(super) fn toggle_macro_recording(&mut self, key: char) {
        if let Some(state) = &self.macro_recording {
            if state.key == key {
                // Stop recording
                self.stop_macro_recording();
            } else {
                // Recording to a different key, stop current and start new
                self.stop_macro_recording();
                self.start_macro_recording(key);
            }
        } else {
            // Start recording
            self.start_macro_recording(key);
        }
    }

    /// Start recording a macro
    pub(super) fn start_macro_recording(&mut self, key: char) {
        self.macro_recording = Some(MacroRecordingState {
            key,
            actions: Vec::new(),
        });

        // Build the stop hint dynamically from keybindings
        let stop_hint = self.build_macro_stop_hint(key);
        self.set_status_message(
            t!(
                "macro.recording_with_hint",
                key = key,
                stop_hint = stop_hint
            )
            .to_string(),
        );
    }

    /// Build a hint message for how to stop macro recording
    fn build_macro_stop_hint(&self, _key: char) -> String {
        let mut hints = Vec::new();

        // Check for F5 (stop_macro_recording)
        if let Some(stop_key) = self.get_keybinding_for_action("stop_macro_recording") {
            hints.push(stop_key);
        }

        // Get command palette keybinding
        let palette_key = self
            .get_keybinding_for_action("command_palette")
            .unwrap_or_else(|| "Ctrl+P".to_string());

        if hints.is_empty() {
            // No keybindings found, just mention command palette
            format!("{} → Stop Recording Macro", palette_key)
        } else {
            // Show keybindings and command palette
            format!("{} or {} → Stop Recording", hints.join("/"), palette_key)
        }
    }

    /// Stop recording and save the macro
    pub(super) fn stop_macro_recording(&mut self) {
        if let Some(state) = self.macro_recording.take() {
            let action_count = state.actions.len();
            let key = state.key;
            self.macros.insert(key, state.actions);
            self.last_macro_register = Some(key);

            // Build play hint
            let play_hint = self.build_macro_play_hint();
            self.set_status_message(
                t!(
                    "macro.saved",
                    key = key,
                    count = action_count,
                    play_hint = play_hint
                )
                .to_string(),
            );
        } else {
            self.set_status_message(t!("macro.not_recording").to_string());
        }
    }

    /// Build a hint message for how to play a macro
    fn build_macro_play_hint(&self) -> String {
        // Check for play_last_macro keybinding (e.g. F4)
        if let Some(play_key) = self.get_keybinding_for_action("play_last_macro") {
            return format!("{} → Play Last Macro", play_key);
        }

        // Fall back to command palette hint
        let palette_key = self
            .get_keybinding_for_action("command_palette")
            .unwrap_or_else(|| "Ctrl+P".to_string());

        format!("{} → Play Macro", palette_key)
    }

    /// Play back a recorded macro
    pub(super) fn play_macro(&mut self, key: char) {
        // Prevent recursive macro playback
        if self.macro_playing {
            return;
        }

        if let Some(actions) = self.macros.get(&key).cloned() {
            if actions.is_empty() {
                self.set_status_message(t!("macro.empty", key = key).to_string());
                return;
            }

            // Temporarily disable recording to avoid recording the playback
            let was_recording = self.macro_recording.take();
            self.macro_playing = true;

            let action_count = actions.len();
            for action in actions {
                let _ = self.handle_action(action);
            }

            // Restore recording and playing state
            self.macro_recording = was_recording;
            self.macro_playing = false;

            self.set_status_message(
                t!("macro.played", key = key, count = action_count).to_string(),
            );
        } else {
            self.set_status_message(t!("macro.not_found", key = key).to_string());
        }
    }

    /// Record an action to the current macro (if recording)
    pub(super) fn record_macro_action(&mut self, action: &Action) {
        if let Some(state) = &mut self.macro_recording {
            // Don't record macro control actions themselves
            match action {
                Action::StartMacroRecording
                | Action::StopMacroRecording
                | Action::PlayMacro(_)
                | Action::ToggleMacroRecording(_)
                | Action::ShowMacro(_)
                | Action::ListMacros
                | Action::PromptRecordMacro
                | Action::PromptPlayMacro
                | Action::PlayLastMacro => {}
                // When recording PromptConfirm, capture the current prompt text
                // so it can be replayed correctly
                Action::PromptConfirm => {
                    if let Some(prompt) = &self.prompt {
                        let text = prompt.get_text().to_string();
                        state.actions.push(Action::PromptConfirmWithText(text));
                    } else {
                        state.actions.push(action.clone());
                    }
                }
                _ => {
                    state.actions.push(action.clone());
                }
            }
        }
    }

    /// Show a macro in a buffer as JSON
    pub(super) fn show_macro_in_buffer(&mut self, key: char) {
        // Get macro data and cache what we need before any mutable borrows
        let (json, actions_len) = match self.macros.get(&key) {
            Some(actions) => {
                let json = match serde_json::to_string_pretty(actions) {
                    Ok(json) => json,
                    Err(e) => {
                        self.set_status_message(
                            t!("macro.serialize_failed", error = e.to_string()).to_string(),
                        );
                        return;
                    }
                };
                (json, actions.len())
            }
            None => {
                self.set_status_message(t!("macro.not_found", key = key).to_string());
                return;
            }
        };

        // Create header with macro info
        let content = format!(
            "// Macro '{}' ({} actions)\n// This buffer can be saved as a .json file for persistence\n\n{}",
            key,
            actions_len,
            json
        );

        // Create a new buffer for the macro
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            std::sync::Arc::clone(&self.filesystem),
        );
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs.insert(buffer_id, EventLog::new());

        // Set buffer content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer = crate::model::buffer::Buffer::from_str(
                &content,
                self.config.editor.large_file_threshold_bytes as usize,
                std::sync::Arc::clone(&self.filesystem),
            );
        }

        // Set metadata
        let metadata = BufferMetadata {
            kind: BufferKind::Virtual {
                mode: "macro-view".to_string(),
            },
            display_name: format!("*Macro {}*", key),
            lsp_enabled: false,
            lsp_disabled_reason: Some("Virtual macro buffer".to_string()),
            read_only: false, // Allow editing for saving
            binary: false,
            lsp_opened_with: std::collections::HashSet::new(),
            hidden_from_tabs: false,
            recovery_id: None,
        };
        self.buffer_metadata.insert(buffer_id, metadata);

        // Switch to the new buffer
        self.set_active_buffer(buffer_id);
        self.set_status_message(
            t!("macro.shown_buffer", key = key, count = actions_len).to_string(),
        );
    }

    /// List all recorded macros in a buffer
    pub(super) fn list_macros_in_buffer(&mut self) {
        if self.macros.is_empty() {
            self.set_status_message(t!("macro.none_recorded").to_string());
            return;
        }

        // Build a summary of all macros
        let mut content =
            String::from("// Recorded Macros\n// Use ShowMacro(key) to see details\n\n");

        let mut keys: Vec<char> = self.macros.keys().copied().collect();
        keys.sort();

        for key in keys {
            if let Some(actions) = self.macros.get(&key) {
                content.push_str(&format!("Macro '{}': {} actions\n", key, actions.len()));

                // Show all actions
                for (i, action) in actions.iter().enumerate() {
                    content.push_str(&format!("  {}. {:?}\n", i + 1, action));
                }
                content.push('\n');
            }
        }

        // Create a new buffer for the macro list
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            std::sync::Arc::clone(&self.filesystem),
        );
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs.insert(buffer_id, EventLog::new());

        // Set buffer content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer = crate::model::buffer::Buffer::from_str(
                &content,
                self.config.editor.large_file_threshold_bytes as usize,
                std::sync::Arc::clone(&self.filesystem),
            );
        }

        // Set metadata
        let metadata = BufferMetadata {
            kind: BufferKind::Virtual {
                mode: "macro-list".to_string(),
            },
            display_name: "*Macros*".to_string(),
            lsp_enabled: false,
            lsp_disabled_reason: Some("Virtual macro list buffer".to_string()),
            read_only: true,
            binary: false,
            lsp_opened_with: std::collections::HashSet::new(),
            hidden_from_tabs: false,
            recovery_id: None,
        };
        self.buffer_metadata.insert(buffer_id, metadata);

        // Switch to the new buffer
        self.set_active_buffer(buffer_id);
        self.set_status_message(t!("macro.showing", count = self.macros.len()).to_string());
    }

    /// Set a bookmark at the current position
    pub(super) fn set_bookmark(&mut self, key: char) {
        let buffer_id = self.active_buffer();
        let position = self.active_cursors().primary().position;
        self.bookmarks.insert(
            key,
            Bookmark {
                buffer_id,
                position,
            },
        );
        self.set_status_message(t!("bookmark.set", key = key).to_string());
    }

    /// Jump to a bookmark
    pub(super) fn jump_to_bookmark(&mut self, key: char) {
        if let Some(bookmark) = self.bookmarks.get(&key).cloned() {
            // Switch to the buffer if needed
            if bookmark.buffer_id != self.active_buffer() {
                if self.buffers.contains_key(&bookmark.buffer_id) {
                    self.set_active_buffer(bookmark.buffer_id);
                } else {
                    self.set_status_message(t!("bookmark.buffer_gone", key = key).to_string());
                    self.bookmarks.remove(&key);
                    return;
                }
            }

            // Move cursor to bookmark position
            let cursor = *self.active_cursors().primary();
            let cursor_id = self.active_cursors().primary_id();
            let state = self.active_state_mut();
            let new_pos = bookmark.position.min(state.buffer.len());

            let event = Event::MoveCursor {
                cursor_id,
                old_position: cursor.position,
                new_position: new_pos,
                old_anchor: cursor.anchor,
                new_anchor: None,
                old_sticky_column: cursor.sticky_column,
                new_sticky_column: 0,
            };

            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);
            self.set_status_message(t!("bookmark.jumped", key = key).to_string());
        } else {
            self.set_status_message(t!("bookmark.not_set", key = key).to_string());
        }
    }

    /// Clear a bookmark
    pub(super) fn clear_bookmark(&mut self, key: char) {
        if self.bookmarks.remove(&key).is_some() {
            self.set_status_message(t!("bookmark.cleared", key = key).to_string());
        } else {
            self.set_status_message(t!("bookmark.not_set", key = key).to_string());
        }
    }

    /// List all bookmarks
    pub(super) fn list_bookmarks(&mut self) {
        if self.bookmarks.is_empty() {
            self.set_status_message(t!("bookmark.none_set").to_string());
            return;
        }

        let mut bookmark_list: Vec<_> = self.bookmarks.iter().collect();
        bookmark_list.sort_by_key(|(k, _)| *k);

        let list_str: String = bookmark_list
            .iter()
            .map(|(k, bm)| {
                let buffer_name = self
                    .buffer_metadata
                    .get(&bm.buffer_id)
                    .map(|m| m.display_name.as_str())
                    .unwrap_or("unknown");
                format!("'{}': {} @ {}", k, buffer_name, bm.position)
            })
            .collect::<Vec<_>>()
            .join(", ");

        self.set_status_message(t!("bookmark.list", list = list_str).to_string());
    }

    /// Clear the search history
    /// Used primarily for testing to ensure test isolation
    pub fn clear_search_history(&mut self) {
        if let Some(history) = self.prompt_histories.get_mut("search") {
            history.clear();
        }
    }

    /// Save all prompt histories to disk
    /// Called on shutdown to persist history across sessions
    pub fn save_histories(&self) {
        // Ensure data directory exists
        if let Err(e) = self.filesystem.create_dir_all(&self.dir_context.data_dir) {
            tracing::warn!("Failed to create data directory: {}", e);
            return;
        }

        // Save all prompt histories
        for (key, history) in &self.prompt_histories {
            let path = self.dir_context.prompt_history_path(key);
            if let Err(e) = history.save_to_file(&path) {
                tracing::warn!("Failed to save {} history: {}", key, e);
            } else {
                tracing::debug!("Saved {} history to {:?}", key, path);
            }
        }
    }

    /// Ensure the active tab in a split is visible by adjusting its scroll offset.
    /// This function recalculates the required scroll_offset based on the active tab's position
    /// and the available width, and updates the SplitViewState.
    pub(super) fn ensure_active_tab_visible(
        &mut self,
        split_id: SplitId,
        active_buffer: BufferId,
        available_width: u16,
    ) {
        tracing::debug!(
            "ensure_active_tab_visible called: split={:?}, buffer={:?}, width={}",
            split_id,
            active_buffer,
            available_width
        );
        let Some(view_state) = self.split_view_states.get_mut(&split_id) else {
            tracing::debug!("  -> no view_state for split");
            return;
        };

        let split_buffers = view_state.open_buffers.clone();

        // Use the shared function to calculate tab widths (same as render_for_split)
        let (tab_widths, rendered_buffer_ids) = crate::view::ui::tabs::calculate_tab_widths(
            &split_buffers,
            &self.buffers,
            &self.buffer_metadata,
            &self.composite_buffers,
        );

        let total_tabs_width: usize = tab_widths.iter().sum();
        let max_visible_width = available_width as usize;

        // Find the active tab index among rendered buffers
        // Note: tab_widths includes separators, so we need to map buffer index to width index
        let active_tab_index = rendered_buffer_ids
            .iter()
            .position(|id| *id == active_buffer);

        // Map buffer index to width index (accounting for separators)
        // Widths are: [sep?, tab0, sep, tab1, sep, tab2, ...]
        // First tab has no separator before it, subsequent tabs have separator before
        let active_width_index = active_tab_index.map(|buf_idx| {
            if buf_idx == 0 {
                0
            } else {
                // Each tab after the first has a separator before it
                // So tab N is at position 2*N (sep before tab1 is at 1, tab1 at 2, sep before tab2 at 3, tab2 at 4, etc.)
                // Wait, the structure is: [tab0, sep, tab1, sep, tab2]
                // So tab N (0-indexed) is at position 2*N
                buf_idx * 2
            }
        });

        // Calculate offset to bring active tab into view
        let old_offset = view_state.tab_scroll_offset;
        let new_scroll_offset = if let Some(idx) = active_width_index {
            crate::view::ui::tabs::scroll_to_show_tab(
                &tab_widths,
                idx,
                view_state.tab_scroll_offset,
                max_visible_width,
            )
        } else {
            view_state
                .tab_scroll_offset
                .min(total_tabs_width.saturating_sub(max_visible_width))
        };

        tracing::debug!(
            "  -> offset: {} -> {} (idx={:?}, max_width={}, total={})",
            old_offset,
            new_scroll_offset,
            active_width_index,
            max_visible_width,
            total_tabs_width
        );
        view_state.tab_scroll_offset = new_scroll_offset;
    }

    /// Synchronize viewports for all scroll sync groups
    ///
    /// This syncs the inactive split's viewport to match the active split's position.
    /// By deriving from the active split's actual viewport, we capture all viewport
    /// changes regardless of source (scroll events, cursor movements, etc.).
    fn sync_scroll_groups(&mut self) {
        let active_split = self.split_manager.active_split();
        let group_count = self.scroll_sync_manager.groups().len();

        if group_count > 0 {
            tracing::debug!(
                "sync_scroll_groups: active_split={:?}, {} groups",
                active_split,
                group_count
            );
        }

        // Collect sync info: for each group where active split participates,
        // get the active split's current line position
        let sync_info: Vec<_> = self
            .scroll_sync_manager
            .groups()
            .iter()
            .filter_map(|group| {
                tracing::debug!(
                    "sync_scroll_groups: checking group {}, left={:?}, right={:?}",
                    group.id,
                    group.left_split,
                    group.right_split
                );

                if !group.contains_split(active_split) {
                    tracing::debug!(
                        "sync_scroll_groups: active split {:?} not in group",
                        active_split
                    );
                    return None;
                }

                // Get active split's current viewport top_byte
                let active_top_byte = self
                    .split_view_states
                    .get(&active_split)?
                    .viewport
                    .top_byte;

                // Get active split's buffer to convert bytes → line
                let active_buffer_id = self.split_manager.buffer_for_split(active_split)?;
                let buffer_state = self.buffers.get(&active_buffer_id)?;
                let buffer_len = buffer_state.buffer.len();
                let active_line = buffer_state.buffer.get_line_number(active_top_byte);

                tracing::debug!(
                    "sync_scroll_groups: active_split={:?}, buffer_id={:?}, top_byte={}, buffer_len={}, active_line={}",
                    active_split,
                    active_buffer_id,
                    active_top_byte,
                    buffer_len,
                    active_line
                );

                // Determine the other split and compute its target line
                let (other_split, other_line) = if group.is_left_split(active_split) {
                    // Active is left, sync right
                    (group.right_split, group.left_to_right_line(active_line))
                } else {
                    // Active is right, sync left
                    (group.left_split, group.right_to_left_line(active_line))
                };

                tracing::debug!(
                    "sync_scroll_groups: syncing other_split={:?} to line {}",
                    other_split,
                    other_line
                );

                Some((other_split, other_line))
            })
            .collect();

        // Apply sync to other splits
        for (other_split, target_line) in sync_info {
            if let Some(buffer_id) = self.split_manager.buffer_for_split(other_split) {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    let buffer = &mut state.buffer;
                    if let Some(view_state) = self.split_view_states.get_mut(&other_split) {
                        view_state.viewport.scroll_to(buffer, target_line);
                    }
                }
            }
        }

        // Same-buffer scroll sync: when two splits show the same buffer (e.g., source
        // vs compose mode), sync the inactive split's viewport to match the active
        // split's line position. This is a simpler case than anchor-based sync since
        // both splits reference the same buffer (identity line mapping).
        let active_buffer_id = self.split_manager.buffer_for_split(active_split);
        if let Some(active_buf_id) = active_buffer_id {
            let active_top_byte = self
                .split_view_states
                .get(&active_split)
                .map(|vs| vs.viewport.top_byte);

            if let Some(top_byte) = active_top_byte {
                if let Some(buffer_state) = self.buffers.get(&active_buf_id) {
                    let active_line = buffer_state.buffer.get_line_number(top_byte);

                    // Find other splits showing the same buffer (not in an explicit sync group)
                    let other_splits: Vec<_> = self
                        .split_view_states
                        .keys()
                        .filter(|&&s| {
                            s != active_split
                                && self.split_manager.buffer_for_split(s) == Some(active_buf_id)
                                && !self.scroll_sync_manager.is_split_synced(s)
                        })
                        .copied()
                        .collect();

                    for other_split in other_splits {
                        if let Some(state) = self.buffers.get_mut(&active_buf_id) {
                            let buffer = &mut state.buffer;
                            if let Some(view_state) = self.split_view_states.get_mut(&other_split) {
                                view_state.viewport.scroll_to(buffer, active_line);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Pre-sync ensure_visible for scroll sync groups
    ///
    /// When the active split is in a scroll sync group, we need to update its viewport
    /// BEFORE sync_scroll_groups runs. This ensures cursor movements like 'G' (go to end)
    /// properly sync to the other split.
    ///
    /// After updating the active split's viewport, we mark the OTHER splits in the group
    /// to skip ensure_visible so the sync position isn't undone during rendering.
    fn pre_sync_ensure_visible(&mut self, active_split: SplitId) {
        // Check if active split is in any scroll sync group
        let group_info = self
            .scroll_sync_manager
            .find_group_for_split(active_split)
            .map(|g| (g.left_split, g.right_split));

        if let Some((left_split, right_split)) = group_info {
            // Get the active split's buffer and update its viewport
            if let Some(buffer_id) = self.split_manager.buffer_for_split(active_split) {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    let buffer = &mut state.buffer;

                    if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                        let cursor = *view_state.cursors.primary();
                        // Update viewport to show cursor - this is what ensure_visible does
                        view_state.viewport.ensure_visible(buffer, &cursor);

                        tracing::debug!(
                            "pre_sync_ensure_visible: updated active split {:?} viewport, top_byte={}",
                            active_split,
                            view_state.viewport.top_byte
                        );
                    }
                }
            }

            // Mark the OTHER split to skip ensure_visible so the sync position isn't undone
            let other_split = if active_split == left_split {
                right_split
            } else {
                left_split
            };

            if let Some(view_state) = self.split_view_states.get_mut(&other_split) {
                view_state.viewport.set_skip_ensure_visible();
                tracing::debug!(
                    "pre_sync_ensure_visible: marked other split {:?} to skip ensure_visible",
                    other_split
                );
            }
        }

        // Same-buffer scroll sync: also mark other splits showing the same buffer
        // to skip ensure_visible, so our sync_scroll_groups position isn't undone.
        if let Some(active_buf_id) = self.split_manager.buffer_for_split(active_split) {
            let other_same_buffer_splits: Vec<_> = self
                .split_view_states
                .keys()
                .filter(|&&s| {
                    s != active_split
                        && self.split_manager.buffer_for_split(s) == Some(active_buf_id)
                        && !self.scroll_sync_manager.is_split_synced(s)
                })
                .copied()
                .collect();

            for other_split in other_same_buffer_splits {
                if let Some(view_state) = self.split_view_states.get_mut(&other_split) {
                    view_state.viewport.set_skip_ensure_visible();
                }
            }
        }
    }
}
