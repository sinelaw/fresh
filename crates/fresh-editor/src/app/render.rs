use super::lsp_status::compose_lsp_status;
use super::*;
use rust_i18n::t;

impl Editor {
    /// Render the editor to the terminal
    pub fn render(&mut self, frame: &mut Frame) {
        let _span = tracing::info_span!("render").entered();
        let size = frame.area();

        // Save frame dimensions for recompute_layout (used by macro replay)
        self.cached_layout.last_frame_width = size.width;
        self.cached_layout.last_frame_height = size.height;

        // Reset per-cell theme key map for this frame
        self.cached_layout.reset_cell_theme_map();

        // For scroll sync groups, we need to update the active split's viewport position BEFORE
        // calling sync_scroll_groups, so that the sync reads the correct position.
        // Otherwise, cursor movements like 'G' (go to end) won't sync properly because
        // viewport.top_byte hasn't been updated yet.
        let active_split = self.split_manager.active_split();
        {
            let _span = tracing::info_span!("pre_sync_ensure_visible").entered();
            self.pre_sync_ensure_visible(active_split);
        }

        // Synchronize scroll sync groups (anchor-based scroll for side-by-side diffs)
        // This sets viewport positions based on the authoritative scroll_line in each group
        {
            let _span = tracing::info_span!("sync_scroll_groups").entered();
            self.sync_scroll_groups();
        }

        // NOTE: Viewport sync with cursor is handled by split_rendering.rs which knows the
        // correct content area dimensions. Don't sync here with incorrect EditorState viewport size.

        // Prepare all buffers for rendering (pre-load viewport data for lazy loading)
        // Each split may have a different viewport position on the same buffer
        let mut semantic_ranges: std::collections::HashMap<BufferId, (usize, usize)> =
            std::collections::HashMap::new();
        {
            let _span = tracing::info_span!("compute_semantic_ranges").entered();
            for (split_id, view_state) in &self.split_view_states {
                if let Some(buffer_id) = self.split_manager.get_buffer_id((*split_id).into()) {
                    if let Some(state) = self.buffers.get(&buffer_id) {
                        let start_line = state.buffer.get_line_number(view_state.viewport.top_byte);
                        let visible_lines =
                            view_state.viewport.visible_line_count().saturating_sub(1);
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
        }
        for (buffer_id, (start_line, end_line)) in semantic_ranges {
            self.maybe_request_semantic_tokens_range(buffer_id, start_line, end_line);
            self.maybe_request_semantic_tokens_full_debounced(buffer_id);
            self.maybe_request_folding_ranges_debounced(buffer_id);
        }

        {
            let _span = tracing::info_span!("prepare_for_render").entered();
            for (split_id, view_state) in &self.split_view_states {
                if let Some(buffer_id) = self.split_manager.get_buffer_id((*split_id).into()) {
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
            Constraint::Length(
                if !self.status_bar_visible || has_suggestions || has_file_browser {
                    0
                } else {
                    1
                },
            ), // Status bar (hidden when toggled off or with popups)
            Constraint::Length(if show_search_options { 1 } else { 0 }),   // Search options bar
            Constraint::Length(if self.prompt_line_visible || self.prompt.is_some() {
                1
            } else {
                0
            }), // Prompt line (auto-hidden when no prompt active)
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
            let remote_connection = self.remote_connection_info().map(|conn| {
                if self.filesystem.is_remote_connected() {
                    conn.to_string()
                } else {
                    format!("{} (Disconnected)", conn)
                }
            });

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
                let keybindings = self.keybindings.read().unwrap();
                FileExplorerRenderer::render(
                    explorer,
                    frame,
                    horizontal_chunks[0],
                    is_focused,
                    &files_with_unsaved_changes,
                    &self.file_explorer_decoration_cache,
                    &keybindings,
                    self.key_context.clone(),
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
                            split_id: split_id.into(),
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
                tracing::trace!(count = commands.len(), cmds = ?cmd_names, "process_commands during render");
            }
            for command in commands {
                if let Err(e) = self.handle_plugin_command(command) {
                    tracing::error!("Error handling plugin command: {}", e);
                }
            }

            // Flush any deferred grammar rebuilds as a single batch
            self.flush_pending_grammars();
        }

        // Render editor content (same for both layouts)
        let lsp_waiting = !self.pending_completion_requests.is_empty()
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
            Some(HoverTarget::TabName(target, split_id)) => Some((*target, *split_id, false)),
            Some(HoverTarget::TabCloseButton(target, split_id)) => Some((*target, *split_id, true)),
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

        let _content_span = tracing::info_span!("render_content").entered();
        let (
            split_areas,
            tab_layouts,
            close_split_areas,
            maximize_split_areas,
            view_line_mappings,
            horizontal_scrollbar_areas,
            grouped_separator_areas,
        ) = SplitRenderer::render_content(
            frame,
            editor_content_area,
            &self.split_manager,
            &mut self.buffers,
            &self.buffer_metadata,
            &mut self.event_logs,
            &mut self.composite_buffers,
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
            &self.grouped_subtrees,
            hide_cursor,
            hovered_tab,
            hovered_close_split,
            hovered_maximize_split,
            is_maximized,
            self.config.editor.relative_line_numbers,
            self.tab_bar_visible,
            self.config.editor.use_terminal_bg,
            self.session_mode || !self.software_cursor_only,
            self.software_cursor_only,
            self.config.editor.show_vertical_scrollbar,
            self.config.editor.show_horizontal_scrollbar,
            self.config.editor.diagnostics_inline_text,
            self.config.editor.show_tilde,
            &mut self.cached_layout.cell_theme_map,
            size.width,
        );

        drop(_content_span);

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
                    if let Some(buffer_id) = self.split_manager.get_buffer_id((*split_id).into()) {
                        // Compute top_line if line info is available
                        let top_line = self.buffers.get(&buffer_id).and_then(|state| {
                            if state.buffer.line_count().is_some() {
                                Some(state.buffer.get_line_number(view_state.viewport.top_byte))
                            } else {
                                None
                            }
                        });
                        tracing::debug!(
                            "Firing viewport_changed hook: split={:?} buffer={:?} top_byte={} top_line={:?}",
                            split_id,
                            buffer_id,
                            view_state.viewport.top_byte,
                            top_line
                        );
                        self.plugin_manager.run_hook(
                            "viewport_changed",
                            crate::services::plugins::hooks::HookArgs::ViewportChanged {
                                split_id: (*split_id).into(),
                                buffer_id,
                                top_byte: view_state.viewport.top_byte,
                                top_line,
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
        let mut separator_areas = self
            .split_manager
            .get_separators_with_ids(editor_content_area);
        // Grouped subtrees live in a side-map outside the main split tree, so
        // their inner separators are not visited by `get_separators_with_ids`
        // above. The renderer collected them (using the same content rect it
        // drew them at) — merge so clicks on those rendered columns register.
        separator_areas.extend(grouped_separator_areas);
        self.cached_layout.separator_areas = separator_areas;
        self.cached_layout.editor_content_area = Some(editor_content_area);

        // Render hover highlights for separators and scrollbars
        self.render_hover_highlights(frame);

        // Initialize popup/suggestion layout state (rendered after status bar below)
        self.cached_layout.suggestions_area = None;
        self.file_browser_layout = None;

        // Clone all immutable values before the mutable borrow
        let display_name = self
            .buffer_metadata
            .get(&self.active_buffer())
            .map(|m| m.display_name.clone())
            .unwrap_or_else(|| "[No Name]".to_string());
        let status_message = self.status_message.clone();
        let plugin_status_message = self.plugin_status_message.clone();
        let prompt = self.prompt.clone();
        // Compute a simple buffer-aware LSP indicator.
        // Compose the LSP status-bar segment for the active buffer. This
        // runs every render — the editor has no precomputed LSP-status
        // string cached anywhere else, so there is a single source of
        // truth for what the user sees.
        //
        // Priority order (first non-empty wins):
        //
        //   1. Active `$/progress` work for this language — e.g.
        //      "LSP (cpp): indexing (42%)". Conveys the transient
        //      startup/indexing phase.
        //   2. A running server — "LSP". Short because detail belongs
        //      in LSP-specific UI, not the compact status bar pill.
        //   3. Configured `auto_start=true` servers that haven't started
        //      (error / crashed / pending) — "LSP off".
        //   4. Configured `enabled && !auto_start` servers that the user
        //      has to opt into — "LSP: off (N)".
        //   5. Nothing.
        //
        // Rules 3 and 4 address heuristic eval H-1: without them, a
        // configured-but-dormant server is indistinguishable from "no
        // LSP at all."
        let current_language = self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
            .unwrap_or_default();
        let (lsp_status, lsp_indicator_state) = compose_lsp_status(
            &current_language,
            &self.lsp_progress,
            &self.lsp_server_statuses,
            &self.config.lsp,
            &self.user_dismissed_lsp_languages,
        );
        let theme = self.theme.clone();
        let keybindings_cloned = self.keybindings.read().unwrap().clone(); // Clone the keybindings
        let chord_state_cloned = self.chord_state.clone(); // Clone the chord state

        // Get update availability info
        let update_available = self.latest_version().map(|v| v.to_string());

        // Render status bar (hidden when toggled off, or when suggestions/file browser popup is shown)
        if self.status_bar_visible && !has_suggestions && !has_file_browser {
            // Get warning level for colored indicator (respects config setting)
            // LSP warning level is scoped to the current buffer's language
            let (warning_level, general_warning_count) =
                if self.config.warnings.show_status_indicator {
                    let lsp_level = {
                        use crate::services::async_bridge::LspServerStatus;
                        let mut level = WarningLevel::None;
                        for ((lang, _), status) in &self.lsp_server_statuses {
                            if lang == &current_language {
                                match status {
                                    LspServerStatus::Error => {
                                        level = WarningLevel::Error;
                                        break;
                                    }
                                    LspServerStatus::Starting | LspServerStatus::Initializing => {
                                        if level != WarningLevel::Error {
                                            level = WarningLevel::Warning;
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                        level
                    };
                    (lsp_level, self.get_general_warning_count())
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
            let remote_connection = self.remote_connection_info().map(|conn| {
                if self.filesystem.is_remote_connected() {
                    conn.to_string()
                } else {
                    format!("{} (Disconnected)", conn)
                }
            });

            // Get session name for display (only in session mode)
            let session_name = self.session_name().map(|s| s.to_string());

            let active_split = self.effective_active_split();
            let active_buf = self.active_buffer();
            let default_cursors = crate::model::cursor::Cursors::new();
            let status_cursors = self
                .split_view_states
                .get(&active_split)
                .map(|vs| &vs.cursors)
                .unwrap_or(&default_cursors);
            let is_read_only = self
                .buffer_metadata
                .get(&active_buf)
                .map(|m| m.read_only)
                .unwrap_or(false);
            let mut status_ctx = crate::view::ui::status_bar::StatusBarContext {
                state: self.buffers.get_mut(&active_buf).unwrap(),
                cursors: status_cursors,
                status_message: &status_message,
                plugin_status_message: &plugin_status_message,
                lsp_status: &lsp_status,
                lsp_indicator_state,
                theme: &theme,
                display_name: &display_name,
                keybindings: &keybindings_cloned,
                chord_state: &chord_state_cloned,
                update_available: update_available.as_deref(),
                warning_level,
                general_warning_count,
                hover: status_bar_hover,
                remote_connection: remote_connection.as_deref(),
                session_name: session_name.as_deref(),
                read_only: is_read_only,
            };
            let status_bar_layout = StatusBarRenderer::render_status_bar(
                frame,
                main_chunks[status_bar_idx],
                &mut status_ctx,
                &self.config.editor.status_bar,
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

        // Render file browser popup or suggestions popup AFTER status bar + prompt,
        // so they overlay on top of both (fixes bottom border being overwritten by status bar)
        self.render_prompt_popups(frame, main_chunks[prompt_line_idx], size.width);

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

            // Get the content_rect for the active split from the cached layout.
            // This is the absolute screen rect (already accounts for file explorer,
            // tab bar, scrollbars, etc.). The gutter is rendered inside this rect,
            // so we add gutter_width to get the text content origin.
            let content_rect = self
                .cached_layout
                .split_areas
                .iter()
                .find(|(split_id, _, _, _, _, _)| *split_id == active_split)
                .map(|(_, _, rect, _, _, _)| *rect);

            let primary_cursor = self
                .split_view_states
                .get(&active_split)
                .map(|vs| *vs.cursors.primary());
            let state = self.active_state_mut();
            if state.popups.is_visible() {
                // Get the primary cursor position for popup positioning
                let primary_cursor =
                    primary_cursor.unwrap_or_else(|| crate::model::cursor::Cursor::new(0));

                // Compute gutter width so we know where text content starts
                let gutter_width = viewport
                    .as_ref()
                    .map(|vp| vp.gutter_width(&state.buffer) as u16)
                    .unwrap_or(0);

                let cursor_screen_pos = viewport
                    .as_ref()
                    .map(|vp| vp.cursor_screen_position(&mut state.buffer, &primary_cursor))
                    .unwrap_or((0, 0));

                // For completion popups, compute the word-start screen position so
                // the popup aligns with the beginning of the word being completed,
                // not the current cursor position.
                let word_start_screen_pos = {
                    use crate::primitives::word_navigation::find_completion_word_start;
                    let word_start =
                        find_completion_word_start(&state.buffer, primary_cursor.position);
                    let word_start_cursor = crate::model::cursor::Cursor::new(word_start);
                    viewport
                        .as_ref()
                        .map(|vp| vp.cursor_screen_position(&mut state.buffer, &word_start_cursor))
                        .unwrap_or((0, 0))
                };

                // Use content_rect as the single source of truth for the text
                // content area origin. content_rect.x is the split's left edge
                // (already past the file explorer), content_rect.y is below the
                // tab bar. Adding gutter_width gives us the text content start.
                let (base_x, base_y) = content_rect
                    .map(|r| (r.x + gutter_width, r.y))
                    .unwrap_or((gutter_width, 1));

                let cursor_screen_pos =
                    (cursor_screen_pos.0 + base_x, cursor_screen_pos.1 + base_y);
                let word_start_screen_pos = (
                    word_start_screen_pos.0 + base_x,
                    word_start_screen_pos.1 + base_y,
                );

                // Collect popup data
                state
                    .popups
                    .all()
                    .iter()
                    .enumerate()
                    .map(|(popup_idx, popup)| {
                        // Use word-start x for completion popups, cursor x for others
                        let popup_pos = if popup.kind == crate::view::popup::PopupKind::Completion {
                            (word_start_screen_pos.0, cursor_screen_pos.1)
                        } else {
                            cursor_screen_pos
                        };
                        let popup_area = popup.calculate_area(size, Some(popup_pos));

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
            let keybindings = self.keybindings.read().unwrap();
            self.cached_layout.menu_layout = Some(crate::view::ui::MenuRenderer::render(
                frame,
                menu_bar_area,
                &self.menus,
                &self.menu_state,
                &keybindings,
                &self.theme,
                self.mouse_state.hover_target.as_ref(),
                self.config.editor.menu_bar_mnemonics,
            ));
        } else {
            self.cached_layout.menu_layout = None;
        }

        // Render tab context menu if open
        if let Some(ref menu) = self.tab_context_menu {
            self.render_tab_context_menu(frame, menu);
        }

        // Record non-editor region theme keys for the theme inspector
        self.record_non_editor_theme_regions();

        // Render theme info popup (Ctrl+Right-Click)
        self.render_theme_info_popup(frame);

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

    /// Render file browser or suggestions popup as overlay above the prompt line.
    /// Called after status bar + prompt so the popup draws on top of both.
    fn render_prompt_popups(
        &mut self,
        frame: &mut Frame,
        prompt_area: ratatui::layout::Rect,
        width: u16,
    ) {
        let Some(prompt) = &self.prompt else { return };

        if matches!(
            prompt.prompt_type,
            PromptType::OpenFile | PromptType::SwitchProject | PromptType::SaveFileAs
        ) {
            let Some(file_open_state) = &self.file_open_state else {
                return;
            };
            let max_height = prompt_area.y.saturating_sub(1).min(20);
            let popup_area = ratatui::layout::Rect {
                x: 0,
                y: prompt_area.y.saturating_sub(max_height),
                width,
                height: max_height,
            };
            let keybindings = self.keybindings.read().unwrap();
            self.file_browser_layout = crate::view::ui::FileBrowserRenderer::render(
                frame,
                popup_area,
                file_open_state,
                &self.theme,
                &self.mouse_state.hover_target,
                Some(&*keybindings),
            );
            return;
        }

        if prompt.suggestions.is_empty() {
            return;
        }

        let suggestion_count = prompt.suggestions.len().min(10);
        let is_quick_open = prompt.prompt_type == crate::view::prompt::PromptType::QuickOpen;
        let hints_height: u16 = if is_quick_open { 1 } else { 0 };
        let height = suggestion_count as u16 + 2 + hints_height;

        let suggestions_area = ratatui::layout::Rect {
            x: 0,
            y: prompt_area.y.saturating_sub(height),
            width,
            height: height - hints_height,
        };

        frame.render_widget(ratatui::widgets::Clear, suggestions_area);

        self.cached_layout.suggestions_area = SuggestionsRenderer::render_with_hover(
            frame,
            suggestions_area,
            prompt,
            &self.theme,
            self.mouse_state.hover_target.as_ref(),
        );

        if is_quick_open {
            let hints_area = ratatui::layout::Rect {
                x: 0,
                y: prompt_area.y.saturating_sub(hints_height),
                width,
                height: hints_height,
            };
            frame.render_widget(ratatui::widgets::Clear, hints_area);
            Self::render_quick_open_hints(frame, hints_area, &self.theme);
        }
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
            Some(HoverTarget::ScrollbarTrack(split_id, hovered_row)) => {
                // Highlight only the hovered cell on the scrollbar track
                for (sid, _buffer_id, _content_rect, scrollbar_rect, _thumb_start, _thumb_end) in
                    &self.cached_layout.split_areas
                {
                    if sid == split_id {
                        let track_hover_style =
                            Style::default().bg(self.theme.scrollbar_track_hover_fg);
                        let paragraph = Paragraph::new(Span::styled(" ", track_hover_style));
                        frame.render_widget(
                            paragraph,
                            ratatui::layout::Rect::new(
                                scrollbar_rect.x,
                                scrollbar_rect.y + hovered_row,
                                1,
                                1,
                            ),
                        );
                    }
                }
            }
            Some(HoverTarget::FileExplorerBorder) => {
                // Highlight the file explorer border for resize
                if let Some(explorer_area) = self.cached_layout.file_explorer_area {
                    let hover_style = Style::default().fg(self.theme.split_separator_hover_fg);
                    let border_x = explorer_area.x + explorer_area.width.saturating_sub(1);
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

        // Complete --wait tracking if this buffer had a popup-based wait
        let active = self.active_buffer();
        if let Some((wait_id, true)) = self.wait_tracking.remove(&active) {
            self.completed_waits.push(wait_id);
        }

        // Clear hover symbol highlight if present
        if let Some(handle) = self.hover.take_symbol_overlay() {
            let remove_overlay_event = crate::model::event::Event::RemoveOverlay { handle };
            self.apply_event_to_active_buffer(&remove_overlay_event);
        }
        self.hover.set_symbol_range(None);
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
        self.hover.clear_pending();

        // Clear hover symbol highlight if present
        if let Some(handle) = self.hover.take_symbol_overlay() {
            let remove_overlay_event = crate::model::event::Event::RemoveOverlay { handle };
            self.apply_event_to_active_buffer(&remove_overlay_event);
        }
        self.hover.set_symbol_range(None);
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

        // Get file path from active buffer for workspace root detection
        let file_path = self
            .buffer_metadata
            .get(&self.active_buffer())
            .and_then(|meta| meta.file_path().cloned());

        match action {
            "allow_once" => {
                // Spawn the LSP server just this once (don't add to always-allowed)
                if let Some(lsp) = &mut self.lsp {
                    // Temporarily allow this language for spawning
                    lsp.allow_language(&language);
                    // Use force_spawn since user explicitly confirmed
                    if lsp.force_spawn(&language, file_path.as_deref()).is_some() {
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
                    if lsp.force_spawn(&language, file_path.as_deref()).is_some() {
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

        // Get file path for LSP spawn
        let file_path = metadata.file_path().cloned();

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
        let (text, line_count, buffer_version) =
            if let Some(state) = self.buffers.get(&active_buffer) {
                let text = match state.buffer.to_string() {
                    Some(t) => t,
                    None => {
                        tracing::debug!("notify_lsp_current_file_opened: buffer not fully loaded");
                        return;
                    }
                };
                let line_count = state.buffer.line_count().unwrap_or(1000);
                (text, line_count, state.buffer.version())
            } else {
                tracing::debug!("notify_lsp_current_file_opened: no buffer state");
                return;
            };

        // Send didOpen to all LSP handles (use force_spawn to ensure they're started)
        if let Some(lsp) = &mut self.lsp {
            // force_spawn starts all servers for this language
            if lsp.force_spawn(language, file_path.as_deref()).is_some() {
                tracing::info!("Sending didOpen to LSP servers for: {}", uri.as_str());
                let mut any_opened = false;
                for sh in lsp.get_handles_mut(language) {
                    if let Err(e) =
                        sh.handle
                            .did_open(uri.clone(), text.clone(), file_language.clone())
                    {
                        tracing::warn!("Failed to send didOpen to '{}': {}", sh.name, e);
                    } else {
                        any_opened = true;
                    }
                }

                if any_opened {
                    tracing::info!("Successfully sent didOpen to LSP after confirmation");

                    // Request pull diagnostics from primary handle
                    if let Some(handle) = lsp.get_handle_mut(language) {
                        let previous_result_id =
                            self.diagnostic_result_ids.get(uri.as_str()).cloned();
                        let request_id = self.next_lsp_request_id;
                        self.next_lsp_request_id += 1;

                        if let Err(e) =
                            handle.document_diagnostic(request_id, uri.clone(), previous_result_id)
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

                            let last_line = line_count.saturating_sub(1) as u32;
                            let last_char = 10000u32;

                            if let Err(e) = handle.inlay_hints(
                                request_id,
                                uri.clone(),
                                0,
                                0,
                                last_line,
                                last_char,
                            ) {
                                tracing::debug!(
                                    "Failed to request inlay hints (server may not support): {}",
                                    e
                                );
                            } else {
                                self.pending_inlay_hints_requests.insert(
                                    request_id,
                                    super::InlayHintsRequest {
                                        buffer_id: active_buffer,
                                        version: buffer_version,
                                    },
                                );
                            }
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
        let buffer_id = self.active_buffer();
        self.notify_lsp_save_buffer(buffer_id);
    }

    /// Notify LSP of a file save for a specific buffer
    pub(super) fn notify_lsp_save_buffer(&mut self, buffer_id: BufferId) {
        // Check if LSP is enabled for this buffer
        let metadata = match self.buffer_metadata.get(&buffer_id) {
            Some(m) => m,
            None => {
                tracing::debug!(
                    "notify_lsp_save_buffer: no metadata for buffer {:?}",
                    buffer_id
                );
                return;
            }
        };

        if !metadata.lsp_enabled {
            tracing::debug!(
                "notify_lsp_save_buffer: LSP disabled for buffer {:?}",
                buffer_id
            );
            return;
        }

        // Get file path for LSP spawn
        let file_path = metadata.file_path().cloned();

        // Get the URI
        let uri = match metadata.file_uri() {
            Some(u) => u.clone(),
            None => {
                tracing::debug!("notify_lsp_save_buffer: no URI for buffer {:?}", buffer_id);
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
            if lsp.try_spawn(&language, file_path.as_deref()) != LspSpawnResult::Spawned {
                tracing::debug!(
                    "notify_lsp_save: LSP not running for {} (auto_start disabled)",
                    language
                );
                return;
            }
            // Broadcast didSave to all handles for this language
            let mut any_sent = false;
            for sh in lsp.get_handles_mut(&language) {
                if let Err(e) = sh.handle.did_save(uri.clone(), Some(full_text.clone())) {
                    tracing::warn!("Failed to send didSave to '{}': {}", sh.name, e);
                } else {
                    any_sent = true;
                }
            }
            if any_sent {
                tracing::info!("Successfully sent didSave to LSP");
            } else {
                tracing::warn!("notify_lsp_save: no LSP handles for {}", language);
            }
        } else {
            tracing::debug!("notify_lsp_save: no LSP manager available");
        }
    }

    /// Convert an action into a list of events to apply to the active buffer
    /// Returns None for actions that don't generate events (like Quit)
    pub fn action_to_events(&mut self, action: Action) -> Option<Vec<Event>> {
        let auto_indent = self.config.editor.auto_indent;
        let estimated_line_length = self.config.editor.estimated_line_length;

        // Use the *effective* active split: when the user is focused on an
        // inner panel of a grouped buffer (e.g. a magit-style review panel),
        // its leaf id lives in `split_view_states` but is not in the main
        // split tree. `effective_active_split` returns that inner leaf, so
        // motion targets the panel's own buffer/cursors instead of the
        // group host's.
        let active_split = self.effective_active_split();
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

        // Use per-buffer settings which respect language overrides and user changes
        let tab_size = state.buffer_settings.tab_size;
        let auto_close = state.buffer_settings.auto_close;
        let auto_surround = state.buffer_settings.auto_surround;

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
            auto_close,
            auto_surround,
            estimated_line_length,
            viewport_height,
        )
    }

    /// Handle visual line movement actions using the cached layout
    /// Returns Some(events) if the action was handled, None if it should fall through
    fn handle_visual_line_movement(
        &mut self,
        action: &Action,
        split_id: LeafId,
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

        // First, collect cursor data we need (to avoid borrow conflicts).
        // Use the *effective* active split + buffer so that cursor motion in
        // a focused buffer-group panel reads the panel's own cursors and
        // buffer instead of the group host's.
        let cursor_data: Vec<_> = {
            let active_split = self.effective_active_split();
            let active_buffer = self.active_buffer();
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
                VisualAction::UpDown {
                    direction,
                    is_select,
                } => {
                    // When a selection is active, plain (non-selecting) vertical
                    // motion starts from the selection's edge closest to the
                    // motion direction (top edge for Up, bottom edge for Down),
                    // matching VSCode/Sublime/browser behavior (issue #1566).
                    // Emacs mark-mode (`deselect_on_move == false`) is unaffected.
                    let from_pos = if deselect_on_move && !*is_select {
                        if let Some(anchor) = anchor {
                            if *direction < 0 {
                                position.min(anchor)
                            } else {
                                position.max(anchor)
                            }
                        } else {
                            position
                        }
                    } else {
                        position
                    };

                    // Calculate current visual column from cached layout
                    let current_visual_col = self
                        .cached_layout
                        .byte_to_visual_column(split_id, from_pos)?;

                    let goal_visual_col = if sticky_column > 0 {
                        sticky_column
                    } else {
                        current_visual_col
                    };

                    match self.cached_layout.move_visual_line(
                        split_id,
                        from_pos,
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


    /// Recompute the view_line_mappings layout without drawing.
    /// Used during macro replay so that visual-line movements (MoveLineEnd,
    /// MoveUp, MoveDown on wrapped lines) see correct, up-to-date layout
    /// information between each replayed action.
    pub fn recompute_layout(&mut self, width: u16, height: u16) {
        let size = ratatui::layout::Rect::new(0, 0, width, height);

        // Replicate the pre-render sync steps from render()
        let active_split = self.split_manager.active_split();
        self.pre_sync_ensure_visible(active_split);
        self.sync_scroll_groups();

        // Replicate the layout computation that produces editor_content_area.
        // Same constraints as render(): [menu_bar, main_content, status_bar, search_options, prompt_line]
        let constraints = vec![
            Constraint::Length(if self.menu_bar_visible { 1 } else { 0 }),
            Constraint::Min(0),
            Constraint::Length(if self.status_bar_visible { 1 } else { 0 }), // status bar
            Constraint::Length(0), // search options (doesn't matter for layout)
            Constraint::Length(if self.prompt_line_visible { 1 } else { 0 }), // prompt line
        ];
        let main_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(constraints)
            .split(size);
        let main_content_area = main_chunks[1];

        // Compute editor_content_area (with file explorer split if visible)
        let file_explorer_should_show = self.file_explorer_visible
            && (self.file_explorer.is_some() || self.file_explorer_sync_in_progress);
        let editor_content_area = if file_explorer_should_show {
            let explorer_percent = (self.file_explorer_width_percent * 100.0) as u16;
            let editor_percent = 100 - explorer_percent;
            let horizontal_chunks = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([
                    Constraint::Percentage(explorer_percent),
                    Constraint::Percentage(editor_percent),
                ])
                .split(main_content_area);
            horizontal_chunks[1]
        } else {
            main_content_area
        };

        // Compute layout for all visible splits and update cached view_line_mappings
        let view_line_mappings = SplitRenderer::compute_content_layout(
            editor_content_area,
            &self.split_manager,
            &mut self.buffers,
            &mut self.split_view_states,
            &self.theme,
            false, // lsp_waiting — not relevant for layout
            self.config.editor.estimated_line_length,
            self.config.editor.highlight_context_bytes,
            self.config.editor.relative_line_numbers,
            self.config.editor.use_terminal_bg,
            self.session_mode || !self.software_cursor_only,
            self.software_cursor_only,
            self.tab_bar_visible,
            self.config.editor.show_vertical_scrollbar,
            self.config.editor.show_horizontal_scrollbar,
            self.config.editor.diagnostics_inline_text,
            self.config.editor.show_tilde,
        );

        self.cached_layout.view_line_mappings = view_line_mappings;
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
        split_id: LeafId,
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
        // Collect group names from the stashed Grouped subtrees.
        let group_names: std::collections::HashMap<LeafId, String> = self
            .grouped_subtrees
            .iter()
            .filter_map(|(leaf_id, node)| {
                if let crate::view::split::SplitNode::Grouped { name, .. } = node {
                    Some((*leaf_id, name.clone()))
                } else {
                    None
                }
            })
            .collect();

        // Use the shared function to calculate tab widths (same as render_for_split)
        let (tab_widths, rendered_targets) = crate::view::ui::tabs::calculate_tab_widths(
            &split_buffers,
            &self.buffers,
            &self.buffer_metadata,
            &self.composite_buffers,
            &group_names,
        );

        let total_tabs_width: usize = tab_widths.iter().sum();
        let max_visible_width = available_width as usize;

        // Determine the active target from the SplitViewState marker.
        let active_target = view_state.active_target();
        // If the caller passed an explicit buffer_id and the split doesn't
        // have a group marked active, use that buffer as the target.
        let active_target = if matches!(active_target, crate::view::split::TabTarget::Buffer(_)) {
            crate::view::split::TabTarget::Buffer(active_buffer)
        } else {
            active_target
        };

        // Find the active tab index among rendered targets
        // Note: tab_widths includes separators, so we need to map tab index to width index
        let active_tab_index = rendered_targets.iter().position(|t| *t == active_target);

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

                if !group.contains_split(active_split.into()) {
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
                let (other_split, other_line) = if group.is_left_split(active_split.into()) {
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
            let other_leaf = LeafId(other_split);
            if let Some(buffer_id) = self.split_manager.buffer_for_split(other_leaf) {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    let buffer = &mut state.buffer;
                    if let Some(view_state) = self.split_view_states.get_mut(&other_leaf) {
                        view_state.viewport.scroll_to(buffer, target_line);
                    }
                }
            }
        }

        // Same-buffer scroll sync: when two splits show the same buffer (e.g., source
        // vs compose mode), sync the inactive split's viewport to match the active
        // split's scroll position.  Gated on the user-togglable scroll sync flag.
        //
        // We copy top_byte directly for the general case.  At the bottom edge the
        // two splits may disagree because compose mode has soft-break virtual lines.
        // Rather than computing the correct position here (where view lines aren't
        // available), we set a flag and let `render_buffer_in_split` fix it up using
        // the same view-line-based logic that `ensure_visible_in_layout` uses.
        let active_buffer_id = if self.same_buffer_scroll_sync {
            self.split_manager.buffer_for_split(active_split)
        } else {
            None
        };
        if let Some(active_buf_id) = active_buffer_id {
            let active_top_byte = self
                .split_view_states
                .get(&active_split)
                .map(|vs| vs.viewport.top_byte);
            let active_viewport_height = self
                .split_view_states
                .get(&active_split)
                .map(|vs| vs.viewport.visible_line_count())
                .unwrap_or(0);

            if let Some(top_byte) = active_top_byte {
                // Find other splits showing the same buffer (not in an explicit sync group)
                let other_splits: Vec<_> = self
                    .split_view_states
                    .keys()
                    .filter(|&&s| {
                        s != active_split
                            && self.split_manager.buffer_for_split(s) == Some(active_buf_id)
                            && !self.scroll_sync_manager.is_split_synced(s.into())
                    })
                    .copied()
                    .collect();

                if !other_splits.is_empty() {
                    // Detect whether the active split is at the bottom of the
                    // buffer (remaining lines fit within the viewport).
                    let at_bottom = if let Some(state) = self.buffers.get_mut(&active_buf_id) {
                        let mut iter = state.buffer.line_iterator(top_byte, 80);
                        let mut lines_remaining = 0;
                        while iter.next_line().is_some() {
                            lines_remaining += 1;
                            if lines_remaining > active_viewport_height {
                                break;
                            }
                        }
                        lines_remaining <= active_viewport_height
                    } else {
                        false
                    };

                    for other_split in other_splits {
                        if let Some(view_state) = self.split_view_states.get_mut(&other_split) {
                            view_state.viewport.top_byte = top_byte;
                            // At the bottom edge, tell the render pass to
                            // adjust using view lines (soft-break-aware).
                            view_state.viewport.sync_scroll_to_end = at_bottom;
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
    fn pre_sync_ensure_visible(&mut self, active_split: LeafId) {
        // Check if active split is in any scroll sync group
        let group_info = self
            .scroll_sync_manager
            .find_group_for_split(active_split.into())
            .map(|g| (g.left_split, g.right_split));

        if let Some((left_split, right_split)) = group_info {
            // Get the active split's buffer and update its viewport
            if let Some(buffer_id) = self.split_manager.buffer_for_split(active_split) {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                        // Update viewport to show cursor
                        view_state.ensure_cursor_visible(&mut state.buffer, &state.marker_list);

                        tracing::debug!(
                            "pre_sync_ensure_visible: updated active split {:?} viewport, top_byte={}",
                            active_split,
                            view_state.viewport.top_byte
                        );
                    }
                }
            }

            // Mark the OTHER split to skip ensure_visible so the sync position isn't undone
            let active_sid: SplitId = active_split.into();
            let other_split: SplitId = if active_sid == left_split {
                right_split
            } else {
                left_split
            };

            if let Some(view_state) = self.split_view_states.get_mut(&LeafId(other_split)) {
                view_state.viewport.set_skip_ensure_visible();
                tracing::debug!(
                    "pre_sync_ensure_visible: marked other split {:?} to skip ensure_visible",
                    other_split
                );
            }
        }

        // Same-buffer scroll sync: also mark other splits showing the same buffer
        // to skip ensure_visible, so our sync_scroll_groups position isn't undone.
        if !self.same_buffer_scroll_sync {
            // Scroll sync disabled — don't interfere with other splits.
        } else if let Some(active_buf_id) = self.split_manager.buffer_for_split(active_split) {
            let other_same_buffer_splits: Vec<_> = self
                .split_view_states
                .keys()
                .filter(|&&s| {
                    s != active_split
                        && self.split_manager.buffer_for_split(s) == Some(active_buf_id)
                        && !self.scroll_sync_manager.is_split_synced(s.into())
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
