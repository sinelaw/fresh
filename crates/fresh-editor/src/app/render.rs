use super::lsp_status::compose_lsp_status;
use super::*;
use crate::config::FileExplorerSide;

impl Editor {
    /// Render the editor to the terminal
    pub fn render(&mut self, frame: &mut Frame) {
        let _span = tracing::info_span!("render").entered();
        let size = frame.area();

        // Let active animations snapshot the previous frame's buffer
        // from the runner's own cache. We can't read the live
        // `frame.buffer_mut()` — ratatui resets it before each draw —
        // so the runner keeps a post-apply clone from the last frame.
        self.animations.capture_before_all();

        // Save frame dimensions for recompute_layout (used by macro replay)
        self.cached_layout.last_frame_width = size.width;
        self.cached_layout.last_frame_height = size.height;

        // Reset per-cell theme key map for this frame
        self.cached_layout.reset_cell_theme_map();

        // Attach any queued LSP auto-start prompt to the currently
        // active buffer. Done here (rather than at file-open) so the
        // popup follows the user's focus through a session restore
        // that opens several files of the same language in
        // succession. No-op when nothing is queued.
        self.drain_pending_lsp_prompt_for_active_buffer();

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
            // Split horizontally based on side placement
            tracing::trace!(
                "render: file explorer layout active (present={}, sync_in_progress={}, side={:?})",
                self.file_explorer.is_some(),
                self.file_explorer_sync_in_progress,
                self.file_explorer_side
            );
            let explorer_cols = self.file_explorer_width.to_cols(main_content_area.width);

            let (explorer_area, editor_area) = match self.file_explorer_side {
                FileExplorerSide::Left => {
                    let chunks = Layout::default()
                        .direction(Direction::Horizontal)
                        .constraints([Constraint::Length(explorer_cols), Constraint::Min(0)])
                        .split(main_content_area);
                    (chunks[0], chunks[1])
                }
                FileExplorerSide::Right => {
                    let chunks = Layout::default()
                        .direction(Direction::Horizontal)
                        .constraints([Constraint::Min(0), Constraint::Length(explorer_cols)])
                        .split(main_content_area);
                    (chunks[1], chunks[0])
                }
            };

            self.cached_layout.file_explorer_area = Some(explorer_area);
            editor_content_area = editor_area;

            // Get connection string before mutable borrow of file_explorer.
            let remote_connection = self.connection_display_string();

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
                let empty: Vec<std::path::PathBuf> = Vec::new();
                let cut_paths = self
                    .file_explorer_clipboard
                    .as_ref()
                    .filter(|cb| cb.is_cut)
                    .map(|cb| cb.paths.as_slice())
                    .unwrap_or(empty.as_slice());
                FileExplorerRenderer::render(
                    explorer,
                    frame,
                    explorer_area,
                    is_focused,
                    &files_with_unsaved_changes,
                    &self.file_explorer_decoration_cache,
                    &keybindings,
                    self.key_context.clone(),
                    &self.theme,
                    close_button_hovered,
                    remote_connection.as_deref(),
                    cut_paths,
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

        // The active split's buffer renderer records where the hardware
        // cursor *wants* to appear here; we only commit it to the frame at
        // the very end of this draw pass, after popups have been rendered,
        // so a popup covering the cursor cell causes the cursor to be
        // hidden (otherwise the hardware caret would bleed through the
        // popup).
        let mut pending_hardware_cursor: Option<(u16, u16)> = None;

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
            self.config.editor.highlight_current_column,
            &mut self.cached_layout.cell_theme_map,
            size.width,
            &mut pending_hardware_cursor,
        );

        drop(_content_span);

        // Cursor-jump animation: compare the cursor's screen position to
        // the prior frame and animate either when the cursor crossed split
        // panes or moved more than two rows within the same pane. The
        // trail crosses pane separators when the jump is across splits —
        // that's the intended "follow the focus" cue.
        self.maybe_start_cursor_jump_animation(pending_hardware_cursor, active_split);

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

        // Promote any deferred virtual-buffer animations whose Rect is now
        // known. Done here (after split_areas is recomputed, before
        // apply_all runs at the end of render) so the first frame of the
        // effect lands on the same paint that made the buffer visible.
        self.drain_pending_vb_animations();
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

        // Reflect the active buffer in the terminal window/tab title. Only
        // writes when the title actually changes so we don't flood stdout
        // with OSC sequences every frame.
        self.update_terminal_title(&display_name);

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
        let buffer_lsp_disabled_reason = self
            .buffer_metadata
            .get(&self.active_buffer())
            .filter(|m| !m.lsp_enabled)
            .and_then(|m| m.lsp_disabled_reason.as_deref());
        let (lsp_status, lsp_indicator_state) = compose_lsp_status(
            &current_language,
            buffer_lsp_disabled_reason,
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
                Some(HoverTarget::StatusBarRemoteIndicator) => StatusBarHover::RemoteIndicator,
                _ => StatusBarHover::None,
            };

            let remote_connection = self.connection_display_string();

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
            let is_synthetic_placeholder = self
                .buffer_metadata
                .get(&active_buf)
                .map(|m| m.synthetic_placeholder)
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
                remote_state_override: self.remote_indicator_override.as_ref(),
                is_synthetic_placeholder,
                // Filled in by `render_status` from the user's
                // status_bar config; the value here is just a
                // safe default for the rare path that builds the
                // ctx but doesn't run `render_status`.
                remote_indicator_on_bar: false,
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
            self.cached_layout.status_bar_remote_area = status_bar_layout.remote_indicator;
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

        // Render editor-level popups (e.g. plugin action popups) on top of any
        // buffer content so they stay visible across buffer switches and over
        // virtual buffers (Dashboard, diagnostics) that own the whole split.
        // These don't need cursor-relative positioning — they all use absolute
        // positions like BottomRight or Centered.
        //
        // Queue semantics: concurrent action popups stack in `global_popups`,
        // but only the top one renders & receives input. Deeper popups
        // surface as the top is resolved — the alternative (drawing all at
        // the same BottomRight slot) makes them illegible.
        self.cached_layout.global_popup_areas.clear();
        if let Some(popup) = self.global_popups.top() {
            let top_idx = self.global_popups.all().len() - 1;
            let popup_area = popup.calculate_area(size, None);
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
            self.cached_layout.global_popup_areas.push((
                top_idx,
                popup_area,
                inner_area,
                popup.scroll_offset,
                num_items,
            ));
            popup.render_with_hover(frame, popup_area, &theme_clone, hover_target.as_ref());
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
            // Pre-expand DynamicSubmenu items once per registry; without this
            // MenuRenderer::render rescans + reparses every theme JSON file
            // on every frame.
            self.expanded_menus_cache.update(
                &self.theme_registry,
                &self.menus,
                &self.menu_state.themes_dir,
            );
            let expanded = self.expanded_menus_cache.get().expect("just updated");
            let keybindings = self.keybindings.read().unwrap();
            self.cached_layout.menu_layout = Some(crate::view::ui::MenuRenderer::render(
                frame,
                menu_bar_area,
                expanded,
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

        if let Some(ref menu) = self.file_explorer_context_menu {
            self.render_file_explorer_context_menu(frame, menu);
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

        // Commit the active-split hardware cursor (deferred since
        // `render_content`) unless a popup has been drawn over that cell.
        // Ratatui draws the hardware caret on top of every cell, so a
        // popup cannot hide the cursor by painting cells — the only way
        // to hide it is to leave `Frame::cursor_position` as `None`, which
        // triggers `Terminal::hide_cursor` at the end of the draw.
        //
        // When a prompt is active the prompt renderer already placed the
        // caret on the prompt line via `frame.set_cursor_position`; don't
        // override it with the (now-irrelevant) buffer cursor.
        if let Some((cx, cy)) = pending_hardware_cursor {
            if self.prompt.is_none() && !self.cursor_obscured_by_overlay(cx, cy) {
                frame.set_cursor_position((cx, cy));
            }
        }

        // Convert all colors for terminal capability (256/16 color fallback)
        crate::view::color_support::convert_buffer_colors(
            frame.buffer_mut(),
            self.color_capability,
        );

        // Frame-buffer animations run last so they mutate the final paint.
        self.animations.apply_all(frame.buffer_mut());
    }

    /// Compare the hardware cursor's screen position to the previous frame's
    /// and, if it moved by more than the "jump" threshold, start a
    /// `CursorJump` animation from the old to the new on-screen position.
    /// Successive jumps cancel the prior animation so trail effects don't
    /// pile up.
    ///
    /// Cross-split and cross-buffer transitions (focus change, tab switch)
    /// are also animated — the trail crosses pane separators on its way
    /// from one buffer's cursor cell to another's.
    ///
    /// The threshold is intentionally generous: arrow-key/typing moves
    /// (small `dx`/`dy`) must NOT trigger the animation, but search jumps,
    /// goto-line/definition, and pane switches (which always cross several
    /// rows or many columns) must.
    fn maybe_start_cursor_jump_animation(
        &mut self,
        current_pos: Option<(u16, u16)>,
        active_split: crate::model::event::LeafId,
    ) {
        // Honour the global animations toggle. Tests default to
        // `animations = false` so single-tick `render()` calls observe the
        // settled buffer instead of a mid-flight trail; users can also
        // disable animations entirely from config.
        if !self.config.editor.animations {
            self.previous_cursor_screen_pos = current_pos.map(|p| (p, active_split));
            return;
        }

        let Some(current) = current_pos else {
            // Cursor is hidden this frame (e.g. prompt has focus). Reset the
            // tracker so the re-emerging cursor doesn't animate from a stale
            // spot when focus returns to a buffer.
            self.previous_cursor_screen_pos = None;
            return;
        };

        let prev_entry = self.previous_cursor_screen_pos;
        // Update tracking unconditionally for the next frame.
        self.previous_cursor_screen_pos = Some((current, active_split));

        let Some((prev, prev_split)) = prev_entry else {
            return;
        };
        if prev == current && prev_split == active_split {
            return;
        }

        let dx = (current.0 as i32 - prev.0 as i32).abs();
        let dy = (current.1 as i32 - prev.1 as i32).abs();
        // Animate when the cursor crossed split panes, or when it made a
        // non-incremental move within the same pane: more than two rows
        // vertically, or at least ten columns horizontally. Small hops
        // (typing, arrow keys, word-jump, home/end on short lines) are
        // intentionally skipped.
        let crossed_panes = prev_split != active_split;
        let row_jump = dy > 2;
        let col_jump = dx >= 10;
        if !crossed_panes && !row_jump && !col_jump {
            return;
        }

        // Cancel any prior cursor-jump animation so trails don't stack.
        if let Some(prev_anim) = self.cursor_jump_animation.take() {
            self.animations.cancel(prev_anim);
        }

        let id = self.animations.start(
            // The bounding box is for runner bookkeeping only — CursorJump
            // paints at absolute screen coords and ignores `area`.
            ratatui::layout::Rect {
                x: prev.0.min(current.0),
                y: prev.1.min(current.1),
                width: dx as u16 + 1,
                height: dy as u16 + 1,
            },
            crate::view::animation::AnimationKind::CursorJump {
                from: prev,
                to: current,
                duration: std::time::Duration::from_millis(140),
                cursor_color: self.theme.cursor,
                bg_color: self.theme.editor_bg,
            },
        );
        self.cursor_jump_animation = Some(id);
    }

    /// Returns true if `(x, y)` falls inside any popup-style overlay that
    /// was rendered this frame. Used to decide whether the hardware cursor
    /// should be shown or hidden so it does not bleed through a popup.
    fn cursor_obscured_by_overlay(&self, x: u16, y: u16) -> bool {
        let inside = |rect: ratatui::layout::Rect| -> bool {
            x >= rect.x
                && x < rect.x.saturating_add(rect.width)
                && y >= rect.y
                && y < rect.y.saturating_add(rect.height)
        };

        if self
            .cached_layout
            .popup_areas
            .iter()
            .any(|entry| inside(entry.1))
        {
            return true;
        }
        if self
            .cached_layout
            .global_popup_areas
            .iter()
            .any(|entry| inside(entry.1))
        {
            return true;
        }
        if let Some((rect, _, _, _)) = self.cached_layout.suggestions_area {
            if inside(rect) {
                return true;
            }
        }
        if let Some(ref fb) = self.file_browser_layout {
            if inside(fb.popup_area) {
                return true;
            }
        }
        false
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

    /// Render the file explorer context menu
    fn render_file_explorer_context_menu(
        &self,
        frame: &mut Frame,
        menu: &super::types::FileExplorerContextMenu,
    ) {
        use ratatui::style::Style;
        use ratatui::text::{Line, Span};
        use ratatui::widgets::{Block, Borders, Clear, Paragraph};

        let items = menu.items();
        let menu_width = super::types::FILE_EXPLORER_CONTEXT_MENU_WIDTH;
        let menu_height = menu.height();
        let (menu_x, menu_y) = menu.clamped_position(frame.area().width, frame.area().height);

        let area = ratatui::layout::Rect::new(menu_x, menu_y, menu_width, menu_height);

        frame.render_widget(Clear, area);

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

            let label = item.label();
            let content_width = (menu_width as usize).saturating_sub(2);
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
            let explorer_cols = self.file_explorer_width.to_cols(main_content_area.width);
            let horizontal_chunks = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([Constraint::Length(explorer_cols), Constraint::Min(0)])
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

    /// Emit an OSC 2 escape sequence to set the host terminal's window/tab
    /// title based on the active buffer's display name. Deduplicated against
    /// the last title we wrote so we don't spam stdout every frame.
    ///
    /// Gated by `editor.set_window_title` (default on). Terminals that
    /// don't implement OSC 2 silently drop the sequence.
    fn update_terminal_title(&mut self, display_name: &str) {
        if !self.config.editor.set_window_title {
            return;
        }
        let new_title = format!("{} \u{2014} Fresh", display_name);
        if self.last_window_title.as_deref() == Some(new_title.as_str()) {
            return;
        }
        crate::services::terminal_title::write_terminal_title(&new_title);
        self.last_window_title = Some(new_title);
    }

    /// Save all prompt histories to disk
    /// Called on shutdown to persist history across sessions
    pub fn save_histories(&self) {
        // Ensure data directory exists
        if let Err(e) = self
            .authority
            .filesystem
            .create_dir_all(&self.dir_context.data_dir)
        {
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
}
