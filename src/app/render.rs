use super::*;

impl Editor {
    /// Render the editor to the terminal
    pub fn render(&mut self, frame: &mut Frame) {
        let _span = tracing::trace_span!("render").entered();
        let size = frame.area();

        // NOTE: Viewport sync with cursor is handled by split_rendering.rs which knows the
        // correct content area dimensions. Don't sync here with incorrect EditorState viewport size.

        // Prepare all buffers for rendering (pre-load viewport data for lazy loading)
        // Each split may have a different viewport position on the same buffer
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

        // Refresh search highlights for the current viewport if we have an active search
        // This ensures highlights update when scrolling to show matches in the new viewport
        if let Some(ref search_state) = self.search_state {
            let query = search_state.query.clone();
            self.update_search_highlights(&query);
        }

        // Determine if we need to show search options bar
        let show_search_options = self.prompt.as_ref().map_or(false, |p| {
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
            .map_or(false, |p| !p.suggestions.is_empty());
        let has_file_browser = self
            .prompt
            .as_ref()
            .map_or(false, |p| p.prompt_type == PromptType::OpenFile)
            && self.file_open_state.is_some();

        // Build main vertical layout: [menu_bar, main_content, status_bar, search_options, prompt_line]
        // Status bar is hidden when suggestions popup is shown
        // Search options bar is shown when in search prompt
        let constraints = vec![
            Constraint::Length(1), // Menu bar
            Constraint::Min(0),    // Main content area
            Constraint::Length(if has_suggestions || has_file_browser {
                0
            } else {
                1
            }), // Status bar (hidden with popups)
            Constraint::Length(if show_search_options { 1 } else { 0 }), // Search options bar
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
        let editor_content_area;

        if self.file_explorer_visible && self.file_explorer.is_some() {
            // Split horizontally: [file_explorer | editor]
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

            // Render file explorer
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
                    &self.keybindings,
                    self.key_context,
                    &self.theme,
                    close_button_hovered,
                );
            }
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
        if let Some(ref mut ts_manager) = self.ts_plugin_manager {
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
                    let render_start_args =
                        crate::services::plugins::hooks::HookArgs::RenderStart { buffer_id };
                    ts_manager.run_hook("render_start", render_start_args);

                    // Fire view_transform_request hook with base tokens
                    // This allows plugins to transform the view (e.g., soft breaks for markdown)
                    let visible_count = split_area.height as usize;
                    let is_binary = state.buffer.is_binary();
                    let base_tokens =
                        crate::view::ui::split_rendering::SplitRenderer::build_base_tokens_for_hook(
                            &mut state.buffer,
                            viewport_top_byte,
                            self.config.editor.estimated_line_length,
                            visible_count,
                            is_binary,
                        );
                    let viewport_start = viewport_top_byte;
                    let viewport_end = base_tokens
                        .last()
                        .and_then(|t| t.source_offset)
                        .unwrap_or(viewport_start);
                    let transform_args =
                        crate::services::plugins::hooks::HookArgs::ViewTransformRequest {
                            buffer_id,
                            split_id,
                            viewport_start,
                            viewport_end,
                            tokens: base_tokens,
                        };
                    ts_manager.run_hook("view_transform_request", transform_args);

                    // Use the split area height as visible line count
                    let visible_count = split_area.height as usize;
                    let top_byte = viewport_top_byte;

                    // Get or create the seen byte ranges set for this buffer
                    let seen_byte_ranges = self
                        .seen_byte_ranges
                        .entry(buffer_id)
                        .or_insert_with(std::collections::HashSet::new);

                    // Collect only NEW lines (not seen before based on byte range)
                    let mut new_lines: Vec<crate::services::plugins::hooks::LineInfo> = Vec::new();
                    let mut line_number = state.buffer.get_line_number(top_byte);
                    let mut iter = state
                        .buffer
                        .line_iterator(top_byte, self.config.editor.estimated_line_length);

                    for _ in 0..visible_count {
                        if let Some((line_start, line_content)) = iter.next() {
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
                        let hook_args = crate::services::plugins::hooks::HookArgs::LinesChanged {
                            buffer_id,
                            lines: new_lines,
                        };
                        ts_manager.run_hook("lines_changed", hook_args);
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

            // Process any plugin commands (like AddOverlay) that resulted from the hooks
            let commands = ts_manager.process_commands();
            for command in commands {
                if let Err(e) = self.handle_plugin_command(command) {
                    tracing::error!("Error handling plugin command: {}", e);
                }
            }
        }

        // Render editor content (same for both layouts)
        let lsp_waiting = self.pending_completion_request.is_some()
            || self.pending_goto_definition_request.is_some();

        // Hide the hardware cursor when menu is open, file explorer is focused, or in terminal mode
        // (the file explorer will set its own cursor position when focused)
        // (terminal mode renders its own cursor via the terminal emulator)
        // This also causes visual cursor indicators in the editor to be dimmed
        let hide_cursor = self.menu_state.active_menu.is_some()
            || self.key_context == KeyContext::FileExplorer
            || self.terminal_mode;

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

        let (split_areas, tab_areas, close_split_areas, view_line_mappings) =
            SplitRenderer::render_content(
                frame,
                editor_content_area,
                &self.split_manager,
                &mut self.buffers,
                &self.buffer_metadata,
                &mut self.event_logs,
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
            );

        // Render terminal content on top of split content for terminal buffers
        self.render_terminal_splits(frame, &split_areas);

        self.cached_layout.split_areas = split_areas;
        self.cached_layout.tab_areas = tab_areas;
        self.cached_layout.close_split_areas = close_split_areas;
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
            // For OpenFile prompt, render the file browser popup
            if prompt.prompt_type == PromptType::OpenFile {
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
                    );
                }
            } else if !prompt.suggestions.is_empty() {
                // For other prompts, render suggestions as before
                // Calculate overlay area: position above prompt line (which is below status bar)
                let suggestion_count = prompt.suggestions.len().min(10);
                let height = suggestion_count as u16 + 2; // +2 for borders

                // Position suggestions above the prompt line
                // The prompt line is at main_chunks[3], so suggestions go above it
                let suggestions_area = ratatui::layout::Rect {
                    x: 0,
                    y: main_chunks[prompt_line_idx].y.saturating_sub(height),
                    width: size.width,
                    height,
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
            StatusBarRenderer::render_status_bar(
                frame,
                main_chunks[status_bar_idx],
                self.active_state_mut(), // Use the mutable reference
                &status_message,
                &plugin_status_message,
                &lsp_status,
                &theme,
                &display_name,
                &keybindings_cloned,         // Pass the cloned keybindings
                &chord_state_cloned,         // Pass the cloned chord state
                update_available.as_deref(), // Pass update availability
            );
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

            StatusBarRenderer::render_search_options(
                frame,
                main_chunks[search_options_idx],
                self.search_case_sensitive,
                self.search_whole_word,
                self.search_use_regex,
                confirm_each,
                &theme,
                &keybindings_cloned,
            );
        }

        // Render prompt line if active
        if let Some(prompt) = &prompt {
            // Use specialized renderer for file open prompt to show colorized path
            if prompt.prompt_type == crate::view::prompt::PromptType::OpenFile {
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

            let state = self.active_state_mut();
            if state.popups.is_visible() {
                // Get the primary cursor position for popup positioning
                let primary_cursor = state.cursors.primary();
                let cursor_screen_pos = viewport
                    .as_ref()
                    .map(|vp| vp.cursor_screen_position(&mut state.buffer, primary_cursor))
                    .unwrap_or((0, 0));

                // Adjust cursor position to account for tab bar (1 line offset)
                let cursor_screen_pos = (cursor_screen_pos.0, cursor_screen_pos.1 + 1);

                // Collect popup data
                state
                    .popups
                    .all()
                    .iter()
                    .enumerate()
                    .map(|(popup_idx, popup)| {
                        let popup_area = popup.calculate_area(size, Some(cursor_screen_pos));

                        // Track popup area for mouse hit testing
                        let inner_area = if popup.bordered {
                            ratatui::layout::Rect {
                                x: popup_area.x + 1,
                                y: popup_area.y + 1,
                                width: popup_area.width.saturating_sub(2),
                                height: popup_area.height.saturating_sub(2),
                            }
                        } else {
                            popup_area
                        };

                        let num_items = match &popup.content {
                            crate::view::popup::PopupContent::List { items, .. } => items.len(),
                            _ => 0,
                        };

                        (
                            popup_idx,
                            popup_area,
                            inner_area,
                            popup.scroll_offset,
                            num_items,
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
                if let Some((_, popup_area, _, _, _)) = popup_info.get(popup_idx) {
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
        // Collect values first to avoid borrow conflicts
        use crate::view::ui::context_keys;
        let line_numbers = self
            .buffers
            .get(&self.active_buffer())
            .map(|state| state.margins.show_line_numbers)
            .unwrap_or(true);
        let line_wrap = {
            let active_split = self.split_manager.active_split();
            self.split_view_states
                .get(&active_split)
                .map(|vs| vs.viewport.line_wrap_enabled)
                .unwrap_or(false)
        };
        let compose_mode = self
            .buffers
            .get(&self.active_buffer())
            .map(|state| state.view_mode == crate::state::ViewMode::Compose)
            .unwrap_or(false);
        let file_explorer_exists = self.file_explorer.is_some();
        let file_explorer_focused =
            self.key_context == crate::input::keybindings::KeyContext::FileExplorer;
        let mouse_capture = self.mouse_enabled;
        let mouse_hover = self.config.editor.mouse_hover_enabled;
        // Check if LSP is enabled for this buffer AND the server is running and ready
        let lsp_available = self
            .buffer_metadata
            .get(&self.active_buffer())
            .and_then(|metadata| {
                if !metadata.lsp_enabled {
                    return None;
                }
                // Get file path and detect language
                metadata.file_path().and_then(|path| {
                    detect_language(path, &self.config.languages).and_then(|language| {
                        // Check if LSP server for this language is ready
                        self.lsp.as_ref().map(|lsp| lsp.is_server_ready(&language))
                    })
                })
            })
            .unwrap_or(false);
        let show_hidden = self
            .file_explorer
            .as_ref()
            .map(|fe| fe.ignore_patterns().show_hidden())
            .unwrap_or(false);
        let show_gitignored = self
            .file_explorer
            .as_ref()
            .map(|fe| fe.ignore_patterns().show_gitignored())
            .unwrap_or(false);
        let has_selection = self.has_active_selection();

        self.menu_state
            .context
            .set(context_keys::LINE_NUMBERS, line_numbers)
            .set(context_keys::LINE_WRAP, line_wrap)
            .set(context_keys::COMPOSE_MODE, compose_mode)
            .set(context_keys::FILE_EXPLORER, file_explorer_exists)
            .set(context_keys::FILE_EXPLORER_FOCUSED, file_explorer_focused)
            .set(context_keys::MOUSE_CAPTURE, mouse_capture)
            .set(context_keys::MOUSE_HOVER, mouse_hover)
            .set(context_keys::LSP_AVAILABLE, lsp_available)
            .set(context_keys::FILE_EXPLORER_SHOW_HIDDEN, show_hidden)
            .set(context_keys::FILE_EXPLORER_SHOW_GITIGNORED, show_gitignored)
            .set(context_keys::HAS_SELECTION, has_selection);

        crate::view::ui::MenuRenderer::render(
            frame,
            menu_bar_area,
            &self.config.menu,
            &self.menu_state,
            &self.keybindings,
            &self.theme,
            self.mouse_state.hover_target.as_ref(),
        );

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
                        let hover_style = Style::default().fg(self.theme.scrollbar_thumb_hover_fg);
                        for row_offset in *thumb_start..*thumb_end {
                            let paragraph = Paragraph::new(Span::styled("█", hover_style));
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
                            Style::default().fg(self.theme.scrollbar_track_hover_fg);
                        let thumb_style = Style::default().fg(self.theme.scrollbar_thumb_fg);
                        for row_offset in 0..scrollbar_rect.height {
                            let is_thumb = (row_offset as usize) >= *thumb_start
                                && (row_offset as usize) < *thumb_end;
                            let (char, style) = if is_thumb {
                                ("█", thumb_style)
                            } else {
                                ("│", track_hover_style)
                            };
                            let paragraph = Paragraph::new(Span::styled(char, style));
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
        };
        self.apply_event_to_active_buffer(&event);
        // Return the handle of the last added overlay
        let state = self.active_state();
        state
            .overlays
            .all()
            .last()
            .map(|o| o.handle.clone())
            .unwrap_or_else(crate::view::overlay::OverlayHandle::new)
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

    /// Dismiss transient popups (Hover, Signature Help) if present
    /// These popups should be dismissed on scroll or other user actions
    pub(super) fn dismiss_transient_popups(&mut self) {
        let is_transient_popup = self
            .active_state()
            .popups
            .top()
            .and_then(|p| p.title.as_ref())
            .is_some_and(|title| title == "Hover" || title == "Signature Help");

        if is_transient_popup {
            self.hide_popup();
            tracing::debug!("Dismissed transient popup on scroll");
        }
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
            PopupContentData, PopupData, PopupListItemData, PopupPositionData,
        };

        // Store the pending confirmation
        self.pending_lsp_confirmation = Some(language.to_string());

        // Get the server command for display
        let server_info = if let Some(lsp) = &self.lsp {
            if let Some(config) = lsp.get_config(language) {
                format!("{} ({})", language, config.command)
            } else {
                language.to_string()
            }
        } else {
            language.to_string()
        };

        let popup = PopupData {
            title: Some(format!("Start LSP Server: {}?", server_info)),
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
                    if lsp.get_or_spawn(&language).is_some() {
                        tracing::info!("LSP server for {} started (allowed once)", language);
                        self.set_status_message(format!("LSP server for {} started", language));
                    } else {
                        self.set_status_message(format!(
                            "Failed to start LSP server for {}",
                            language
                        ));
                    }
                }
                // Notify LSP about the current file
                self.notify_lsp_current_file_opened(&language);
            }
            "allow_always" => {
                // Spawn the LSP server and remember the preference
                if let Some(lsp) = &mut self.lsp {
                    lsp.allow_language(&language);
                    if lsp.get_or_spawn(&language).is_some() {
                        tracing::info!("LSP server for {} started (always allowed)", language);
                        self.set_status_message(format!(
                            "LSP server for {} started (will auto-start in future)",
                            language
                        ));
                    } else {
                        self.set_status_message(format!(
                            "Failed to start LSP server for {}",
                            language
                        ));
                    }
                }
                // Notify LSP about the current file
                self.notify_lsp_current_file_opened(&language);
            }
            "deny" | _ => {
                // User declined - don't start the server
                tracing::info!("LSP server for {} startup declined by user", language);
                self.set_status_message(format!("LSP server for {} startup cancelled", language));
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

        // Get the file path and verify language matches
        let path = match metadata.file_path() {
            Some(p) => p,
            None => {
                tracing::debug!("notify_lsp_current_file_opened: no file path for buffer");
                return;
            }
        };

        let file_language = match detect_language(path, &self.config.languages) {
            Some(l) => l,
            None => {
                tracing::debug!(
                    "notify_lsp_current_file_opened: no language detected for {:?}",
                    path
                );
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

        // Get the buffer text and line count before borrowing lsp
        let active_buffer = self.active_buffer();
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

        // Send didOpen to LSP
        if let Some(lsp) = &mut self.lsp {
            if let Some(client) = lsp.get_or_spawn(language) {
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

    /// Try to get or spawn an LSP handle, showing confirmation popup if needed
    ///
    /// This is the recommended way to access LSP functionality. It checks if
    /// confirmation is required and shows the popup if so.
    ///
    /// Returns:
    /// - `Some(true)` if LSP is ready (handle was already available or spawned)
    /// - `Some(false)` if confirmation popup was shown (user needs to respond)
    /// - `None` if LSP is not available (disabled, not configured, not auto-start, or failed)
    pub fn try_get_lsp_with_confirmation(&mut self, language: &str) -> Option<bool> {
        use crate::services::lsp::manager::LspSpawnResult;

        let result = {
            let lsp = self.lsp.as_mut()?;
            lsp.try_spawn(language)
        };

        match result {
            LspSpawnResult::Spawned => Some(true),
            LspSpawnResult::NotAutoStart => None, // Not configured for auto-start
            LspSpawnResult::Failed => None,
        }
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

    /// Notify LSP of a text change event
    pub(super) fn notify_lsp_change(&mut self, event: &Event) {
        // Collect all changes from the event (handles batches efficiently)
        let changes = self.collect_lsp_changes(event);
        if changes.is_empty() {
            return;
        }

        // Check if LSP is enabled for this buffer
        let metadata = match self.buffer_metadata.get(&self.active_buffer()) {
            Some(m) => m,
            None => {
                tracing::debug!(
                    "notify_lsp_change: no metadata for buffer {:?}",
                    self.active_buffer()
                );
                return;
            }
        };

        if !metadata.lsp_enabled {
            // LSP is disabled for this buffer, don't try to spawn or notify
            tracing::debug!("notify_lsp_change: LSP disabled for this buffer");
            return;
        }

        // Get the URI (computed once in with_file)
        let uri = match metadata.file_uri() {
            Some(u) => u.clone(),
            None => {
                tracing::debug!(
                    "notify_lsp_change: no URI for buffer (not a file or URI creation failed)"
                );
                return;
            }
        };

        // Get the file path for language detection
        let path = match metadata.file_path() {
            Some(p) => p,
            None => {
                tracing::debug!("notify_lsp_change: no file path for buffer");
                return;
            }
        };

        let language = match detect_language(path, &self.config.languages) {
            Some(l) => l,
            None => {
                tracing::debug!("notify_lsp_change: no language detected for {:?}", path);
                return;
            }
        };

        tracing::debug!(
            "notify_lsp_change: sending {} changes to {} in single didChange notification",
            changes.len(),
            uri.as_str()
        );

        if let Some(lsp) = &mut self.lsp {
            if let Some(client) = lsp.get_or_spawn(&language) {
                // Send all changes in a single didChange notification
                // This is much more efficient for batch operations like LSP rename
                if let Err(e) = client.did_change(uri.clone(), changes) {
                    tracing::warn!("Failed to send didChange to LSP: {}", e);
                } else {
                    tracing::info!("Successfully sent batched didChange to LSP");

                    // Request pull diagnostics after the change
                    // TODO: Consider debouncing this to avoid excessive requests during rapid typing
                    let previous_result_id = self.diagnostic_result_ids.get(uri.as_str()).cloned();
                    let request_id = self.next_lsp_request_id;
                    self.next_lsp_request_id += 1;

                    if let Err(e) =
                        client.document_diagnostic(request_id, uri.clone(), previous_result_id)
                    {
                        tracing::debug!(
                            "Failed to request pull diagnostics after change (server may not support): {}",
                            e
                        );
                    } else {
                        tracing::debug!(
                            "Requested pull diagnostics after change for {} (request_id={})",
                            uri.as_str(),
                            request_id
                        );
                    }
                }
            } else {
                tracing::warn!(
                    "notify_lsp_change: failed to get or spawn LSP client for {}",
                    language
                );
            }
        } else {
            tracing::debug!("notify_lsp_change: no LSP manager available");
        }
    }

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
        let path = match metadata.file_path() {
            Some(p) => p,
            None => {
                tracing::debug!("notify_lsp_save: no file path for buffer");
                return;
            }
        };

        let language = match detect_language(path, &self.config.languages) {
            Some(l) => l,
            None => {
                tracing::debug!("notify_lsp_save: no language detected for {:?}", path);
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

        if let Some(lsp) = &mut self.lsp {
            if let Some(client) = lsp.get_or_spawn(&language) {
                // Send didSave with the full text content
                if let Err(e) = client.did_save(uri, Some(full_text)) {
                    tracing::warn!("Failed to send didSave to LSP: {}", e);
                } else {
                    tracing::info!("Successfully sent didSave to LSP");
                }
            } else {
                tracing::warn!(
                    "notify_lsp_save: failed to get or spawn LSP client for {}",
                    language
                );
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

        convert_action_to_events(
            self.active_state_mut(),
            action,
            tab_size,
            auto_indent,
            estimated_line_length,
            viewport_height,
        )
    }

    // === Search and Replace Methods ===

    /// Clear all search highlights from the active buffer
    pub(super) fn clear_search_highlights(&mut self) {
        let ns = self.search_namespace.clone();
        let state = self.active_state_mut();
        state.overlays.clear_namespace(&ns, &mut state.marker_list);

        // Also clear search state
        self.search_state = None;
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
                if let Some((line_start, line_content)) = line_iter.next() {
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
            self.set_status_message("Search cancelled.".to_string());
            return;
        }

        let search_range = self.pending_search_range.take();

        let buffer_content = {
            let state = self.active_state();
            match state.buffer.to_string() {
                Some(t) => t,
                None => {
                    self.set_status_message("Buffer not fully loaded".to_string());
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
                self.set_status_message(format!("Invalid regex: {}", e));
                return;
            }
        };

        // Find all matches within the search range
        let search_slice = &buffer_content[search_start..search_end];
        let matches: Vec<usize> = regex
            .find_iter(search_slice)
            .map(|m| search_start + m.start())
            .collect();

        if matches.is_empty() {
            self.search_state = None;
            let msg = if search_range.is_some() {
                format!("No matches found for '{}' in selection", query)
            } else {
                format!("No matches found for '{}'", query)
            };
            self.set_status_message(msg);
            return;
        }

        // Find the first match at or after the current cursor position
        let cursor_pos = {
            let state = self.active_state();
            state.cursors.primary().position
        };
        let current_match_index = matches
            .iter()
            .position(|&pos| pos >= cursor_pos)
            .unwrap_or(0);

        // Move cursor to the first match
        let match_pos = matches[current_match_index];
        {
            let active_split = self.split_manager.active_split();
            let active_buffer = self.active_buffer();
            let state = self.active_state_mut();
            state.cursors.primary_mut().position = match_pos;
            state.cursors.primary_mut().anchor = None;
            // Ensure cursor is visible - get viewport from SplitViewState
            if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                let state = self.buffers.get_mut(&active_buffer).unwrap();
                view_state
                    .viewport
                    .ensure_visible(&mut state.buffer, state.cursors.primary());
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
            case_sensitive: self.search_case_sensitive,
            whole_word: self.search_whole_word,
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

    /// Find the next match
    pub(super) fn find_next(&mut self) {
        if let Some(ref mut search_state) = self.search_state {
            if search_state.matches.is_empty() {
                return;
            }

            let current_index = search_state.current_match_index.unwrap_or(0);
            let next_index = if current_index + 1 < search_state.matches.len() {
                current_index + 1
            } else if search_state.wrap_search {
                0 // Wrap to beginning
            } else {
                self.set_status_message("No more matches.".to_string());
                return;
            };

            search_state.current_match_index = Some(next_index);
            let match_pos = search_state.matches[next_index];
            let matches_len = search_state.matches.len();

            {
                let active_split = self.split_manager.active_split();
                let active_buffer = self.active_buffer();
                let state = self.active_state_mut();
                state.cursors.primary_mut().position = match_pos;
                state.cursors.primary_mut().anchor = None;
                // Ensure cursor is visible - get viewport from SplitViewState
                if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                    let state = self.buffers.get_mut(&active_buffer).unwrap();
                    view_state
                        .viewport
                        .ensure_visible(&mut state.buffer, state.cursors.primary());
                }
            }

            self.set_status_message(format!("Match {} of {}", next_index + 1, matches_len));
        } else {
            self.set_status_message("No active search. Press Ctrl+F to search.".to_string());
        }
    }

    /// Find the previous match
    pub(super) fn find_previous(&mut self) {
        if let Some(ref mut search_state) = self.search_state {
            if search_state.matches.is_empty() {
                return;
            }

            let current_index = search_state.current_match_index.unwrap_or(0);
            let prev_index = if current_index > 0 {
                current_index - 1
            } else if search_state.wrap_search {
                search_state.matches.len() - 1 // Wrap to end
            } else {
                self.set_status_message("No more matches.".to_string());
                return;
            };

            search_state.current_match_index = Some(prev_index);
            let match_pos = search_state.matches[prev_index];
            let matches_len = search_state.matches.len();

            {
                let active_split = self.split_manager.active_split();
                let active_buffer = self.active_buffer();
                let state = self.active_state_mut();
                state.cursors.primary_mut().position = match_pos;
                state.cursors.primary_mut().anchor = None;
                // Ensure cursor is visible - get viewport from SplitViewState
                if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                    let state = self.buffers.get_mut(&active_buffer).unwrap();
                    view_state
                        .viewport
                        .ensure_visible(&mut state.buffer, state.cursors.primary());
                }
            }

            self.set_status_message(format!("Match {} of {}", prev_index + 1, matches_len));
        } else {
            self.set_status_message("No active search. Press Ctrl+F to search.".to_string());
        }
    }

    /// Perform a replace-all operation
    /// Replaces all occurrences of the search query with the replacement text
    pub(super) fn perform_replace(&mut self, search: &str, replacement: &str) {
        if search.is_empty() {
            self.set_status_message("Replace: empty search query.".to_string());
            return;
        }

        // Find all matches first (before making any modifications)
        let matches = {
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
                    matches.push(offset);
                    current_pos = offset + search.len();
                } else {
                    break;
                }
            }
            matches
        };

        let count = matches.len();

        if count == 0 {
            self.set_status_message(format!("No occurrences of '{}' found.", search));
            return;
        }

        // Capture current cursor state for undo
        let cursor_id = self.active_state().cursors.primary_id();
        let cursor = self.active_state().cursors.get(cursor_id).unwrap().clone();
        let old_position = cursor.position;
        let old_anchor = cursor.anchor;
        let old_sticky_column = cursor.sticky_column;

        // Create events for all replacements (in reverse order to preserve positions)
        let mut events = Vec::new();

        // Add MoveCursor at the beginning to save cursor position for undo
        events.push(Event::MoveCursor {
            cursor_id,
            old_position,
            new_position: old_position, // Keep cursor where it is
            old_anchor,
            new_anchor: old_anchor,
            old_sticky_column,
            new_sticky_column: old_sticky_column,
        });

        for match_pos in matches.into_iter().rev() {
            let end = match_pos + search.len();
            let range = match_pos..end;

            // Get the text being deleted
            let deleted_text = self
                .active_state_mut()
                .get_text_range(range.start, range.end);

            // Add Delete event
            events.push(Event::Delete {
                range: range.clone(),
                deleted_text,
                cursor_id,
            });

            // Add Insert event
            events.push(Event::Insert {
                position: match_pos,
                text: replacement.to_string(),
                cursor_id,
            });
        }

        // Wrap all replacement events in a single Batch for atomic undo
        let batch = Event::Batch {
            events,
            description: format!("Replace all '{}' with '{}'", search, replacement),
        };

        // Apply through event log for proper undo support
        self.active_event_log_mut().append(batch.clone());
        self.apply_event_to_active_buffer(&batch);

        // Clear search state since positions are now invalid
        self.search_state = None;

        // Clear any search highlight overlays
        let ns = self.search_namespace.clone();
        let state = self.active_state_mut();
        state.overlays.clear_namespace(&ns, &mut state.marker_list);

        // Set status message
        self.set_status_message(format!(
            "Replaced {} occurrence{} of '{}' with '{}'",
            count,
            if count == 1 { "" } else { "s" },
            search,
            replacement
        ));
    }

    /// Start interactive replace mode (query-replace)
    pub(super) fn start_interactive_replace(&mut self, search: &str, replacement: &str) {
        if search.is_empty() {
            self.set_status_message("Query replace: empty search query.".to_string());
            return;
        }

        // Find the first match lazily (don't find all matches upfront)
        let state = self.active_state();
        let start_pos = state.cursors.primary().position;
        let first_match = state.buffer.find_next(search, start_pos);

        let Some(first_match_pos) = first_match else {
            self.set_status_message(format!("No occurrences of '{}' found.", search));
            return;
        };

        // Initialize interactive replace state with just the current match
        self.interactive_replace_state = Some(InteractiveReplaceState {
            search: search.to_string(),
            replacement: replacement.to_string(),
            current_match_pos: first_match_pos,
            start_pos: first_match_pos,
            has_wrapped: false,
            replacements_made: 0,
        });

        // Move cursor to first match
        let active_split = self.split_manager.active_split();
        let active_buffer = self.active_buffer();
        {
            let state = self.active_state_mut();
            state.cursors.primary_mut().position = first_match_pos;
            state.cursors.primary_mut().anchor = None;
        }
        // Ensure cursor is visible - get viewport from SplitViewState
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            let state = self.buffers.get_mut(&active_buffer).unwrap();
            view_state
                .viewport
                .ensure_visible(&mut state.buffer, state.cursors.primary());
        }

        // Show the query-replace prompt
        self.prompt = Some(Prompt::new(
            "Replace? (y)es (n)o (a)ll (c)ancel: ".to_string(),
            PromptType::QueryReplaceConfirm,
        ));
    }

    /// Handle interactive replace key press (y/n/a/c)
    pub(super) fn handle_interactive_replace_key(&mut self, c: char) -> std::io::Result<()> {
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
                if let Some((next_match, wrapped)) =
                    self.find_next_match_for_replace(&ir_state, search_pos)
                {
                    ir_state.current_match_pos = next_match;
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
                let search_pos = ir_state.current_match_pos + ir_state.search.len();
                if let Some((next_match, wrapped)) =
                    self.find_next_match_for_replace(&ir_state, search_pos)
                {
                    ir_state.current_match_pos = next_match;
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
                // Uses streaming search (doesn't materialize file), but collects positions for batch

                // First replace the current match
                self.replace_current_match(&ir_state)?;
                ir_state.replacements_made += 1;

                // Find all remaining matches using streaming search
                // Collecting positions (Vec<usize>) is low memory cost even for huge files
                let search_pos = ir_state.current_match_pos + ir_state.replacement.len();
                let remaining_matches = {
                    let mut matches = Vec::new();
                    let mut current_pos = search_pos;
                    let mut temp_state = ir_state.clone();

                    // Find matches lazily one at a time, collect positions
                    loop {
                        if let Some((next_match, wrapped)) =
                            self.find_next_match_for_replace(&temp_state, current_pos)
                        {
                            matches.push(next_match);
                            current_pos = next_match + temp_state.search.len();
                            if wrapped {
                                temp_state.has_wrapped = true;
                            }
                        } else {
                            break;
                        }
                    }
                    matches
                };

                let remaining_count = remaining_matches.len();

                if remaining_count > 0 {
                    // Capture current cursor state for undo
                    let cursor_id = self.active_state().cursors.primary_id();
                    let cursor = self.active_state().cursors.get(cursor_id).unwrap().clone();
                    let old_position = cursor.position;
                    let old_anchor = cursor.anchor;
                    let old_sticky_column = cursor.sticky_column;

                    // Create events for all remaining replacements (reverse order preserves positions)
                    let mut events = Vec::new();

                    // Add MoveCursor at the beginning to save cursor position for undo
                    events.push(Event::MoveCursor {
                        cursor_id,
                        old_position,
                        new_position: old_position, // Keep cursor where it is
                        old_anchor,
                        new_anchor: old_anchor,
                        old_sticky_column,
                        new_sticky_column: old_sticky_column,
                    });

                    for match_pos in remaining_matches.into_iter().rev() {
                        let end = match_pos + ir_state.search.len();
                        let range = match_pos..end;
                        let deleted_text = self
                            .active_state_mut()
                            .get_text_range(range.start, range.end);

                        events.push(Event::Delete {
                            range: range.clone(),
                            deleted_text,
                            cursor_id,
                        });

                        events.push(Event::Insert {
                            position: match_pos,
                            text: ir_state.replacement.clone(),
                            cursor_id,
                        });
                    }

                    // Single Batch = single undo step for all remaining replacements
                    let batch = Event::Batch {
                        events,
                        description: format!(
                            "Query replace remaining '{}' with '{}'",
                            ir_state.search, ir_state.replacement
                        ),
                    };

                    self.active_event_log_mut().append(batch.clone());
                    self.apply_event_to_active_buffer(&batch);

                    ir_state.replacements_made += remaining_count;
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
    pub(super) fn find_next_match_for_replace(
        &self,
        ir_state: &InteractiveReplaceState,
        start_pos: usize,
    ) -> Option<(usize, bool)> {
        let state = self.active_state();

        if ir_state.has_wrapped {
            // We've already wrapped - only search from start_pos up to (but not including) the original start position
            // Use find_next_in_range to avoid wrapping again
            let search_range = Some(start_pos..ir_state.start_pos);
            if let Some(match_pos) =
                state
                    .buffer
                    .find_next_in_range(&ir_state.search, start_pos, search_range)
            {
                return Some((match_pos, true));
            }
            None // No more matches before original start position
        } else {
            // Haven't wrapped yet - search normally from start_pos
            // First try from start_pos to end of buffer
            let buffer_len = state.buffer.len();
            let search_range = Some(start_pos..buffer_len);
            if let Some(match_pos) =
                state
                    .buffer
                    .find_next_in_range(&ir_state.search, start_pos, search_range)
            {
                return Some((match_pos, false));
            }

            // No match from start_pos to end - wrap to beginning
            // Search from 0 to start_pos (original position)
            let wrap_range = Some(0..ir_state.start_pos);
            if let Some(match_pos) =
                state
                    .buffer
                    .find_next_in_range(&ir_state.search, 0, wrap_range)
            {
                return Some((match_pos, true)); // Found match after wrapping
            }

            None // No matches found anywhere
        }
    }

    /// Replace the current match in interactive replace mode
    pub(super) fn replace_current_match(
        &mut self,
        ir_state: &InteractiveReplaceState,
    ) -> std::io::Result<()> {
        let match_pos = ir_state.current_match_pos;
        let search_len = ir_state.search.len();
        let range = match_pos..(match_pos + search_len);

        // Get the deleted text for the event
        let deleted_text = self
            .active_state_mut()
            .get_text_range(range.start, range.end);

        // Capture current cursor state for undo
        let cursor_id = self.active_state().cursors.primary_id();
        let cursor = self.active_state().cursors.get(cursor_id).unwrap().clone();
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
                text: ir_state.replacement.clone(),
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
        {
            let state = self.active_state_mut();
            state.cursors.primary_mut().position = match_pos;
            state.cursors.primary_mut().anchor = None;
        }
        // Ensure cursor is visible - get viewport from SplitViewState
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            let state = self.buffers.get_mut(&active_buffer).unwrap();
            view_state
                .viewport
                .ensure_visible(&mut state.buffer, state.cursors.primary());
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

        self.set_status_message(format!(
            "Replaced {} occurrence{}",
            replacements_made,
            if replacements_made == 1 { "" } else { "s" }
        ));
    }

    /// Smart home: toggle between line start and first non-whitespace character
    pub(super) fn smart_home(&mut self) {
        let estimated_line_length = self.config.editor.estimated_line_length;
        let state = self.active_state_mut();
        let cursor = state.cursors.primary().clone();
        let cursor_id = state.cursors.primary_id();

        // Get line information
        let mut iter = state
            .buffer
            .line_iterator(cursor.position, estimated_line_length);
        if let Some((line_start, line_content)) = iter.next() {
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

    /// Indent the selection or current line
    pub(super) fn indent_selection(&mut self) {
        let tab_size = self.config.editor.tab_size;
        let estimated_line_length = self.config.editor.estimated_line_length;
        let indent_str = " ".repeat(tab_size);

        let state = self.active_state_mut();
        // Collect lines to indent
        let cursor = state.cursors.primary().clone();
        let cursor_id = state.cursors.primary_id();

        let (start_pos, end_pos) = if let Some(range) = cursor.selection_range() {
            (range.start, range.end)
        } else {
            // No selection - indent current line
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

        // Collect all line starts by iterating through lines
        loop {
            if let Some((_, content)) = iter.next() {
                current_pos += content.len();
                if current_pos > end_pos || current_pos > buffer_len {
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
            } else {
                break;
            }
        }

        if line_starts.is_empty() {
            return;
        }

        // Create insert events for each line start (in reverse order)
        let mut events = Vec::new();
        for &line_start in line_starts.iter().rev() {
            events.push(Event::Insert {
                position: line_start,
                text: indent_str.clone(),
                cursor_id,
            });
        }

        let batch = Event::Batch {
            events,
            description: "Indent selection".to_string(),
        };

        self.active_event_log_mut().append(batch.clone());
        self.apply_event_to_active_buffer(&batch);
        self.set_status_message(format!("Indented {} line(s)", line_starts.len()));
    }

    /// Dedent the selection or current line
    pub(super) fn dedent_selection(&mut self) {
        let tab_size = self.config.editor.tab_size;
        let estimated_line_length = self.config.editor.estimated_line_length;

        let state = self.active_state_mut();
        // Collect lines to dedent
        let cursor = state.cursors.primary().clone();
        let cursor_id = state.cursors.primary_id();

        let (start_pos, end_pos) = if let Some(range) = cursor.selection_range() {
            (range.start, range.end)
        } else {
            // No selection - dedent current line
            let iter = state
                .buffer
                .line_iterator(cursor.position, estimated_line_length);
            let line_start = iter.current_position();
            (line_start, cursor.position)
        };

        // Find all line starts in the range (same logic as indent)
        let buffer_len = state.buffer.len();
        let mut line_starts = Vec::new();
        let mut iter = state.buffer.line_iterator(start_pos, estimated_line_length);
        let mut current_pos = iter.current_position();
        line_starts.push(current_pos);

        loop {
            if let Some((_, content)) = iter.next() {
                current_pos += content.len();
                if current_pos > end_pos || current_pos > buffer_len {
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
            } else {
                break;
            }
        }

        if line_starts.is_empty() {
            return;
        }

        // Create delete events for leading spaces (in reverse order)
        let mut events = Vec::new();
        let mut lines_dedented = 0;

        for &line_start in line_starts.iter().rev() {
            // Check how many leading spaces the line has
            let line_bytes = state
                .buffer
                .slice_bytes(line_start..buffer_len.min(line_start + tab_size + 1));
            let spaces_to_remove = line_bytes
                .iter()
                .take(tab_size)
                .take_while(|&&b| b == b' ')
                .count();

            if spaces_to_remove > 0 {
                let deleted_text = " ".repeat(spaces_to_remove);
                events.push(Event::Delete {
                    range: line_start..line_start + spaces_to_remove,
                    deleted_text,
                    cursor_id,
                });
                lines_dedented += 1;
            }
        }

        if events.is_empty() {
            self.set_status_message("No indentation to remove".to_string());
            return;
        }

        let batch = Event::Batch {
            events,
            description: "Dedent selection".to_string(),
        };

        self.active_event_log_mut().append(batch.clone());
        self.apply_event_to_active_buffer(&batch);
        self.set_status_message(format!("Dedented {} line(s)", lines_dedented));
    }

    /// Toggle comment on the current line or selection
    pub(super) fn toggle_comment(&mut self) {
        // Determine comment prefix based on file extension
        let comment_prefix = if let Some(metadata) = self.buffer_metadata.get(&self.active_buffer()) {
            if let Some(path) = metadata.file_path() {
                match path.extension().and_then(|e| e.to_str()) {
                    Some("rs") | Some("c") | Some("cpp") | Some("h") | Some("hpp") | Some("js")
                    | Some("ts") | Some("jsx") | Some("tsx") | Some("java") | Some("go")
                    | Some("swift") | Some("kt") | Some("scala") => "// ",
                    Some("py") | Some("rb") | Some("sh") | Some("bash") | Some("zsh")
                    | Some("pl") | Some("r") | Some("yml") | Some("yaml") | Some("toml") => "# ",
                    Some("lua") | Some("sql") => "-- ",
                    Some("html") | Some("xml") => "<!-- ",
                    Some("css") | Some("scss") | Some("sass") => "/* ",
                    Some("vim") => "\" ",
                    Some("lisp") | Some("el") | Some("clj") => ";; ",
                    _ => "// ",
                }
            } else {
                "// "
            }
        } else {
            "// "
        };

        let estimated_line_length = self.config.editor.estimated_line_length;

        let state = self.active_state_mut();
        let cursor = state.cursors.primary().clone();
        let cursor_id = state.cursors.primary_id();

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

        loop {
            if let Some((_, content)) = iter.next() {
                current_pos += content.len();
                if current_pos > end_pos || current_pos > buffer_len {
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
            } else {
                break;
            }
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
                    let remove_len = if rest.starts_with(comment_prefix) {
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
                }
            }
        } else {
            // Comment: add comment prefix to each line
            for &line_start in line_starts.iter().rev() {
                events.push(Event::Insert {
                    position: line_start,
                    text: comment_prefix.to_string(),
                    cursor_id,
                });
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
        let batch = Event::Batch {
            events,
            description: format!("{} lines", action_desc),
        };

        self.active_event_log_mut().append(batch.clone());
        self.apply_event_to_active_buffer(&batch);
        self.set_status_message(format!("{}ed {} line(s)", action_desc, line_starts.len()));
    }

    /// Go to matching bracket
    pub(super) fn goto_matching_bracket(&mut self) {
        let state = self.active_state_mut();
        let cursor = state.cursors.primary().clone();
        let cursor_id = state.cursors.primary_id();

        let pos = cursor.position;
        if pos >= state.buffer.len() {
            self.set_status_message("No bracket at cursor".to_string());
            return;
        }

        let bytes = state.buffer.slice_bytes(pos..pos + 1);
        if bytes.is_empty() {
            self.set_status_message("No bracket at cursor".to_string());
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
                self.set_status_message("No bracket at cursor".to_string());
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
            self.set_status_message("No matching bracket found".to_string());
        }
    }

    /// Jump to next error/diagnostic
    pub(super) fn jump_to_next_error(&mut self) {
        let diagnostic_ns = self.lsp_diagnostic_namespace.clone();
        let state = self.active_state_mut();
        let cursor_pos = state.cursors.primary().position;
        let cursor_id = state.cursors.primary_id();
        let cursor = state.cursors.primary().clone();

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
            self.set_status_message("No diagnostics in current buffer".to_string());
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
        let state = self.active_state_mut();
        let cursor_pos = state.cursors.primary().position;
        let cursor_id = state.cursors.primary_id();
        let cursor = state.cursors.primary().clone();

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
            self.set_status_message("No diagnostics in current buffer".to_string());
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
        self.set_status_message(format!(
            "Recording macro '{}' (press Ctrl+Shift+R {} to stop)",
            key, key
        ));
    }

    /// Stop recording and save the macro
    pub(super) fn stop_macro_recording(&mut self) {
        if let Some(state) = self.macro_recording.take() {
            let action_count = state.actions.len();
            let key = state.key;
            self.macros.insert(key, state.actions);
            self.last_macro_register = Some(key);
            self.set_status_message(format!("Macro '{}' saved ({} actions)", key, action_count));
        } else {
            self.set_status_message("Not recording a macro".to_string());
        }
    }

    /// Play back a recorded macro
    pub(super) fn play_macro(&mut self, key: char) {
        if let Some(actions) = self.macros.get(&key).cloned() {
            if actions.is_empty() {
                self.set_status_message(format!("Macro '{}' is empty", key));
                return;
            }

            // Temporarily disable recording to avoid recording the playback
            let was_recording = self.macro_recording.take();

            let action_count = actions.len();
            for action in actions {
                let _ = self.handle_action(action);
            }

            // Restore recording state
            self.macro_recording = was_recording;

            self.set_status_message(format!("Played macro '{}' ({} actions)", key, action_count));
        } else {
            self.set_status_message(format!("No macro recorded for '{}'", key));
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
                        self.set_status_message(format!("Failed to serialize macro: {}", e));
                        return;
                    }
                };
                (json, actions.len())
            }
            None => {
                self.set_status_message(format!("No macro recorded for '{}'", key));
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

        let state = EditorState::new(
            self.terminal_width.into(),
            self.terminal_height.into(),
            self.config.editor.large_file_threshold_bytes as usize,
        );

        self.buffers.insert(buffer_id, state);
        self.event_logs.insert(buffer_id, EventLog::new());

        // Set buffer content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer = crate::model::buffer::Buffer::from_str(
                &content,
                self.config.editor.large_file_threshold_bytes as usize,
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
        };
        self.buffer_metadata.insert(buffer_id, metadata);

        // Switch to the new buffer
        self.set_active_buffer(buffer_id);
        self.set_status_message(format!(
            "Macro '{}' shown in buffer ({} actions) - save as .json for persistence",
            key,
            actions_len
        ));
    }

    /// List all recorded macros in a buffer
    pub(super) fn list_macros_in_buffer(&mut self) {
        if self.macros.is_empty() {
            self.set_status_message("No macros recorded".to_string());
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

                // Show first few actions as preview
                for (i, action) in actions.iter().take(5).enumerate() {
                    content.push_str(&format!("  {}. {:?}\n", i + 1, action));
                }
                if actions.len() > 5 {
                    content.push_str(&format!("  ... and {} more actions\n", actions.len() - 5));
                }
                content.push('\n');
            }
        }

        // Create a new buffer for the macro list
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        let state = EditorState::new(
            self.terminal_width.into(),
            self.terminal_height.into(),
            self.config.editor.large_file_threshold_bytes as usize,
        );

        self.buffers.insert(buffer_id, state);
        self.event_logs.insert(buffer_id, EventLog::new());

        // Set buffer content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer = crate::model::buffer::Buffer::from_str(
                &content,
                self.config.editor.large_file_threshold_bytes as usize,
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
        };
        self.buffer_metadata.insert(buffer_id, metadata);

        // Switch to the new buffer
        self.set_active_buffer(buffer_id);
        self.set_status_message(format!("Showing {} recorded macro(s)", self.macros.len()));
    }

    /// Set a bookmark at the current position
    pub(super) fn set_bookmark(&mut self, key: char) {
        let buffer_id = self.active_buffer();
        let position = self.active_state().cursors.primary().position;
        self.bookmarks.insert(
            key,
            Bookmark {
                buffer_id,
                position,
            },
        );
        self.set_status_message(format!("Bookmark '{}' set", key));
    }

    /// Jump to a bookmark
    pub(super) fn jump_to_bookmark(&mut self, key: char) {
        if let Some(bookmark) = self.bookmarks.get(&key).cloned() {
            // Switch to the buffer if needed
            if bookmark.buffer_id != self.active_buffer() {
                if self.buffers.contains_key(&bookmark.buffer_id) {
                    self.set_active_buffer(bookmark.buffer_id);
                } else {
                    self.set_status_message(format!("Bookmark '{}': buffer no longer exists", key));
                    self.bookmarks.remove(&key);
                    return;
                }
            }

            // Move cursor to bookmark position
            let state = self.active_state_mut();
            let cursor_id = state.cursors.primary_id();
            let old_pos = state.cursors.primary().position;
            let new_pos = bookmark.position.min(state.buffer.len());

            let event = Event::MoveCursor {
                cursor_id,
                old_position: old_pos,
                new_position: new_pos,
                old_anchor: state.cursors.primary().anchor,
                new_anchor: None,
                old_sticky_column: state.cursors.primary().sticky_column,
                new_sticky_column: 0,
            };

            self.active_event_log_mut().append(event.clone());
            self.apply_event_to_active_buffer(&event);
            self.set_status_message(format!("Jumped to bookmark '{}'", key));
        } else {
            self.set_status_message(format!("Bookmark '{}' not set", key));
        }
    }

    /// Clear a bookmark
    pub(super) fn clear_bookmark(&mut self, key: char) {
        if self.bookmarks.remove(&key).is_some() {
            self.set_status_message(format!("Bookmark '{}' cleared", key));
        } else {
            self.set_status_message(format!("Bookmark '{}' not set", key));
        }
    }

    /// List all bookmarks
    pub(super) fn list_bookmarks(&mut self) {
        if self.bookmarks.is_empty() {
            self.set_status_message("No bookmarks set".to_string());
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

        self.set_status_message(format!("Bookmarks: {}", list_str));
    }

    /// Clear the search history
    /// Used primarily for testing to ensure test isolation
    pub fn clear_search_history(&mut self) {
        self.search_history.clear();
    }

    /// Save search and replace histories to disk
    /// Called on shutdown to persist history across sessions
    pub fn save_histories(&self) {
        // Ensure data directory exists
        if let Err(e) = std::fs::create_dir_all(&self.dir_context.data_dir) {
            tracing::warn!("Failed to create data directory: {}", e);
            return;
        }

        // Save search history
        let search_path = self.dir_context.search_history_path();
        if let Err(e) = self.search_history.save_to_file(&search_path) {
            tracing::warn!("Failed to save search history: {}", e);
        } else {
            tracing::debug!("Saved search history to {:?}", search_path);
        }

        // Save replace history
        let replace_path = self.dir_context.replace_history_path();
        if let Err(e) = self.replace_history.save_to_file(&replace_path) {
            tracing::warn!("Failed to save replace history: {}", e);
        } else {
            tracing::debug!("Saved replace history to {:?}", replace_path);
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
        let Some(view_state) = self.split_view_states.get_mut(&split_id) else {
            return;
        };

        let split_buffers = &view_state.open_buffers;
        let buffers = &self.buffers;
        let buffer_metadata = &self.buffer_metadata;
        // The theme is not strictly necessary here, but passed to TabsRenderer
        // so we'll just use a dummy default style for width calculation

        // Calculate widths of tabs (and separators)
        let mut tab_layout_info: Vec<(usize, bool)> = Vec::new();
        for (idx, id) in split_buffers.iter().enumerate() {
            let Some(state) = buffers.get(id) else {
                continue;
            };

            let name = if let Some(metadata) = buffer_metadata.get(id) {
                metadata.display_name.as_str()
            } else {
                state
                    .buffer
                    .file_path()
                    .and_then(|p| p.file_name())
                    .and_then(|n| n.to_str())
                    .unwrap_or("[No Name]")
            };

            let modified_indicator_width = if state.buffer.is_modified() { 1 } else { 0 };
            let tab_width = 2 + name.chars().count() + modified_indicator_width; // " {name}{modified} "
            let is_active = *id == active_buffer;

            tab_layout_info.push((tab_width, is_active));
            if idx < split_buffers.len() - 1 {
                tab_layout_info.push((1, false)); // separator
            }
        }

        let total_tabs_width: usize = tab_layout_info.iter().map(|(w, _)| w).sum();
        let max_visible_width = available_width as usize;

        let tab_widths: Vec<usize> = tab_layout_info.iter().map(|(w, _)| *w).collect();
        let active_tab_index = tab_layout_info.iter().position(|(_, is_active)| *is_active);

        let new_scroll_offset = if let Some(idx) = active_tab_index {
            crate::view::ui::tabs::compute_tab_scroll_offset(
                &tab_widths,
                idx,
                max_visible_width,
                view_state.tab_scroll_offset,
                1, // separator width
            )
        } else {
            view_state
                .tab_scroll_offset
                .min(total_tabs_width.saturating_sub(max_visible_width))
        };

        view_state.tab_scroll_offset = new_scroll_offset;
    }
}
