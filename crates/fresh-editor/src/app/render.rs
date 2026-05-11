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
        self.active_window_mut().animations.capture_before_all();

        // Save frame dimensions for recompute_layout (used by macro replay)
        self.active_chrome_mut().last_frame_width = size.width;
        self.active_chrome_mut().last_frame_height = size.height;

        // Reset per-cell theme key map for this frame
        self.active_chrome_mut().reset_cell_theme_map();

        // For scroll sync groups, we need to update the active split's viewport position BEFORE
        // calling sync_scroll_groups, so that the sync reads the correct position.
        // Otherwise, cursor movements like 'G' (go to end) won't sync properly because
        // viewport.top_byte hasn't been updated yet.
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.splits.as_ref())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        {
            let _span = tracing::info_span!("pre_sync_ensure_visible").entered();
            self.active_window_mut()
                .pre_sync_ensure_visible(active_split);
        }

        // Synchronize scroll sync groups (anchor-based scroll for side-by-side diffs)
        // This sets viewport positions based on the authoritative scroll_line in each group
        {
            let _span = tracing::info_span!("sync_scroll_groups").entered();
            self.active_window_mut().sync_scroll_groups();
        }

        // NOTE: Viewport sync with cursor is handled by split_rendering.rs which knows the
        // correct content area dimensions. Don't sync here with incorrect EditorState viewport size.

        // Prepare all buffers for rendering (pre-load viewport data for lazy loading)
        // Each split may have a different viewport position on the same buffer
        let mut semantic_ranges: std::collections::HashMap<BufferId, (usize, usize)> =
            std::collections::HashMap::new();
        {
            let _span = tracing::info_span!("compute_semantic_ranges").entered();
            for (split_id, view_state) in self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.splits.as_ref())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
            {
                if let Some(buffer_id) = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.splits.as_ref())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .get_buffer_id((*split_id).into())
                {
                    if let Some(state) = self
                        .windows
                        .get(&self.active_window)
                        .map(|w| &w.buffers)
                        .expect("active window present")
                        .get(&buffer_id)
                    {
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
            // Pre-collect (split_id, top_byte, height, buffer_id) so we
            // can mutate buffers below without holding a read borrow on
            // self.windows.
            let active_id = self.active_window;
            let prep_targets: Vec<(BufferId, usize, u16)> = {
                let win = self
                    .windows
                    .get(&active_id)
                    .expect("active window must exist");
                let (mgr, vs_map) = win
                    .splits
                    .as_ref()
                    .expect("active window must have a populated split layout");
                vs_map
                    .iter()
                    .filter_map(|(split_id, vs)| {
                        mgr.get_buffer_id((*split_id).into())
                            .map(|bid| (bid, vs.viewport.top_byte, vs.viewport.height))
                    })
                    .collect()
            };
            let win_buffers = &mut self
                .windows
                .get_mut(&active_id)
                .expect("active window must exist")
                .buffers;
            for (buffer_id, top_byte, height) in prep_targets {
                if let Some(state) = win_buffers.get_mut(&buffer_id) {
                    if let Err(e) = state.prepare_for_render(top_byte, height) {
                        tracing::error!("Failed to prepare buffer for render: {}", e);
                    }
                }
            }
        }

        // Refresh search highlights only during incremental search (when prompt is active)
        // After search is confirmed, overlays exist for ALL matches and shouldn't be overwritten
        let is_search_prompt_active = self.active_window().prompt.as_ref().is_some_and(|p| {
            matches!(
                p.prompt_type,
                PromptType::Search | PromptType::ReplaceSearch | PromptType::QueryReplaceSearch
            )
        });
        if is_search_prompt_active {
            if let Some(ref search_state) = self.active_window().search_state {
                let query = search_state.query.clone();
                self.update_search_highlights(&query);
            }
        }

        // Determine if we need to show search options bar.
        // (Held in mutable bindings because the in-render
        // `process_commands` block below can dispatch commands —
        // e.g. `StartPromptAsync`, `SetPromptSuggestions` — that
        // mutate `self.active_window_mut().prompt`. When that happens we recompute these
        // flags and re-split `main_chunks` so the bottom-row
        // rendering uses an up-to-date layout. See the
        // "Recompute layout if mid-render commands changed state"
        // block below.)
        let mut show_search_options = self.active_window().prompt.as_ref().is_some_and(|p| {
            matches!(
                p.prompt_type,
                PromptType::Search
                    | PromptType::ReplaceSearch
                    | PromptType::Replace { .. }
                    | PromptType::QueryReplaceSearch
                    | PromptType::QueryReplace { .. }
            )
        });

        // Hide status bar when suggestions popup or file browser
        // popup is shown — those popups float just above the prompt
        // line, and a visible status bar wedged between them looks
        // wrong. Floating-overlay prompts (Live Grep, issue #1796)
        // are exempt because their suggestions live inside the
        // centred frame, not above the bottom row.
        let mut prompt_is_overlay = self
            .active_window()
            .prompt
            .as_ref()
            .is_some_and(|p| p.overlay);
        let mut has_suggestions = self
            .active_window()
            .prompt
            .as_ref()
            .is_some_and(|p| !p.suggestions.is_empty())
            && !prompt_is_overlay;
        let mut has_file_browser = self.active_window().prompt.as_ref().is_some_and(|p| {
            matches!(
                p.prompt_type,
                PromptType::OpenFile | PromptType::SwitchProject | PromptType::SaveFileAs
            )
        }) && self.active_window_mut().file_open_state.is_some();

        // Build main vertical layout: [menu_bar, main_content, status_bar, search_options, prompt_line]
        // Status bar is hidden when suggestions popup is shown
        // Search options bar is shown when in search prompt
        let mut main_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![
                Constraint::Length(if self.active_window_mut().menu_bar_visible {
                    1
                } else {
                    0
                }), // Menu bar
                Constraint::Min(0), // Main content area
                Constraint::Length(
                    if !self.active_window_mut().status_bar_visible
                        || has_suggestions
                        || has_file_browser
                    {
                        0
                    } else {
                        1
                    },
                ), // Status bar (hidden when toggled off or with popups)
                Constraint::Length(if show_search_options { 1 } else { 0 }), // Search options bar
                Constraint::Length(
                    // Prompt line is auto-hidden when no prompt active.
                    // Overlay prompts (Live Grep, issue #1796) host the
                    // input row inside the centred frame, so the
                    // bottom row stays available for editor content
                    // rather than being reserved as dead space.
                    if (self.active_window_mut().prompt_line_visible
                        || self.active_window().prompt.is_some())
                        && !prompt_is_overlay
                    {
                        1
                    } else {
                        0
                    },
                ), // Prompt line
            ])
            .split(size);

        let menu_bar_area = main_chunks[0];
        let main_content_area = main_chunks[1];
        let status_bar_idx = 2;
        let search_options_idx = 3;
        let prompt_line_idx = 4;

        // Split main content area based on file explorer visibility
        // Also keep the layout split if a sync is in progress (to avoid flicker)
        let editor_content_area;
        let file_explorer_should_show = self.file_explorer_visible()
            && (self.file_explorer().is_some()
                || self.active_window().file_explorer_sync_in_progress);

        if file_explorer_should_show {
            // Split horizontally based on side placement
            tracing::trace!(
                "render: file explorer layout active (present={}, sync_in_progress={}, side={:?})",
                self.file_explorer().is_some(),
                self.active_window().file_explorer_sync_in_progress,
                self.active_window().file_explorer_side
            );
            let explorer_cols = self
                .active_window()
                .file_explorer_width
                .to_cols(main_content_area.width);

            let (explorer_area, editor_area) = match self.active_window().file_explorer_side {
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

            self.active_layout_mut().file_explorer_area = Some(explorer_area);
            editor_content_area = editor_area;

            // Get connection string before mutable borrow of file_explorer.
            let remote_connection = self.connection_display_string();

            // Render file explorer (only if we have it - during sync we just keep the area reserved).
            // Uses direct `self.windows.get_mut(...)` (not `file_explorer_mut()`) so the body
            // can keep reading other Editor fields (buffers, theme, keybindings, …) — Rust
            // splits the borrow on `self.windows` from the borrows on those other fields.
            let active_id = self.active_window;
            // Read window-state inputs before taking the &mut borrow on the
            // window for the explorer/buffer access below.
            let is_focused = self.active_window().key_context == KeyContext::FileExplorer;
            let key_context_clone = self.active_window().key_context.clone();
            let close_button_hovered = matches!(
                &self.active_window().mouse_state.hover_target,
                Some(HoverTarget::FileExplorerCloseButton)
            );
            // Take one &mut on the active window; the explorer + buffers
            // come from disjoint sub-fields so they can coexist.
            let __win = self
                .windows
                .get_mut(&active_id)
                .expect("active window must exist");
            let __buffers_ref: &HashMap<BufferId, EditorState> = &__win.buffers;
            if let Some(explorer) = __win.file_explorer.as_mut() {
                // Build set of files with unsaved changes
                let mut files_with_unsaved_changes = std::collections::HashSet::new();
                for (buffer_id, state) in __buffers_ref {
                    if state.buffer.is_modified() {
                        if let Some(metadata) = __win.buffer_metadata.get(buffer_id) {
                            if let Some(file_path) = metadata.file_path() {
                                files_with_unsaved_changes.insert(file_path.clone());
                            }
                        }
                    }
                }

                let keybindings = self.keybindings.read().unwrap();
                let empty: Vec<std::path::PathBuf> = Vec::new();
                let cut_paths = __win
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
                    &__win.file_explorer_decoration_cache,
                    &keybindings,
                    key_context_clone,
                    &*self.theme.read().unwrap(),
                    close_button_hovered,
                    remote_connection.as_deref(),
                    cut_paths,
                );
            }
            // Note: if file_explorer is None but sync_in_progress is true,
            // we just leave the area blank (or could render a placeholder)
        } else {
            // No file explorer: use entire main content area for editor
            self.active_layout_mut().file_explorer_area = None;
            editor_content_area = main_content_area;
        }

        // Note: Tabs are now rendered within each split by SplitRenderer

        // Trigger lines_changed hooks for newly visible lines in all visible buffers
        // This allows plugins to add overlays before rendering
        // Only lines that haven't been seen before are sent (batched for efficiency)
        // Use non-blocking hooks to avoid deadlock when actions are awaiting
        if self.plugin_manager.read().unwrap().is_active() {
            let hooks_start = std::time::Instant::now();
            // Get visible buffers and their areas
            let visible_buffers = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.splits.as_ref())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .get_visible_buffers(editor_content_area);

            let mut total_new_lines = 0usize;
            for (split_id, buffer_id, split_area) in visible_buffers {
                // Get viewport from SplitViewState (the authoritative source)
                let viewport_top_byte = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.splits.as_ref())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .get(&split_id)
                    .map(|vs| vs.viewport.top_byte)
                    .unwrap_or(0);

                let __active_id = self.active_window;
                let __win = self
                    .windows
                    .get_mut(&__active_id)
                    .expect("active window must exist");
                if let Some(state) = __win.buffers.get_mut(&buffer_id) {
                    // Fire render_start hook once per buffer
                    self.plugin_manager.read().unwrap().run_hook(
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
                    let __vs_map = &mut __win
                        .splits
                        .as_mut()
                        .expect("active window must have a populated split layout")
                        .1;
                    let cursor_positions: Vec<usize> = __vs_map
                        .get(&split_id)
                        .map(|vs| vs.cursors.iter().map(|(_, c)| c.position).collect())
                        .unwrap_or_default();
                    self.plugin_manager.read().unwrap().run_hook(
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
                    if let Some(vs) = __vs_map.get_mut(&split_id) {
                        vs.view_transform_stale = false;
                    }

                    // Use the split area height as visible line count
                    let visible_count = split_area.height as usize;
                    let top_byte = viewport_top_byte;

                    // Get or create the seen byte ranges set for this buffer.
                    // Use direct __win field access (not active_window_mut())
                    // because __win is already held above; re-locking via the
                    // accessor would re-borrow self.windows.
                    let seen_byte_ranges = __win.seen_byte_ranges.entry(buffer_id).or_default();

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
                        self.plugin_manager.read().unwrap().run_hook(
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
            let commands = self.plugin_manager.write().unwrap().process_commands();
            let dispatched_any = !commands.is_empty();
            if dispatched_any {
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

            // Recompute the bottom-row layout if the in-render command
            // dispatch above mutated state that affects it. Without
            // this, a `StartPromptAsync` (or similar) processed
            // mid-render leaves `main_chunks` reflecting the prior
            // `self.active_window_mut().prompt = None` shape — the prompt slot ends up at
            // (y = size.height, h = 0) and the status bar paints the
            // bottom row in place of the prompt input. Conservative:
            // we recompute on *any* dispatched commands rather than
            // enumerating layout-affecting variants — Layout::split is
            // cheap, and this avoids a maintenance-burden whitelist
            // that would silently regress as new `PluginCommand`
            // variants are added.
            //
            // Bounded — single drain + single recompute. We do not
            // call `process_commands` again, so commands queued by
            // hooks fired inside the dispatch above wait for the next
            // render or `editor_tick` (the existing one-frame-late
            // behaviour the comment above already accepts).
            //
            // `main_content_area` (and the file-explorer / split
            // rendering derived from it earlier in this render) is
            // intentionally NOT re-derived: those areas were already
            // painted, and the bottom-row recompute may overwrite a
            // single row of main content where the new status bar /
            // prompt now sits. That brief overlap self-corrects on
            // the next frame, where the layout is built consistently
            // from the start.
            if dispatched_any {
                show_search_options = self.active_window().prompt.as_ref().is_some_and(|p| {
                    matches!(
                        p.prompt_type,
                        PromptType::Search
                            | PromptType::ReplaceSearch
                            | PromptType::Replace { .. }
                            | PromptType::QueryReplaceSearch
                            | PromptType::QueryReplace { .. }
                    )
                });
                prompt_is_overlay = self
                    .active_window()
                    .prompt
                    .as_ref()
                    .is_some_and(|p| p.overlay);
                has_suggestions = self
                    .active_window()
                    .prompt
                    .as_ref()
                    .is_some_and(|p| !p.suggestions.is_empty())
                    && !prompt_is_overlay;
                has_file_browser = self.active_window().prompt.as_ref().is_some_and(|p| {
                    matches!(
                        p.prompt_type,
                        PromptType::OpenFile | PromptType::SwitchProject | PromptType::SaveFileAs
                    )
                }) && self.active_window_mut().file_open_state.is_some();
                main_chunks = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints(vec![
                        Constraint::Length(if self.active_window_mut().menu_bar_visible {
                            1
                        } else {
                            0
                        }),
                        Constraint::Min(0),
                        Constraint::Length(
                            if !self.active_window_mut().status_bar_visible
                                || has_suggestions
                                || has_file_browser
                            {
                                0
                            } else {
                                1
                            },
                        ),
                        Constraint::Length(if show_search_options { 1 } else { 0 }),
                        Constraint::Length(
                            if (self.active_window_mut().prompt_line_visible
                                || self.active_window().prompt.is_some())
                                && !prompt_is_overlay
                            {
                                1
                            } else {
                                0
                            },
                        ),
                    ])
                    .split(size);
            }
        }

        // Render editor content (same for both layouts)
        let lsp_waiting = !self.active_window().pending_completion_requests.is_empty()
            || self
                .active_window()
                .pending_goto_definition_request
                .is_some();

        // Hide the hardware cursor when menu is open, file explorer is focused, terminal mode,
        // or settings UI is open
        // (the file explorer will set its own cursor position when focused)
        // (terminal mode renders its own cursor via the terminal emulator)
        // (settings UI is a modal that doesn't need the editor cursor)
        // This also causes visual cursor indicators in the editor to be dimmed
        let settings_visible = self.settings_state.as_ref().is_some_and(|s| s.visible);
        let hide_cursor = self.menu_state.active_menu.is_some()
            || self.active_window_mut().key_context == KeyContext::FileExplorer
            || self.active_window().terminal_mode
            || settings_visible
            || self.keybinding_editor.is_some();

        // Convert HoverTarget to tab hover info for rendering
        let hovered_tab = match &self.active_window_mut().mouse_state.hover_target {
            Some(HoverTarget::TabName(target, split_id)) => Some((*target, *split_id, false)),
            Some(HoverTarget::TabCloseButton(target, split_id)) => Some((*target, *split_id, true)),
            _ => None,
        };

        // Get hovered close split button
        let hovered_close_split = match &self.active_window_mut().mouse_state.hover_target {
            Some(HoverTarget::CloseSplitButton(split_id)) => Some(*split_id),
            _ => None,
        };

        // Get hovered maximize split button
        let hovered_maximize_split = match &self.active_window_mut().mouse_state.hover_target {
            Some(HoverTarget::MaximizeSplitButton(split_id)) => Some(*split_id),
            _ => None,
        };

        let is_maximized = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.splits.as_ref())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .is_maximized();

        // The active split's buffer renderer records where the hardware
        // cursor *wants* to appear here; we only commit it to the frame at
        // the very end of this draw pass, after popups have been rendered,
        // so a popup covering the cursor cell causes the cursor to be
        // hidden (otherwise the hardware caret would bleed through the
        // popup).
        let mut pending_hardware_cursor: Option<(u16, u16)> = None;

        let _content_span = tracing::info_span!("render_content").entered();
        // Take a single mutable borrow on the active window's splits and
        // split it into (&SplitManager, &mut HashMap<...>) — Rust can
        // destructure the tuple, but we can't make two separate
        // `windows.get`/`windows.get_mut` calls in the same expression.
        let active_window_id = self.active_window;
        // Take one &mut on the active window. Split-borrow into
        // buffers (mut), split_mgr (immutable view of mgr), and
        // split_view_states (mut) — all disjoint sub-fields.
        let __win = self
            .windows
            .get_mut(&active_window_id)
            .expect("active window must exist");
        let __buffers_mut = &mut __win.buffers;
        let __metadata_ref = &__win.buffer_metadata;
        let __event_logs_mut = &mut __win.event_logs;
        let __grouped_ref = &__win.grouped_subtrees;
        let __composite_buffers_mut = &mut __win.composite_buffers;
        let __composite_view_states_mut = &mut __win.composite_view_states;
        let __cell_theme_map_mut = &mut __win.chrome_layout.cell_theme_map;
        let (mgr_for_split, split_view_states): (
            &crate::view::split::SplitManager,
            &mut HashMap<crate::model::event::LeafId, crate::view::split::SplitViewState>,
        ) = {
            let (m, vs) = __win
                .splits
                .as_mut()
                .expect("active window must have a populated split layout");
            (&*m, vs)
        };
        let split_mgr = mgr_for_split;
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
            split_mgr,
            __buffers_mut,
            __metadata_ref,
            __event_logs_mut,
            __composite_buffers_mut,
            __composite_view_states_mut,
            &*self.theme.read().unwrap(),
            self.ansi_background.as_ref(),
            self.background_fade,
            lsp_waiting,
            self.config.editor.large_file_threshold_bytes,
            self.config.editor.line_wrap,
            self.config.editor.estimated_line_length,
            self.config.editor.highlight_context_bytes,
            Some(split_view_states),
            __grouped_ref,
            hide_cursor,
            hovered_tab,
            hovered_close_split,
            hovered_maximize_split,
            is_maximized,
            self.config.editor.relative_line_numbers,
            __win.tab_bar_visible,
            self.config.editor.use_terminal_bg,
            self.session_mode || !self.software_cursor_only,
            self.software_cursor_only,
            self.config.editor.show_vertical_scrollbar,
            self.config.editor.show_horizontal_scrollbar,
            self.config.editor.diagnostics_inline_text,
            self.config.editor.show_tilde,
            self.config.editor.highlight_current_column,
            __cell_theme_map_mut,
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
        // Compare against previous frame's viewport state (stored in self.active_window().previous_viewports)
        // This correctly detects changes from scroll events that happen before render()
        if self.plugin_manager.read().unwrap().is_active() {
            for (split_id, view_state) in self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.splits.as_ref())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
            {
                let current = (
                    view_state.viewport.top_byte,
                    view_state.viewport.width,
                    view_state.viewport.height,
                );
                // Compare against previous frame's state
                // Skip new splits (None case) - only fire hooks for established splits
                // This matches the original behavior where hooks only fire for splits
                // that existed at the start of render
                let (changed, previous) =
                    match self.active_window().previous_viewports.get(split_id) {
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
                    if let Some(buffer_id) = self
                        .windows
                        .get(&self.active_window)
                        .and_then(|w| w.splits.as_ref())
                        .map(|(mgr, _)| mgr)
                        .expect("active window must have a populated split layout")
                        .get_buffer_id((*split_id).into())
                    {
                        // Compute top_line if line info is available
                        let top_line = self
                            .windows
                            .get(&self.active_window)
                            .map(|w| &w.buffers)
                            .expect("active window present")
                            .get(&buffer_id)
                            .and_then(|state| {
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
                        self.plugin_manager.read().unwrap().run_hook(
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

        // Update previous_viewports for next frame's comparison.
        // Take both `previous_viewports` and the split view-states from
        // the same `__win` borrow so the iterator and the inserts share
        // a single mutable borrow on `self.windows`.
        let __vp_win = self
            .windows
            .get_mut(&self.active_window)
            .expect("active window present");
        __vp_win.previous_viewports.clear();
        let (_, __vp_vs_map) = __vp_win
            .splits
            .as_ref()
            .expect("active window must have a populated split layout");
        let snapshot: Vec<(LeafId, (usize, u16, u16))> = __vp_vs_map
            .iter()
            .map(|(split_id, view_state)| {
                (
                    *split_id,
                    (
                        view_state.viewport.top_byte,
                        view_state.viewport.width,
                        view_state.viewport.height,
                    ),
                )
            })
            .collect();
        for (split_id, vp) in snapshot {
            __vp_win.previous_viewports.insert(split_id, vp);
        }

        // Render terminal content on top of split content for terminal buffers
        self.render_terminal_splits(frame, &split_areas);

        self.active_layout_mut().split_areas = split_areas;
        self.active_layout_mut().horizontal_scrollbar_areas = horizontal_scrollbar_areas;
        self.active_layout_mut().tab_layouts = tab_layouts;
        self.active_layout_mut().close_split_areas = close_split_areas;
        self.active_layout_mut().maximize_split_areas = maximize_split_areas;
        self.active_layout_mut().view_line_mappings = view_line_mappings;

        // Promote any deferred virtual-buffer animations whose Rect is now
        // known. Done here (after split_areas is recomputed, before
        // apply_all runs at the end of render) so the first frame of the
        // effect lands on the same paint that made the buffer visible.
        self.drain_pending_vb_animations();
        let mut separator_areas = self
            .split_manager_mut()
            .get_separators_with_ids(editor_content_area);
        // Grouped subtrees live in a side-map outside the main split tree, so
        // their inner separators are not visited by `get_separators_with_ids`
        // above. The renderer collected them (using the same content rect it
        // drew them at) — merge so clicks on those rendered columns register.
        separator_areas.extend(grouped_separator_areas);
        self.active_layout_mut().separator_areas = separator_areas;
        self.active_layout_mut().editor_content_area = Some(editor_content_area);

        // Render hover highlights for separators and scrollbars
        self.render_hover_highlights(frame);

        // Initialize popup/suggestion layout state (rendered after status bar below)
        self.active_chrome_mut().suggestions_area = None;
        self.active_chrome_mut().suggestions_outer_area = None;
        self.active_window_mut().file_browser_layout = None;

        // Clone all immutable values before the mutable borrow
        let display_name = self
            .active_window()
            .buffer_metadata
            .get(&self.active_buffer())
            .map(|m| m.display_name.clone())
            .unwrap_or_else(|| "[No Name]".to_string());

        // Reflect the active buffer in the terminal window/tab title. Only
        // writes when the title actually changes so we don't flood stdout
        // with OSC sequences every frame.
        self.update_terminal_title(&display_name);

        let status_message = self.active_window().status_message.clone();
        let plugin_status_message = self.active_window().plugin_status_message.clone();
        let prompt = self.active_window().prompt.clone();
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
            .buffers()
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
            .unwrap_or_default();
        let buffer_lsp_disabled_reason = self
            .active_window()
            .buffer_metadata
            .get(&self.active_buffer())
            .filter(|m| !m.lsp_enabled)
            .and_then(|m| m.lsp_disabled_reason.as_deref());
        let (lsp_status, lsp_indicator_state) = compose_lsp_status(
            &current_language,
            buffer_lsp_disabled_reason,
            &self.active_window().lsp_progress,
            &self.active_window().lsp_server_statuses,
            &self.config.lsp,
            &self.active_window().user_dismissed_lsp_languages,
        );
        let theme = self.theme.read().unwrap().clone();
        let keybindings_cloned = self.keybindings.read().unwrap().clone(); // Clone the keybindings
        let chord_state_cloned = self.active_window_mut().chord_state.clone(); // Clone the chord state

        // Get update availability info
        let update_available = self.latest_version().map(|v| v.to_string());

        // Render status bar (hidden when toggled off, or when suggestions/file browser popup is shown)
        if self.active_window_mut().status_bar_visible && !has_suggestions && !has_file_browser {
            // Get warning level for colored indicator (respects config setting)
            // LSP warning level is scoped to the current buffer's language
            let (warning_level, general_warning_count) =
                if self.config.warnings.show_status_indicator {
                    let lsp_level = {
                        use crate::services::async_bridge::LspServerStatus;
                        let mut level = WarningLevel::None;
                        for ((lang, _), status) in &self.active_window().lsp_server_statuses {
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
                    (
                        lsp_level,
                        self.active_window().warning_domains.general.count,
                    )
                } else {
                    (WarningLevel::None, 0)
                };

            // Compute status bar hover state for styling
            use crate::view::ui::status_bar::StatusBarHover;
            let status_bar_hover = match &self.active_window_mut().mouse_state.hover_target {
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
            let is_read_only = self
                .active_window()
                .buffer_metadata
                .get(&active_buf)
                .map(|m| m.read_only)
                .unwrap_or(false);
            let is_synthetic_placeholder = self
                .active_window()
                .buffer_metadata
                .get(&active_buf)
                .map(|m| m.synthetic_placeholder)
                .unwrap_or(false);
            // Single window borrow, split into buffers + cursors so the
            // status-bar context can hold both.
            let __active_id = self.active_window;
            let __win = self
                .windows
                .get_mut(&__active_id)
                .expect("active window must exist");
            let __state = __win.buffers.get_mut(&active_buf).unwrap();
            let status_cursors = __win
                .splits
                .as_ref()
                .map(|(_, vs)| vs)
                .and_then(|vs| vs.get(&active_split))
                .map(|vs| &vs.cursors)
                .unwrap_or(&default_cursors);
            let mut status_ctx = crate::view::ui::status_bar::StatusBarContext {
                state: __state,
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
            self.active_chrome_mut().status_bar_area =
                Some((status_bar_area.y, status_bar_area.x, status_bar_area.width));
            self.active_chrome_mut().status_bar_lsp_area = status_bar_layout.lsp_indicator;
            self.active_chrome_mut().status_bar_warning_area = status_bar_layout.warning_badge;
            self.active_chrome_mut().status_bar_line_ending_area =
                status_bar_layout.line_ending_indicator;
            self.active_chrome_mut().status_bar_encoding_area =
                status_bar_layout.encoding_indicator;
            self.active_chrome_mut().status_bar_language_area =
                status_bar_layout.language_indicator;
            self.active_chrome_mut().status_bar_message_area = status_bar_layout.message_area;
            self.active_chrome_mut().status_bar_remote_area = status_bar_layout.remote_indicator;
        }

        // Render search options bar when in search prompt
        if show_search_options {
            // Show "Confirm" option only in replace modes
            let confirm_each = self.active_window().prompt.as_ref().and_then(|p| {
                if matches!(
                    p.prompt_type,
                    PromptType::ReplaceSearch
                        | PromptType::Replace { .. }
                        | PromptType::QueryReplaceSearch
                        | PromptType::QueryReplace { .. }
                ) {
                    Some(self.active_window().search_confirm_each)
                } else {
                    None
                }
            });

            // Determine hover state for search options
            use crate::view::ui::status_bar::SearchOptionsHover;
            let search_options_hover = match &self.active_window_mut().mouse_state.hover_target {
                Some(HoverTarget::SearchOptionCaseSensitive) => SearchOptionsHover::CaseSensitive,
                Some(HoverTarget::SearchOptionWholeWord) => SearchOptionsHover::WholeWord,
                Some(HoverTarget::SearchOptionRegex) => SearchOptionsHover::Regex,
                Some(HoverTarget::SearchOptionConfirmEach) => SearchOptionsHover::ConfirmEach,
                _ => SearchOptionsHover::None,
            };

            let search_options_layout = StatusBarRenderer::render_search_options(
                frame,
                main_chunks[search_options_idx],
                self.active_window().search_case_sensitive,
                self.active_window().search_whole_word,
                self.active_window().search_use_regex,
                confirm_each,
                &theme,
                &keybindings_cloned,
                search_options_hover,
            );
            self.active_chrome_mut().search_options_layout = Some(search_options_layout);
        } else {
            self.active_chrome_mut().search_options_layout = None;
        }

        // Render prompt line if active. Overlay prompts (Live Grep)
        // skip the bottom-row render entirely — they paint their own
        // input row inside the centred overlay frame, so the user's
        // editor view stays unobstructed at the bottom.
        if let Some(prompt) = &prompt {
            if !prompt.overlay {
                // Use specialized renderer for file/folder open prompt to show colorized path
                if matches!(
                    prompt.prompt_type,
                    crate::view::prompt::PromptType::OpenFile
                        | crate::view::prompt::PromptType::SwitchProject
                ) {
                    if let Some(file_open_state) = &self.active_window_mut().file_open_state {
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
        }

        // Float-overlay preview: load the selected match's file (if
        // the file changed) and seed the phantom leaf's cursor before
        // the renderer reaches it. Done before render_prompt_popups
        // because that path immediately needs the leaf's view state.
        if self
            .active_window()
            .prompt
            .as_ref()
            .is_some_and(|p| p.overlay)
        {
            self.prepare_overlay_preview();
        }

        // Render file browser popup or suggestions popup AFTER status bar + prompt,
        // so they overlay on top of both (fixes bottom border being overwritten by status bar)
        self.render_prompt_popups(frame, main_chunks[prompt_line_idx], size.width);

        // Render popups from the active buffer state
        // Clone theme to avoid borrow checker issues with active_state_mut()
        let theme_clone = self.theme.read().unwrap().clone();
        let hover_target = self.active_window_mut().mouse_state.hover_target.clone();

        // Clear popup areas and recalculate
        self.active_chrome_mut().popup_areas.clear();

        // Collect popup information without holding a mutable borrow
        let popup_info: Vec<_> = {
            // Get viewport from active split's SplitViewState
            let active_split = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.splits.as_ref())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .active_split();
            let viewport = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.splits.as_ref())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
                .get(&active_split)
                .map(|vs| vs.viewport.clone());

            // Get the content_rect for the active split from the cached layout.
            // This is the absolute screen rect (already accounts for file explorer,
            // tab bar, scrollbars, etc.). The gutter is rendered inside this rect,
            // so we add gutter_width to get the text content origin.
            let content_rect = self
                .active_layout()
                .split_areas
                .iter()
                .find(|(split_id, _, _, _, _, _)| *split_id == active_split)
                .map(|(_, _, rect, _, _, _)| *rect);

            let primary_cursor = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.splits.as_ref())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
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
        self.active_chrome_mut().popup_areas = popup_info.clone();

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
        self.active_chrome_mut().global_popup_areas.clear();
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
            let scroll_offset = popup.scroll_offset;
            popup.render_with_hover(frame, popup_area, &theme_clone, hover_target.as_ref());
            self.active_chrome_mut().global_popup_areas.push((
                top_idx,
                popup_area,
                inner_area,
                scroll_offset,
                num_items,
            ));
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
                    &*self.theme.read().unwrap(),
                );
                self.active_chrome_mut().settings_layout = Some(settings_layout);
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
                &*self.theme.read().unwrap(),
            );
        }

        // Render keybinding editor if active
        if let Some(ref mut kb_editor) = self.keybinding_editor {
            crate::view::dimming::apply_dimming(frame, size);
            crate::view::keybinding_editor::render_keybinding_editor(
                frame,
                size,
                kb_editor,
                &*self.theme.read().unwrap(),
            );
        }

        // Render event debug dialog if active
        if let Some(ref debug) = self.event_debug {
            // Dim the editor content behind the dialog modal
            crate::view::dimming::apply_dimming(frame, size);
            crate::view::event_debug::render_event_debug(
                frame,
                size,
                debug,
                &*self.theme.read().unwrap(),
            );
        }

        if self.active_window_mut().menu_bar_visible {
            // Pre-expand DynamicSubmenu items once per registry; without this
            // MenuRenderer::render rescans + reparses every theme JSON file
            // on every frame.
            self.expanded_menus_cache.update(
                &self.theme_registry,
                &self.menus,
                &self.menu_state.themes_dir,
            );
            let hover_target = self.active_window().mouse_state.hover_target.clone();
            let menu_bar_mnemonics = self.config.editor.menu_bar_mnemonics;
            let expanded = self.expanded_menus_cache.get().expect("just updated");
            let keybindings = self.keybindings.read().unwrap();
            let new_menu_layout = crate::view::ui::MenuRenderer::render(
                frame,
                menu_bar_area,
                expanded,
                &self.menu_state,
                &keybindings,
                &*self.theme.read().unwrap(),
                hover_target.as_ref(),
                menu_bar_mnemonics,
            );
            drop(keybindings);
            self.active_chrome_mut().menu_layout = Some(new_menu_layout);
        } else {
            self.active_chrome_mut().menu_layout = None;
        }

        // Render tab context menu if open
        let tab_ctx_menu = self.active_window().tab_context_menu.clone();
        if let Some(menu) = tab_ctx_menu {
            self.render_tab_context_menu(frame, &menu);
        }

        let fe_ctx_menu = self.active_window().file_explorer_context_menu.clone();
        if let Some(menu) = fe_ctx_menu {
            self.render_file_explorer_context_menu(frame, &menu);
        }

        // Record non-editor region theme keys for the theme inspector
        self.record_non_editor_theme_regions();

        // Render theme info popup (Ctrl+Right-Click)
        self.render_theme_info_popup(frame);

        // Render tab drag drop zone overlay if dragging a tab
        let drag_state_clone = self.active_window().mouse_state.dragging_tab.clone();
        if let Some(ref drag_state) = drag_state_clone {
            if drag_state.is_dragging() {
                self.render_tab_drop_zone(frame, drag_state);
            }
        }

        // Render software mouse cursor when GPM is active
        // GPM can't draw its cursor on the alternate screen buffer used by TUI apps,
        // so we draw our own cursor at the tracked mouse position.
        // This must happen LAST in the render flow so we can read the already-rendered
        // cell content and invert it.
        if self.active_window_mut().gpm_active {
            if let Some((col, row)) = self.active_window_mut().mouse_cursor_position {
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
        if self.active_window_mut().keyboard_capture && self.active_window().terminal_mode {
            // Find the active split's content area
            let active_split = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.splits.as_ref())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .active_split();
            let active_split_area = self
                .active_layout()
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
            if self.active_window().prompt.is_none() && !self.cursor_obscured_by_overlay(cx, cy) {
                frame.set_cursor_position((cx, cy));
            }
        }

        // Convert all colors for terminal capability (256/16 color fallback)
        crate::view::color_support::convert_buffer_colors(
            frame.buffer_mut(),
            self.color_capability,
        );

        // Frame-buffer animations run last so they mutate the final paint.
        self.active_window_mut()
            .animations
            .apply_all(frame.buffer_mut());
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
        // disable animations entirely from config. The dedicated
        // `cursor_jump_animation` toggle suppresses just the cursor-jump
        // trail while leaving ambient animations (tab slides, dashboard,
        // plugin effects) running.
        if !self.config.editor.animations || !self.config.editor.cursor_jump_animation {
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
        // vertically, or — for moves that stay within ±2 rows — at
        // least 80 columns horizontally. The horizontal threshold is
        // generous because typing, arrow keys, word-jump, and Home/End
        // on long source lines can all exceed a smaller bound without
        // being a genuine "jump".
        let crossed_panes = prev_split != active_split;
        let row_jump = dy > 2;
        let col_jump = dx >= 80;
        if !crossed_panes && !row_jump && !col_jump {
            return;
        }

        // Cancel any prior cursor-jump animation so trails don't stack.
        if let Some(prev_anim) = self.cursor_jump_animation.take() {
            self.active_window_mut().animations.cancel(prev_anim);
        }

        let cursor_color = self.theme.read().unwrap().cursor;
        let bg_color = self.theme.read().unwrap().editor_bg;
        let id = self.active_window_mut().animations.start(
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
                cursor_color,
                bg_color,
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
            .active_chrome()
            .popup_areas
            .iter()
            .any(|entry| inside(entry.1))
        {
            return true;
        }
        if self
            .active_chrome()
            .global_popup_areas
            .iter()
            .any(|entry| inside(entry.1))
        {
            return true;
        }
        if let Some((rect, _, _, _)) = self.active_chrome().suggestions_area {
            if inside(rect) {
                return true;
            }
        }
        if let Some(ref fb) = self.active_window().file_browser_layout {
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
        let Some(prompt) = &self.active_window_mut().prompt else {
            return;
        };

        // Overlay prompts (Live Grep, issue #1796) get a dedicated
        // centred floating frame instead of the bottom-anchored popup.
        if prompt.overlay {
            let frame_area = frame.area();
            self.render_overlay_prompt(frame, frame_area);
            return;
        }

        if matches!(
            prompt.prompt_type,
            PromptType::OpenFile | PromptType::SwitchProject | PromptType::SaveFileAs
        ) {
            let hover_target = self.active_window().mouse_state.hover_target.clone();
            let theme = self.theme.read().unwrap().clone();
            let keybindings = self.keybindings.read().unwrap();
            let kb_clone = keybindings.clone();
            drop(keybindings);
            let max_height = prompt_area.y.saturating_sub(1).min(20);
            let popup_area = ratatui::layout::Rect {
                x: 0,
                y: prompt_area.y.saturating_sub(max_height),
                width,
                height: max_height,
            };
            let __win = self.active_window_mut();
            let Some(file_open_state) = &mut __win.file_open_state else {
                return;
            };
            __win.file_browser_layout = crate::view::ui::FileBrowserRenderer::render(
                frame,
                popup_area,
                file_open_state,
                &theme,
                &hover_target,
                Some(&kb_clone),
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

        // Adjust the prompt's scroll position to keep the selected item
        // visible, scrolling the minimum amount required.
        if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
            prompt.ensure_selected_visible();
        }
        let Some(prompt) = &self.active_window().prompt else {
            return;
        };

        let new_suggestions_area = SuggestionsRenderer::render_with_hover(
            frame,
            suggestions_area,
            prompt,
            &*self.theme.read().unwrap(),
            self.active_window().mouse_state.hover_target.as_ref(),
            true,
        );
        let chrome = self.active_chrome_mut();
        chrome.suggestions_area = new_suggestions_area;
        if chrome.suggestions_area.is_some() {
            chrome.suggestions_outer_area = Some(suggestions_area);
        }

        if is_quick_open {
            let hints_area = ratatui::layout::Rect {
                x: 0,
                y: prompt_area.y.saturating_sub(hints_height),
                width,
                height: hints_height,
            };
            frame.render_widget(ratatui::widgets::Clear, hints_area);
            Self::render_quick_open_hints(frame, hints_area, &*self.theme.read().unwrap());
        }
    }

    /// Resolve the overlay's currently-selected match into a real
    /// `Buffer` parked in a phantom `LeafId`, so the preview pane can
    /// reuse the regular per-leaf renderer (with syntax highlighting,
    /// gutter, scrollbars, folding). No-op when the prompt has no
    /// selection or its label is not a `path:line[:col]` triple.
    /// Render the entire stashed split tree of `self.preview_window_id`
    /// into `inner` — Primitive #1 of
    /// `docs/internal/conductor-sessions-design.md`'s "Rich
    /// Control Room rendering". Reuses the editor's existing
    /// `render_content` path against the previewed session's
    /// stashed `(SplitManager, view_states)` so syntax
    /// highlighting, terminal grids, decorations, and folding
    /// all surface natively in the preview pane.
    ///
    /// The previewed session's splits stash is `take`n out for
    /// the duration of the call (so we can pass `&mut` through
    /// the renderer without re-entering `self.windows`) and put
    /// back after. `pending_hardware_cursor` and
    /// `cell_theme_map` use scratch locals so the active editor
    /// area's hit-testing isn't clobbered by the preview pass.
    fn render_session_preview_into_rect(
        &mut self,
        frame: &mut ratatui::Frame,
        inner: ratatui::layout::Rect,
        theme: &crate::view::theme::Theme,
    ) {
        let Some(sid) = self.preview_window_id else {
            return;
        };

        // Terminal grid → buffer text sync, preview-safe variant.
        // `sync_terminal_to_buffer` is the canonical sync but it
        // also mutates `self.split_view_states[active_split]` —
        // which during preview is the *active* (caller) session's
        // view-state, not the previewed one. That corrupts the
        // active session's viewport (cursor jumps past EOF, top
        // line becomes blank). Here we do just the parts that are
        // safe to run from a foreign session: append visible
        // screen to backing file, then reload that one buffer.
        let preview_buffers: Vec<fresh_core::BufferId> = self
            .windows
            .get(&sid)
            .map(|s| s.buffers.keys().copied().collect())
            .unwrap_or_default();
        for bid in preview_buffers {
            let Some(&terminal_id) = self.active_window().terminal_buffers.get(&bid) else {
                continue;
            };
            let Some(backing_file) = self
                .active_window()
                .terminal_backing_files
                .get(&terminal_id)
                .cloned()
            else {
                continue;
            };
            if let Some(handle) = self.active_window().terminal_manager.get(terminal_id) {
                if let Ok(mut state) = handle.state.lock() {
                    if let Ok(metadata) = self.authority.filesystem.metadata(&backing_file) {
                        state.set_backing_file_history_end(metadata.size);
                    }
                    if let Ok(mut file) = self
                        .authority
                        .filesystem
                        .open_file_for_append(&backing_file)
                    {
                        use std::io::BufWriter;
                        let mut writer = BufWriter::new(&mut *file);
                        if let Err(e) = state.append_visible_screen(&mut writer) {
                            tracing::error!(
                                "preview: failed to append visible screen for terminal buffer {bid:?}: {e}"
                            );
                        }
                    }
                }
            }
            let large_file_threshold = self.config.editor.large_file_threshold_bytes as usize;
            if let Ok(new_state) = crate::state::EditorState::from_file_with_languages(
                &backing_file,
                self.terminal_width,
                self.terminal_height,
                large_file_threshold,
                &self.grammar_registry,
                &self.config.languages,
                std::sync::Arc::clone(&self.authority.filesystem),
            ) {
                if let Some(state) = self
                    .windows
                    .get_mut(&sid)
                    .map(|w| &mut w.buffers)
                    .expect("preview window present")
                    .get_mut(&bid)
                {
                    *state = new_state;
                    state.buffer.set_modified(false);
                    state.editing_disabled = true;
                }
            }
        }

        // Pull the previewed window's split stash and sub-fields
        // out under one `&mut Window` borrow. Multiple disjoint
        // sub-borrows (`buffers`, `event_logs`, `splits`) coexist
        // on the same `Window`, so the renderer call can take all
        // three by `&mut` while the rest of `&mut self` stays
        // available for `composite_buffers` / `config` / etc.
        //
        // Step 0h: previously this used `splits.take()` + restore
        // because the inline-borrow patterns elsewhere couldn't
        // co-exist with a held `&mut sid.splits`. Now that all
        // per-window state lives on `Window`, we destructure
        // `splits.as_mut()` directly — no transient swap, no
        // side-effect plumbing — matching design Primitive #1.
        // Bail if the session has no stash yet (never been
        // activated and never had a terminal / file routed in via
        // createTerminal({windowId})).
        let __win_for_preview = self.windows.get_mut(&sid).expect("preview window present");
        let __preview_buffers = &mut __win_for_preview.buffers;
        let __preview_metadata = &__win_for_preview.buffer_metadata;
        let __preview_event_logs = &mut __win_for_preview.event_logs;
        let __preview_composite_buffers = &mut __win_for_preview.composite_buffers;
        let __preview_composite_view_states = &mut __win_for_preview.composite_view_states;
        let Some((mgr, view_states)) = __win_for_preview.splits.as_mut().map(|(m, vs)| (m, vs))
        else {
            return;
        };

        // Per-call scratch — keeps the preview pass from
        // clobbering the active editor area's hit-testing /
        // hardware-cursor placement.
        let mut scratch_cell_theme_map: Vec<crate::app::types::CellThemeInfo> = Vec::new();
        let mut scratch_pending_cursor: Option<(u16, u16)> = None;
        let lsp_waiting = false; // preview never shows LSP-waiting chrome
        let no_grouped_subtrees: std::collections::HashMap<
            crate::model::event::LeafId,
            crate::view::split::SplitNode,
        > = std::collections::HashMap::new();

        let _ = crate::view::ui::SplitRenderer::render_content(
            frame,
            inner,
            &*mgr,
            __preview_buffers,
            __preview_metadata,
            __preview_event_logs,
            __preview_composite_buffers,
            __preview_composite_view_states,
            theme,
            self.ansi_background.as_ref(),
            self.background_fade,
            lsp_waiting,
            self.config.editor.large_file_threshold_bytes,
            self.config.editor.line_wrap,
            self.config.editor.estimated_line_length,
            self.config.editor.highlight_context_bytes,
            Some(view_states),
            &no_grouped_subtrees,
            true, // hide_cursor — the active session owns the hardware caret
            None, // no tab-hover routing in the preview
            None,
            None,
            false, // not maximized
            self.config.editor.relative_line_numbers,
            __win_for_preview.tab_bar_visible,
            self.config.editor.use_terminal_bg,
            self.session_mode || !self.software_cursor_only,
            self.software_cursor_only,
            // Scrollbars are noisy in a small preview rect; the
            // active session's chrome is the source of truth.
            false,
            false,
            self.config.editor.diagnostics_inline_text,
            false, // hide tilde markers in the preview
            self.config.editor.highlight_current_column,
            &mut scratch_cell_theme_map,
            inner.width,
            &mut scratch_pending_cursor,
        );
    }

    fn prepare_overlay_preview(&mut self) {
        use crate::input::quick_open::parse_path_line_col;

        let (path_str, line, col) = {
            let Some(prompt) = self.active_window().prompt.as_ref() else {
                return;
            };
            let Some(idx) = prompt.selected_suggestion else {
                return;
            };
            let Some(s) = prompt.suggestions.get(idx) else {
                return;
            };
            // Suggestions emitted by the Finder library use `value` as
            // an opaque index; the parseable label lives in `text`.
            // Resume-replay is the inverse: `value` carries the full
            // path:line:col triple.
            let from_text = parse_path_line_col(&s.text);
            if !from_text.0.is_empty() && from_text.1.is_some() {
                from_text
            } else if let Some(v) = s.value.as_deref() {
                parse_path_line_col(v)
            } else {
                from_text
            }
        };
        if path_str.is_empty() {
            return;
        }
        let line = line.unwrap_or(1).saturating_sub(1);
        let col = col.unwrap_or(1).saturating_sub(1);

        // Resolve relative to the working directory.
        let path_buf = std::path::PathBuf::from(&path_str);
        let abs_path = if path_buf.is_absolute() {
            path_buf
        } else {
            self.working_dir.join(&path_buf)
        };
        // Canonicalize for buffer-dedup parity with open_file_no_focus.
        let abs_path = self
            .authority
            .filesystem
            .canonicalize(&abs_path)
            .unwrap_or(abs_path);

        // If the standalone state already targets this path, just
        // re-seed the cursor and skip the file-load roundtrip.
        let already_target = self
            .active_window()
            .overlay_preview_state
            .as_ref()
            .is_some_and(|st| {
                self.windows
                    .get(&self.active_window)
                    .map(|w| &w.buffers)
                    .expect("active window present")
                    .get(&st.buffer_id)
                    .and_then(|s| s.buffer.file_path())
                    .is_some_and(|p| p == abs_path.as_path())
            });

        let buffer_id = if already_target {
            self.active_window_mut()
                .overlay_preview_state
                .as_ref()
                .unwrap()
                .buffer_id
        } else {
            // Snapshot whether this path was already known so we can
            // tell "I just loaded it for preview" from "the user had
            // it open" — only the former gets cleaned up on close.
            let was_open = self
                .buffers()
                .iter()
                .any(|(_, s)| s.buffer.file_path() == Some(abs_path.as_path()));
            // Capture the active split so we can undo the side
            // effects of `open_file_no_focus` (it adds the buffer to
            // the active split's tabs and may switch its active
            // buffer to the loaded file).
            let source_split = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.splits.as_ref())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .active_split();
            // `open_file_for_preview` always allocates a fresh buffer
            // — never repurposes the "no name" empty buffer the user
            // is currently looking at — so the background view stays
            // intact while we cycle through preview results.
            let buffer_id = match self.open_file_for_preview(abs_path.as_path()) {
                Ok(id) => id,
                Err(_e) => return,
            };
            if !was_open {
                if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(&buffer_id) {
                    meta.hidden_from_tabs = true;
                }
                // Drop the buffer from every split's `open_buffers`
                // list so it doesn't surface as a tab anywhere. The
                // phantom buffer is rendered exclusively via the
                // overlay's standalone view-state — it doesn't need
                // to be in `open_buffers`.
                let leaf_ids: Vec<_> = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.splits.as_ref())
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
                        view_state.remove_buffer(buffer_id);
                    }
                }
                // open_file_no_focus may have switched the active
                // buffer of the source split. Restore it.
                let preview_loaded: std::collections::HashSet<BufferId> = self
                    .active_window_mut()
                    .overlay_preview_state
                    .as_ref()
                    .map(|st| st.loaded_buffers.clone())
                    .unwrap_or_default();
                let __active_id = self.active_window;
                let __win = self
                    .windows
                    .get_mut(&__active_id)
                    .expect("active window must exist");
                let __buffer_keys: Vec<BufferId> = __win.buffers.keys().copied().collect();
                let (__mgr, __vs_map) = __win
                    .splits
                    .as_mut()
                    .expect("active window must have a populated split layout");
                if let Some(source_state) = __vs_map.get_mut(&source_split) {
                    if source_state.active_buffer == buffer_id {
                        let fallback = source_state
                            .open_buffers
                            .iter()
                            .find_map(|t| t.as_buffer())
                            .or_else(|| {
                                __buffer_keys
                                    .iter()
                                    .copied()
                                    .find(|b| *b != buffer_id && !preview_loaded.contains(b))
                            });
                        if let Some(fb) = fallback {
                            source_state.switch_buffer(fb);
                            __mgr.set_split_buffer(source_split, fb);
                        }
                    }
                }
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_manager_mut())
                    .expect("active window must have a populated split layout")
                    .set_active_split(source_split);
            }
            buffer_id
        };

        // Build (or update) the standalone preview state. Held off
        // `split_view_states` so cross-cutting iteration never touches
        // it.
        let need_init = self.active_window_mut().overlay_preview_state.is_none();
        if need_init {
            let mut view_state = crate::view::split::SplitViewState::with_buffer(
                self.terminal_width,
                self.terminal_height,
                buffer_id,
            );
            view_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                self.resolve_line_wrap_for_buffer(buffer_id),
                self.config.editor.wrap_indent,
                self.resolve_wrap_column_for_buffer(buffer_id),
                self.config.editor.rulers.clone(),
            );
            let mut loaded_buffers = std::collections::HashSet::new();
            // Whether this *first* preview buffer was newly loaded.
            // The pre-existing case skips the `was_open` branch so
            // we re-derive it from buffer_metadata: a buffer with
            // hidden_from_tabs=true that we just touched is one we
            // owned. Simpler: track via the existing-target check:
            // if `already_target` was false above, the buffer was
            // either pre-open (we left meta alone) or freshly
            // loaded (we set hidden_from_tabs=true). Re-check.
            if let Some(meta) = self.active_window().buffer_metadata.get(&buffer_id) {
                if meta.hidden_from_tabs {
                    loaded_buffers.insert(buffer_id);
                }
            }
            self.active_window_mut().overlay_preview_state =
                Some(crate::app::types::OverlayPreviewState {
                    buffer_id,
                    view_state,
                    loaded_buffers,
                });
        } else {
            // Pre-compute hidden flag (immutable borrow on self.windows)
            // before taking the mutable borrow on overlay_preview_state.
            let hidden_from_tabs = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffer_metadata.get(&buffer_id))
                .is_some_and(|meta| meta.hidden_from_tabs);
            if let Some(state) = self.active_window_mut().overlay_preview_state.as_mut() {
                if state.buffer_id != buffer_id {
                    state.view_state.switch_buffer(buffer_id);
                    if hidden_from_tabs {
                        state.loaded_buffers.insert(buffer_id);
                    }
                }
            }
        }

        // Set the cursor to the match position and centre the line.
        let byte_offset = self
            .buffers()
            .get(&buffer_id)
            .map(|s| {
                s.buffer
                    .position_to_offset(crate::model::piece_tree::Position { line, column: col })
            })
            .unwrap_or(0);
        let line_start = self
            .buffers()
            .get(&buffer_id)
            .and_then(|s| s.buffer.line_start_offset(line))
            .unwrap_or(byte_offset);
        // Compute top_byte BEFORE taking the mutable borrow on
        // overlay_preview_state to keep the borrows disjoint.
        let h_for_preview = self
            .active_window_mut()
            .overlay_preview_state
            .as_ref()
            .map(|s| s.view_state.viewport.height.max(1) as usize)
            .unwrap_or(1);
        let half = h_for_preview / 2;
        let target_top_line = line.saturating_sub(half);
        let top_byte = self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .get(&buffer_id)
            .and_then(|s| s.buffer.line_start_offset(target_top_line))
            .unwrap_or(line_start);
        if let Some(state) = self.active_window_mut().overlay_preview_state.as_mut() {
            state.view_state.cursors.primary_mut().position = byte_offset;
            state.view_state.viewport.top_byte = top_byte;
        }
    }

    /// Render the active prompt as a centred floating overlay
    /// (issue #1796). Layout, top-down inside the overlay frame:
    ///
    /// ```text
    /// ┌─ Live Grep ──────────────────────────────────[Esc to close]┐
    /// │ Search: split_active|                           12 / 142    │  ← input row
    /// │ ─────────────────────────────────────────────────────────── │
    /// │  src/view/split.rs:1117  pub fn split_active(    │ preview │  ← results
    /// │  src/view/split.rs:1123  self.split_active_pos…  │  pane   │     (+ optional
    /// │ …                                                │         │      preview)
    /// └────────────────────────────────────────────────────────────┘
    /// ```
    ///
    /// The overlay does *not* mutate the split tree; it is a pure
    /// `ratatui` overdraw, so dismissing leaves the user's underlying
    /// layout exactly as it was (the issue-#1796 acceptance test).
    fn render_overlay_prompt(&mut self, frame: &mut Frame, area: ratatui::layout::Rect) {
        use crate::view::popup::PopupPosition;
        use ratatui::layout::Rect;
        use ratatui::style::{Modifier, Style};
        use ratatui::text::{Line, Span};
        use ratatui::widgets::{Block, Borders, Clear, Paragraph};

        // Compute the overlay rect via the same percentage logic the
        // popup engine uses. 80% × 80% of the terminal, centred.
        let overlay_pos = PopupPosition::CenteredOverlay {
            width_pct: 80,
            height_pct: 80,
        };
        let overlay_rect = match overlay_pos {
            PopupPosition::CenteredOverlay {
                width_pct,
                height_pct,
            } => {
                let w_pct = width_pct.clamp(1, 100) as u32;
                let h_pct = height_pct.clamp(1, 100) as u32;
                let w = ((area.width as u32 * w_pct) / 100) as u16;
                let h = ((area.height as u32 * h_pct) / 100) as u16;
                let w = w.max(20).min(area.width);
                let h = h.max(8).min(area.height);
                Rect {
                    x: (area.width.saturating_sub(w)) / 2,
                    y: (area.height.saturating_sub(h)) / 2,
                    width: w,
                    height: h,
                }
            }
            _ => unreachable!(),
        };

        // Snapshot view-relevant state before any mutable borrows.
        let theme = self.theme.read().unwrap().clone();
        // The suggestion list inside the overlay can be ~30 rows
        // tall on a typical terminal. Pass the *actual* visible
        // count to `ensure_selected_visible_within` so the scroll
        // offset only advances when the selection genuinely passes
        // the bottom of the visible window — not when it crosses
        // the bottom-popup default cap of `MAX_VISIBLE_SUGGESTIONS`
        // (= 10), which would scroll prematurely.
        //
        // Geometry: overlay frame border (2) + input row (1) +
        // optional toolbar row (1, when `prompt.title` is non-empty)
        // + separator (1). The suggestions popup is rendered
        // borderless inside the overlay (the outer frame already
        // provides a border, so adding a nested one creates a
        // double-frame). Inner content height = overlay.height -
        // chrome.
        let toolbar_visible = self
            .active_window()
            .prompt
            .as_ref()
            .map(|p| !p.title.is_empty())
            .unwrap_or(false);
        let chrome_rows: usize = 4 + if toolbar_visible { 1 } else { 0 };
        let suggestions_visible_rows = (overlay_rect.height as usize).saturating_sub(chrome_rows);
        if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
            prompt.ensure_selected_visible_within(suggestions_visible_rows);
        }
        let Some(prompt) = self.active_window().prompt.as_ref() else {
            return;
        };
        let prompt = prompt.clone();

        // Dim everything outside the overlay rect so the user's
        // focus visibly belongs to the popup. Reuses the same RGB-
        // darkening pass the Settings modal uses (`view::dimming`)
        // — Modifier::DIM alone is barely visible on most terminals.
        crate::view::dimming::apply_dimming_excluding(frame, frame.area(), Some(overlay_rect));

        // Clear and frame. Plugin-owned prompts can publish their
        // own title via `editor.setPromptTitle(...)`; falls back to
        // " Live Grep " plus shortcut hints when unset (so a
        // Resume-replay prompt and freshly-opened plugin prompt look
        // similar even though they take different code paths).
        frame.render_widget(Clear, overlay_rect);
        let default_title: Vec<fresh_core::api::StyledText> = {
            // Mirrors `updateOverlayTitle` in live_grep.ts (kept in
            // sync deliberately so a Resume-replay overlay and a
            // freshly-opened plugin overlay look identical). The
            // input row's prefix already says "Live grep:", so the
            // frame title doesn't repeat the feature name — it
            // shows shortcut hints only. `resume_live_grep` is
            // intentionally NOT shown here; that shortcut only
            // matters once the overlay is closed.
            use crate::input::keybindings::KeyContext;
            use fresh_core::api::{OverlayColorSpec, OverlayOptions, StyledText};
            let keybindings = self.keybindings.read().unwrap();
            let mut hints: Vec<(String, &str)> = Vec::new();
            if let Some(k) = keybindings
                .find_keybinding_for_action("cycle_live_grep_provider", KeyContext::Prompt)
            {
                hints.push((k, "switch grep provider"));
            }
            if let Some(k) = keybindings
                .find_keybinding_for_action("live_grep_export_quickfix", KeyContext::Prompt)
            {
                hints.push((k, "save matches"));
            }
            if hints.is_empty() {
                Vec::new()
            } else {
                let hint_style = Some(OverlayOptions {
                    fg: Some(OverlayColorSpec::ThemeKey("ui.help_key_fg".into())),
                    ..OverlayOptions::default()
                });
                let sep_style = Some(OverlayOptions {
                    fg: Some(OverlayColorSpec::ThemeKey("ui.popup_border_fg".into())),
                    ..OverlayOptions::default()
                });
                let mut segs: Vec<StyledText> = Vec::new();
                for (i, (k, verb)) in hints.into_iter().enumerate() {
                    if i > 0 {
                        segs.push(StyledText {
                            text: " · ".into(),
                            style: sep_style.clone(),
                        });
                    }
                    segs.push(StyledText {
                        text: k,
                        style: hint_style.clone(),
                    });
                    segs.push(StyledText {
                        text: format!(" {verb}"),
                        style: None,
                    });
                }
                segs
            }
        };
        let title_segs: &[fresh_core::api::StyledText] = if prompt.title.is_empty() {
            &default_title
        } else {
            &prompt.title
        };
        let normal_title_style = Style::default()
            .fg(theme.prompt_fg)
            .add_modifier(Modifier::BOLD);
        let title_spans: Vec<Span> = title_segs
            .iter()
            .map(|seg| {
                let style = match &seg.style {
                    Some(opts) => Self::resolve_overlay_style(opts, &theme),
                    None => normal_title_style,
                };
                Span::styled(seg.text.clone(), style)
            })
            .collect();
        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(theme.popup_border_fg))
            .style(Style::default().bg(theme.suggestion_bg));
        let inner = block.inner(overlay_rect);
        frame.render_widget(block, overlay_rect);

        if inner.height == 0 || inner.width == 0 {
            return;
        }

        // Decide whether to split the inner area into results | preview.
        // Below ~120 cols, stack results-only (preview hidden — see
        // design doc §5 "preview pane size when terminal is narrow").
        let preview_min_cols: u16 = 120;
        let show_preview = overlay_rect.width >= preview_min_cols;
        let (results_area, preview_area) = if show_preview {
            let results_w = inner.width / 2;
            (
                Rect {
                    x: inner.x,
                    y: inner.y,
                    width: results_w,
                    height: inner.height,
                },
                Some(Rect {
                    x: inner.x + results_w,
                    y: inner.y,
                    width: inner.width - results_w,
                    height: inner.height,
                }),
            )
        } else {
            (inner, None)
        };

        // Top row of `results_area` is the prompt input.
        let input_row = Rect {
            x: results_area.x,
            y: results_area.y,
            width: results_area.width,
            height: 1,
        };
        // Use the editor's default bg for the input row so it
        // visually reads as an editable text field (the popup bg
        // is reserved for non-editable chrome — toolbar, results
        // list). Both colours come from theme keys, no hardcoded
        // RGB.
        let input_style = Style::default().fg(theme.prompt_fg).bg(theme.editor_bg);
        let count_str = if prompt.suggestions.is_empty() {
            String::new()
        } else {
            format!(
                "{} / {}",
                prompt.selected_suggestion.map(|i| i + 1).unwrap_or(0),
                prompt.suggestions.len()
            )
        };
        use crate::primitives::display_width::str_width;
        let count_w = str_width(&count_str);
        // Reserve one trailing column so the count doesn't sit
        // flush against the right border.
        let right_gap: usize = if count_w > 0 { 1 } else { 0 };
        let visible_input_width = (results_area.width as usize).saturating_sub(count_w + right_gap);
        let truncated_input: String = prompt
            .input
            .chars()
            .take(visible_input_width.saturating_sub(str_width(&prompt.message)))
            .collect();
        // Pad between the typed input and the count so the count
        // is right-aligned (with `right_gap` empty cols at the
        // very edge), independent of how much the user has typed.
        let used = str_width(&prompt.message) + str_width(&truncated_input) + count_w;
        let pad = (results_area.width as usize).saturating_sub(used + right_gap);
        let line = Line::from(vec![
            Span::styled(prompt.message.clone(), input_style),
            Span::styled(truncated_input, input_style),
            Span::styled(" ".repeat(pad), input_style),
            Span::styled(
                count_str,
                Style::default()
                    .fg(theme.popup_border_fg)
                    .bg(theme.editor_bg),
            ),
        ]);
        frame.render_widget(Paragraph::new(line).style(input_style), input_row);

        // Cursor position on the input row.
        let cursor_x = (str_width(&prompt.message)
            + str_width(&prompt.input[..prompt.cursor_pos.min(prompt.input.len())]))
            as u16;
        if cursor_x < input_row.width {
            frame.set_cursor_position((input_row.x + cursor_x, input_row.y));
        }

        // Optional toolbar row (the styled segments the plugin set
        // via setPromptTitle, e.g. "Provider: rg · Alt+P switch
        // grep provider · …"). Sits between the input row and the
        // separator so the user sees feature-scoped controls right
        // under what they're typing — not on the frame border
        // where shortcut hints get visually lost.
        let toolbar_h: u16 = if toolbar_visible { 1 } else { 0 };
        if toolbar_visible && results_area.height >= 2 {
            let toolbar = Rect {
                x: results_area.x,
                y: results_area.y + 1,
                width: results_area.width,
                height: 1,
            };
            frame.render_widget(
                Paragraph::new(Line::from(title_spans))
                    .style(Style::default().bg(theme.suggestion_bg)),
                toolbar,
            );
        }

        // Separator row.
        if results_area.height >= 2 + toolbar_h {
            let sep = Rect {
                x: results_area.x,
                y: results_area.y + 1 + toolbar_h,
                width: results_area.width,
                height: 1,
            };
            let sep_style = Style::default()
                .fg(theme.popup_border_fg)
                .bg(theme.suggestion_bg);
            let sep_text = "─".repeat(results_area.width as usize);
            frame.render_widget(Paragraph::new(sep_text).style(sep_style), sep);
        }

        // Suggestions list fills the rest of `results_area`. Carve
        // off the rightmost 1-column lane for a scrollbar so the
        // user can see how far through the result set the selection
        // is — important when the visible area only fits ~30 of
        // 100+ matches. Only carve when the result set actually
        // exceeds the visible rows; otherwise the scrollbar is
        // visual noise.
        let chrome_above_list: u16 = 2 + toolbar_h;
        // Plugin-supplied footer row (Primitive #2 chrome region).
        // Reserves the bottom-most row of `results_area` for
        // styled hotkey-hint segments. Skipped when the plugin
        // hasn't set a footer — preserves existing behaviour for
        // Live Grep et al.
        let footer_h: u16 = if prompt.footer.is_empty() { 0 } else { 1 };
        if results_area.height > chrome_above_list + footer_h {
            // No `-2` for popup-own-border — we render the
            // borderless variant below since the overlay frame is
            // already a border.
            let inner_rows = (results_area.height - chrome_above_list - footer_h) as usize;
            let needs_scrollbar = prompt.suggestions.len() > inner_rows.max(1);
            let scrollbar_w: u16 = if needs_scrollbar { 1 } else { 0 };
            let list_area = Rect {
                x: results_area.x,
                y: results_area.y + chrome_above_list,
                width: results_area.width.saturating_sub(scrollbar_w),
                height: results_area.height - chrome_above_list - footer_h,
            };
            self.active_chrome_mut().suggestions_area = SuggestionsRenderer::render_with_hover(
                frame,
                list_area,
                &prompt,
                &theme,
                self.active_window_mut().mouse_state.hover_target.as_ref(),
                false,
            );
            if self.active_chrome_mut().suggestions_area.is_some() {
                self.active_chrome_mut().suggestions_outer_area = Some(list_area);
            }
            // Render the scrollbar in the carved lane. Reuses the
            // shared `view::ui::scrollbar` widget so thumb sizing
            // and theme colours match scrollbars elsewhere in the
            // editor (split rendering, file explorer, …).
            if needs_scrollbar {
                use crate::view::ui::scrollbar::{
                    render_scrollbar, ScrollbarColors, ScrollbarState,
                };
                // Scrollbar rect aligns with the borderless
                // suggestions list — same y/height as the list itself
                // since there's no popup-own border to skip.
                let scrollbar_rect = Rect {
                    x: results_area.x + results_area.width - 1,
                    y: list_area.y,
                    width: 1,
                    height: list_area.height,
                };
                let state = ScrollbarState::new(
                    prompt.suggestions.len(),
                    inner_rows.max(1),
                    prompt.scroll_offset,
                );
                render_scrollbar(
                    frame,
                    scrollbar_rect,
                    &state,
                    &ScrollbarColors::from_theme(&theme),
                );
                // Cache the rect for mouse hit testing in
                // `mouse_input.rs::handle_click_prompt_scrollbar`.
                self.active_chrome_mut().suggestions_scrollbar_rect = Some(scrollbar_rect);
            } else {
                self.active_chrome_mut().suggestions_scrollbar_rect = None;
            }
        } else {
            self.active_chrome_mut().suggestions_scrollbar_rect = None;
        }

        // Plugin-supplied footer chrome row (Primitive #2 chrome
        // region). Each segment is a `StyledText` — same styling
        // primitive used by `setPromptTitle` and inline overlays,
        // so plugins can theme hotkey hints with `ui.help_key_fg`,
        // separators with `ui.popup_border_fg`, etc.
        if footer_h == 1 && results_area.height >= 1 {
            let footer_row = Rect {
                x: results_area.x,
                y: results_area.y + results_area.height - 1,
                width: results_area.width,
                height: 1,
            };
            let footer_default_style = Style::default().fg(theme.prompt_fg).bg(theme.suggestion_bg);
            let footer_spans: Vec<Span> = prompt
                .footer
                .iter()
                .map(|seg| {
                    let style = match &seg.style {
                        Some(opts) => Self::resolve_overlay_style(opts, &theme),
                        None => footer_default_style,
                    };
                    Span::styled(seg.text.clone(), style)
                })
                .collect();
            frame.render_widget(
                Paragraph::new(Line::from(footer_spans))
                    .style(Style::default().bg(theme.suggestion_bg)),
                footer_row,
            );
        }

        // Right-half preview pane: a real Buffer rendered via the
        // same per-leaf pipeline regular splits use. Buffer + cursor
        // are already seeded by `prepare_overlay_preview` (called
        // earlier in the render flow). Borrows are split here so we
        // can hand out independent `&mut` references to the
        // renderer's internals without going back through `&mut self`.
        if let Some(preview_rect) = preview_area {
            // Frame the preview area first (vertical separator) so
            // the renderer fills the inner rect.
            use ratatui::widgets::{Block, Borders, Clear};
            frame.render_widget(Clear, preview_rect);
            let block = Block::default()
                .borders(Borders::LEFT)
                .border_style(Style::default().fg(theme.popup_border_fg))
                .style(Style::default().bg(theme.suggestion_bg));
            let inner = block.inner(preview_rect);
            frame.render_widget(block, preview_rect);

            // Primitive #1: if the active plugin asked us to
            // preview a specific (inactive) session in this
            // rect, render that session's entire stashed split
            // tree natively into `inner`. Falls back to the
            // existing path-based phantom-leaf preview when no
            // session override is set.
            if inner.height > 0
                && inner.width > 0
                && self
                    .preview_window_id
                    .is_some_and(|sid| sid != self.active_window && self.windows.contains_key(&sid))
            {
                self.render_session_preview_into_rect(frame, inner, &theme);
            } else if inner.height > 0 && inner.width > 0 {
                // Snapshot scalar config values up front so the
                // mutable-borrow split below has minimal scope.
                // AnsiBackground isn't Clone, so it's taken as a
                // borrow; Rust permits disjoint-field splitting
                // between `&self.ansi_background` and the `&mut`
                // accesses below because they touch distinct fields.
                let bg_fade = self.background_fade;
                let estimated_line_length = self.config.editor.estimated_line_length;
                let highlight_context_bytes = self.config.editor.highlight_context_bytes;
                let relative_line_numbers = self.config.editor.relative_line_numbers;
                let use_terminal_bg = self.config.editor.use_terminal_bg;
                let session_mode = self.session_mode || !self.software_cursor_only;
                let software_cursor_only = self.software_cursor_only;
                let diagnostics_inline_text = self.config.editor.diagnostics_inline_text;
                let show_tilde = false; // preview hides tilde markers
                let highlight_current_column = self.config.editor.highlight_current_column;
                let screen_width = frame.area().width;

                let ansi_ref = self.ansi_background.as_ref();
                let __win = self
                    .windows
                    .get_mut(&self.active_window)
                    .expect("active window present");
                let buffers = &mut __win.buffers;
                let event_logs = &mut __win.event_logs;
                let cell_theme_map = &mut __win.chrome_layout.cell_theme_map;
                let Some(preview_state) = __win.overlay_preview_state.as_mut() else {
                    return;
                };
                preview_state
                    .view_state
                    .viewport
                    .resize(inner.width, inner.height);
                let buffer_id = preview_state.buffer_id;

                if let Some(state) = buffers.get_mut(&buffer_id) {
                    // Deref the SplitViewState once to a concrete
                    // `&mut BufferViewState` so disjoint field
                    // splits (`viewport` + `folds`) are visible
                    // to the borrow checker.
                    let buf_state = preview_state.view_state.active_state_mut();
                    let cursors = buf_state.cursors.clone();
                    let view_mode = buf_state.view_mode.clone();
                    let compose_width = buf_state.compose_width;
                    let compose_column_guides = buf_state.compose_column_guides.clone();
                    let view_transform = buf_state.view_transform.clone();
                    let rulers = buf_state.rulers.clone();
                    let show_line_numbers = buf_state.show_line_numbers;
                    let highlight_current_line = buf_state.highlight_current_line;
                    let viewport_ref = &mut buf_state.viewport;
                    let folds_ref = &mut buf_state.folds;
                    let event_log = event_logs.get_mut(&buffer_id);
                    let _ = crate::view::ui::SplitRenderer::render_phantom_leaf(
                        frame,
                        state,
                        &cursors,
                        viewport_ref,
                        folds_ref,
                        event_log,
                        inner,
                        &theme,
                        ansi_ref,
                        bg_fade,
                        view_mode,
                        compose_width,
                        compose_column_guides,
                        view_transform,
                        estimated_line_length,
                        highlight_context_bytes,
                        buffer_id,
                        relative_line_numbers,
                        use_terminal_bg,
                        session_mode,
                        software_cursor_only,
                        &rulers,
                        show_line_numbers,
                        highlight_current_line,
                        diagnostics_inline_text,
                        show_tilde,
                        highlight_current_column,
                        cell_theme_map,
                        screen_width,
                    );
                }
            }
        }
    }

    /// Render hover highlights for interactive elements (separators, scrollbars)
    pub(super) fn render_hover_highlights(&self, frame: &mut Frame) {
        use ratatui::style::Style;
        use ratatui::text::Span;
        use ratatui::widgets::Paragraph;

        match &self.active_window().mouse_state.hover_target {
            Some(HoverTarget::SplitSeparator(split_id, direction)) => {
                // Highlight the separator with hover color
                for (sid, dir, x, y, length) in &self.active_layout().separator_areas {
                    if sid == split_id && dir == direction {
                        let hover_style = Style::default().fg(self
                            .theme
                            .read()
                            .unwrap()
                            .split_separator_hover_fg);
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
                    &self.active_layout().split_areas
                {
                    if sid == split_id {
                        let hover_style = Style::default().bg(self
                            .theme
                            .read()
                            .unwrap()
                            .scrollbar_thumb_hover_fg);
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
                    &self.active_layout().split_areas
                {
                    if sid == split_id {
                        let track_hover_style = Style::default().bg(self
                            .theme
                            .read()
                            .unwrap()
                            .scrollbar_track_hover_fg);
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
                if let Some(explorer_area) = self.active_layout().file_explorer_area {
                    let hover_style =
                        Style::default().fg(self.theme.read().unwrap().split_separator_hover_fg);
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
                    .fg(self.theme.read().unwrap().menu_highlight_fg)
                    .bg(self.theme.read().unwrap().menu_highlight_bg)
            } else {
                Style::default()
                    .fg(self.theme.read().unwrap().menu_dropdown_fg)
                    .bg(self.theme.read().unwrap().menu_dropdown_bg)
            };

            // Pad the label to fill the menu width
            let label = item.label();
            let content_width = (menu_width as usize).saturating_sub(2); // -2 for borders
            let padded_label = format!(" {:<width$}", label, width = content_width - 1);

            lines.push(Line::from(vec![Span::styled(padded_label, style)]));
        }

        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(self.theme.read().unwrap().menu_border_fg))
            .style(Style::default().bg(self.theme.read().unwrap().menu_dropdown_bg));

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
                    .fg(self.theme.read().unwrap().menu_highlight_fg)
                    .bg(self.theme.read().unwrap().menu_highlight_bg)
            } else {
                Style::default()
                    .fg(self.theme.read().unwrap().menu_dropdown_fg)
                    .bg(self.theme.read().unwrap().menu_dropdown_bg)
            };

            let label = item.label();
            let content_width = (menu_width as usize).saturating_sub(2);
            let padded_label = format!(" {:<width$}", label, width = content_width - 1);

            lines.push(Line::from(vec![Span::styled(padded_label, style)]));
        }

        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(self.theme.read().unwrap().menu_border_fg))
            .style(Style::default().bg(self.theme.read().unwrap().menu_dropdown_bg));

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
            .active_layout()
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
        let drop_zone_bg = self.theme.read().unwrap().tab_drop_zone_bg;
        let drop_zone_border = self.theme.read().unwrap().tab_drop_zone_border;

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
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.splits.as_ref())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        self.active_window_mut()
            .pre_sync_ensure_visible(active_split);
        self.active_window_mut().sync_scroll_groups();

        // Replicate the layout computation that produces editor_content_area.
        // Same constraints as render(): [menu_bar, main_content, status_bar, search_options, prompt_line]
        let constraints = vec![
            Constraint::Length(if self.active_window_mut().menu_bar_visible {
                1
            } else {
                0
            }),
            Constraint::Min(0),
            Constraint::Length(if self.active_window_mut().status_bar_visible {
                1
            } else {
                0
            }), // status bar
            Constraint::Length(0), // search options (doesn't matter for layout)
            Constraint::Length(if self.active_window_mut().prompt_line_visible {
                1
            } else {
                0
            }), // prompt line
        ];
        let main_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(constraints)
            .split(size);
        let main_content_area = main_chunks[1];

        // Compute editor_content_area (with file explorer split if visible)
        let file_explorer_should_show = self.file_explorer_visible()
            && (self.file_explorer().is_some()
                || self.active_window().file_explorer_sync_in_progress);
        let editor_content_area = if file_explorer_should_show {
            let explorer_cols = self
                .active_window()
                .file_explorer_width
                .to_cols(main_content_area.width);
            let horizontal_chunks = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([Constraint::Length(explorer_cols), Constraint::Min(0)])
                .split(main_content_area);
            horizontal_chunks[1]
        } else {
            main_content_area
        };

        // Compute layout for all visible splits and update cached view_line_mappings.
        // Take one &mut borrow on the active window's splits; destructure into
        // (&SplitManager, &mut HashMap<...>) so both arguments come from the
        // same `&mut self.windows` borrow.
        let active_window_id = self.active_window;
        let __win_l = self
            .windows
            .get_mut(&active_window_id)
            .expect("active window must exist");
        let __buffers_l = &mut __win_l.buffers;
        let (split_mgr_l, split_view_states_l) = {
            let (m, vs) = __win_l
                .splits
                .as_mut()
                .expect("active window must have a populated split layout");
            (&*m as &crate::view::split::SplitManager, vs)
        };
        let view_line_mappings = SplitRenderer::compute_content_layout(
            editor_content_area,
            split_mgr_l,
            __buffers_l,
            split_view_states_l,
            &*self.theme.read().unwrap(),
            false, // lsp_waiting — not relevant for layout
            self.config.editor.estimated_line_length,
            self.config.editor.highlight_context_bytes,
            self.config.editor.relative_line_numbers,
            self.config.editor.use_terminal_bg,
            self.session_mode || !self.software_cursor_only,
            self.software_cursor_only,
            __win_l.tab_bar_visible,
            self.config.editor.show_vertical_scrollbar,
            self.config.editor.show_horizontal_scrollbar,
            self.config.editor.diagnostics_inline_text,
            self.config.editor.show_tilde,
        );

        self.active_layout_mut().view_line_mappings = view_line_mappings;
    }

    /// Clear the search history
    /// Used primarily for testing to ensure test isolation
    pub fn clear_search_history(&mut self) {
        if let Some(history) = self.active_window_mut().prompt_histories.get_mut("search") {
            history.clear();
        }
    }

    /// Emit an OSC 2 escape sequence to set the host terminal's window/tab
    /// title based on the active buffer's display name and the project name
    /// (the working directory's last path component). Deduplicated against
    /// the last title we wrote so we don't spam stdout every frame.
    ///
    /// Gated by `editor.set_window_title` (default on). Terminals that
    /// don't implement OSC 2 silently drop the sequence.
    fn update_terminal_title(&mut self, display_name: &str) {
        if !self.config.editor.set_window_title {
            return;
        }
        let project_name = self.working_dir.file_name().and_then(|s| s.to_str());
        let new_title =
            crate::services::terminal_title::build_window_title(display_name, project_name);
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
        for (key, history) in &self.active_window().prompt_histories {
            let path = self.dir_context.prompt_history_path(key);
            if let Err(e) = history.save_to_file(&path) {
                tracing::warn!("Failed to save {} history: {}", key, e);
            } else {
                tracing::debug!("Saved {} history to {:?}", key, path);
            }
        }
    }

    /// Resolve a plugin-supplied [`OverlayOptions`] to a ratatui
    /// [`Style`] against the active theme. RGB colours pass through;
    /// theme keys (e.g. `"ui.help_key_fg"`) are looked up via
    /// `theme.resolve_theme_key`. Mirrors the resolution
    /// `OverlayFace::from_options` + char_style.rs do for buffer
    /// overlays — pulled here so the prompt-frame renderer can build
    /// styled spans inline.
    fn resolve_overlay_style(
        opts: &fresh_core::api::OverlayOptions,
        theme: &crate::view::theme::Theme,
    ) -> ratatui::style::Style {
        use crate::view::theme::named_color_from_str;
        use fresh_core::api::OverlayColorSpec;
        use ratatui::style::{Color, Modifier, Style};

        let resolve = |spec: &OverlayColorSpec| -> Option<Color> {
            match spec {
                OverlayColorSpec::Rgb(r, g, b) => Some(Color::Rgb(*r, *g, *b)),
                OverlayColorSpec::ThemeKey(k) => {
                    named_color_from_str(k).or_else(|| theme.resolve_theme_key(k))
                }
            }
        };

        let mut style = Style::default();
        if let Some(ref fg) = opts.fg {
            if let Some(c) = resolve(fg) {
                style = style.fg(c);
            }
        }
        if let Some(ref bg) = opts.bg {
            if let Some(c) = resolve(bg) {
                style = style.bg(c);
            }
        }
        let mut m = Modifier::empty();
        if opts.bold {
            m |= Modifier::BOLD;
        }
        if opts.italic {
            m |= Modifier::ITALIC;
        }
        if opts.underline {
            m |= Modifier::UNDERLINED;
        }
        if opts.strikethrough {
            m |= Modifier::CROSSED_OUT;
        }
        if !m.is_empty() {
            style = style.add_modifier(m);
        }
        style
    }
}
