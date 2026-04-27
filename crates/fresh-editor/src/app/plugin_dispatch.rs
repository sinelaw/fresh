//! Plugin command dispatch and plugin-specific handlers on `Editor`.
//!
//! Three clusters previously inline in mod.rs:
//!
//! - `update_plugin_state_snapshot` — synchronizes the immutable view of
//!   editor state plugins observe between commands.
//! - `handle_plugin_command` — the giant match dispatching every
//!   PluginCommand variant to a specialized handler. Most arms call
//!   methods in app/plugin_commands.rs; the rest live below.
//! - The handle_* family — buffer/path lookups, action execution, plugin
//!   lifecycle management, and view-control commands callable from
//!   plugin code.

use std::sync::Arc;

use anyhow::Result as AnyhowResult;

use fresh_core::api::{BufferSavedDiff, JsCallbackId, PluginCommand};

use crate::model::event::{BufferId, LeafId, SplitId};
use crate::services::async_bridge::AsyncMessage;
use crate::view::split::SplitViewState;

use super::Editor;

impl Editor {
    /// Update the plugin state snapshot with current editor state
    #[cfg(feature = "plugins")]
    pub(super) fn update_plugin_state_snapshot(&mut self) {
        // Update TypeScript plugin manager state
        if let Some(snapshot_handle) = self.plugin_manager.state_snapshot_handle() {
            use fresh_core::api::{BufferInfo, CursorInfo, ViewportInfo};
            let mut snapshot = snapshot_handle.write().unwrap();

            // Update grammar info (only rebuild if count changed, cheap check)
            let grammar_count = self.grammar_registry.available_syntaxes().len();
            if snapshot.available_grammars.len() != grammar_count {
                snapshot.available_grammars = self
                    .grammar_registry
                    .available_grammar_info()
                    .into_iter()
                    .map(|g| fresh_core::api::GrammarInfoSnapshot {
                        name: g.name,
                        source: g.source.to_string(),
                        file_extensions: g.file_extensions,
                        short_name: g.short_name,
                    })
                    .collect();
            }

            // Update active buffer ID
            snapshot.active_buffer_id = self.active_buffer();

            // Update active split ID
            snapshot.active_split_id = self.split_manager.active_split().0 .0;

            // Clear and update buffer info
            snapshot.buffers.clear();
            snapshot.buffer_saved_diffs.clear();
            snapshot.buffer_cursor_positions.clear();
            snapshot.buffer_text_properties.clear();

            for (buffer_id, state) in &self.buffers {
                let is_virtual = self
                    .buffer_metadata
                    .get(buffer_id)
                    .map(|m| m.is_virtual())
                    .unwrap_or(false);
                // Report the ACTIVE split's view_mode so plugins can distinguish
                // which mode the user is currently in. Separately, report whether
                // ANY split has compose mode so plugins can maintain decorations
                // for compose-mode splits even when a source-mode split is active.
                let active_split = self.split_manager.active_split();
                let active_vs = self.split_view_states.get(&active_split);
                let view_mode = active_vs
                    .and_then(|vs| vs.buffer_state(*buffer_id))
                    .map(|bs| match bs.view_mode {
                        crate::state::ViewMode::Source => "source",
                        crate::state::ViewMode::PageView => "compose",
                    })
                    .unwrap_or("source");
                let compose_width = active_vs
                    .and_then(|vs| vs.buffer_state(*buffer_id))
                    .and_then(|bs| bs.compose_width);
                let is_composing_in_any_split = self.split_view_states.values().any(|vs| {
                    vs.buffer_state(*buffer_id)
                        .map(|bs| matches!(bs.view_mode, crate::state::ViewMode::PageView))
                        .unwrap_or(false)
                });
                let is_preview = self
                    .buffer_metadata
                    .get(buffer_id)
                    .map(|m| m.is_preview)
                    .unwrap_or(false);
                // Which splits currently hold this buffer — lets plugins
                // implement "focus existing if visible, else open new"
                // without tracking split ids across editor restarts
                // (the restart reassigns them). SplitManager has the
                // authoritative map; we just mirror it.
                let splits: Vec<fresh_core::SplitId> = self
                    .split_manager
                    .splits_for_buffer(*buffer_id)
                    .into_iter()
                    .map(|leaf_id| leaf_id.0)
                    .collect();
                let buffer_info = BufferInfo {
                    id: *buffer_id,
                    path: state.buffer.file_path().map(|p| p.to_path_buf()),
                    modified: state.buffer.is_modified(),
                    length: state.buffer.len(),
                    is_virtual,
                    view_mode: view_mode.to_string(),
                    is_composing_in_any_split,
                    compose_width,
                    language: state.language.clone(),
                    is_preview,
                    splits,
                };
                snapshot.buffers.insert(*buffer_id, buffer_info);

                let diff = {
                    let diff = state.buffer.diff_since_saved();
                    BufferSavedDiff {
                        equal: diff.equal,
                        byte_ranges: diff.byte_ranges.clone(),
                    }
                };
                snapshot.buffer_saved_diffs.insert(*buffer_id, diff);

                // Regular buffers live in exactly one split's keyed_states.
                // Panel (hidden) buffers natively live inside a group's inner
                // split — but the close-buffer path can leave a *shadow*
                // entry in the group's host split (from `switch_buffer`'s
                // auto-insert, kept to preserve the
                // `active_buffer ∈ keyed_states` invariant). For hidden
                // buffers we therefore skip group-host splits and pick the
                // inner split, which is the authoritative home.
                let is_hidden = self
                    .buffer_metadata
                    .get(buffer_id)
                    .is_some_and(|m| m.hidden_from_tabs);
                let source_split = self.split_view_states.iter().find(|(split_id, vs)| {
                    vs.keyed_states.contains_key(buffer_id)
                        && !(is_hidden && self.grouped_subtrees.contains_key(split_id))
                });
                let cursor_pos = source_split
                    .and_then(|(_, vs)| vs.buffer_state(*buffer_id))
                    .map(|bs| bs.cursors.primary().position)
                    .unwrap_or(0);
                tracing::trace!(
                    "snapshot: buffer {:?} cursor_pos={} (from split {:?})",
                    buffer_id,
                    cursor_pos,
                    source_split.map(|(id, _)| *id),
                );
                snapshot
                    .buffer_cursor_positions
                    .insert(*buffer_id, cursor_pos);

                // Store text properties if this buffer has any
                if !state.text_properties.is_empty() {
                    snapshot
                        .buffer_text_properties
                        .insert(*buffer_id, state.text_properties.all().to_vec());
                }
            }

            // Update cursor information for active buffer
            if let Some(active_vs) = self
                .split_view_states
                .get(&self.split_manager.active_split())
            {
                // Primary cursor (from SplitViewState)
                let active_cursors = &active_vs.cursors;
                let primary = active_cursors.primary();
                let primary_position = primary.position;
                let primary_selection = primary.selection_range();

                snapshot.primary_cursor = Some(CursorInfo {
                    position: primary_position,
                    selection: primary_selection.clone(),
                });

                // All cursors
                snapshot.all_cursors = active_cursors
                    .iter()
                    .map(|(_, cursor)| CursorInfo {
                        position: cursor.position,
                        selection: cursor.selection_range(),
                    })
                    .collect();

                // Selected text from primary cursor (for clipboard plugin)
                if let Some(range) = primary_selection {
                    if let Some(active_state) = self.buffers.get_mut(&self.active_buffer()) {
                        snapshot.selected_text =
                            Some(active_state.get_text_range(range.start, range.end));
                    }
                }

                // Viewport - get from SplitViewState (the authoritative source)
                let top_line = self.buffers.get(&self.active_buffer()).and_then(|state| {
                    if state.buffer.line_count().is_some() {
                        Some(state.buffer.get_line_number(active_vs.viewport.top_byte))
                    } else {
                        None
                    }
                });
                snapshot.viewport = Some(ViewportInfo {
                    top_byte: active_vs.viewport.top_byte,
                    top_line,
                    left_column: active_vs.viewport.left_column,
                    width: active_vs.viewport.width,
                    height: active_vs.viewport.height,
                });
            } else {
                snapshot.primary_cursor = None;
                snapshot.all_cursors.clear();
                snapshot.viewport = None;
                snapshot.selected_text = None;
            }

            // Per-split snapshot — every split's active buffer + viewport
            // so plugins (multi-split flash labels, sync decorations,
            // etc.) can iterate every visible buffer instead of only the
            // active one.
            snapshot.splits.clear();
            for (leaf_id, vs) in &self.split_view_states {
                let buf_id = vs.active_buffer;
                let top_line = self.buffers.get(&buf_id).and_then(|state| {
                    if state.buffer.line_count().is_some() {
                        Some(state.buffer.get_line_number(vs.viewport.top_byte))
                    } else {
                        None
                    }
                });
                snapshot.splits.push(fresh_core::api::SplitSnapshot {
                    split_id: leaf_id.0 .0,
                    buffer_id: buf_id,
                    viewport: ViewportInfo {
                        top_byte: vs.viewport.top_byte,
                        top_line,
                        left_column: vs.viewport.left_column,
                        width: vs.viewport.width,
                        height: vs.viewport.height,
                    },
                });
            }

            // Update clipboard (provide internal clipboard content to plugins)
            snapshot.clipboard = self.clipboard.get_internal().to_string();

            // Update working directory (for spawning processes in correct directory)
            snapshot.working_dir = self.working_dir.clone();
            snapshot.authority_label = self.authority.display_label.clone();

            // Update LSP diagnostics: Arc refcount bump; no clone.
            snapshot.diagnostics = Arc::clone(&self.stored_diagnostics);

            // Update LSP folding ranges: Arc refcount bump; no clone.
            snapshot.folding_ranges = Arc::clone(&self.stored_folding_ranges);

            // Update config. Reserialize only when the underlying
            // `Arc<Config>` pointer has actually moved since the last
            // refresh — `Arc::ptr_eq` vs `config_snapshot_anchor` is a
            // sound cache key because the anchor keeps `self.config`'s
            // strong count at ≥ 2, forcing every `Arc::make_mut` on the
            // editor side to CoW into a new allocation. On idle (no
            // config mutation), this branch is skipped entirely and the
            // snapshot update is a refcount bump.
            if !Arc::ptr_eq(&self.config, &self.config_snapshot_anchor) {
                let json = serde_json::to_value(&*self.config).unwrap_or(serde_json::Value::Null);
                self.config_cached_json = Arc::new(json);
                self.config_snapshot_anchor = Arc::clone(&self.config);
            }
            snapshot.config = Arc::clone(&self.config_cached_json);

            // Update user config (cached raw file contents, not merged with defaults).
            // This allows plugins to distinguish between user-set and default values.
            // Arc refcount bump; no clone.
            snapshot.user_config = Arc::clone(&self.user_config_raw);

            // Update editor mode (for vi mode and other modal editing)
            snapshot.editor_mode = self.editor_mode.clone();

            // Update plugin global states from Rust-side store.
            // Merge using or_insert to preserve JS-side write-through entries.
            for (plugin_name, state_map) in &self.plugin_global_state {
                let entry = snapshot
                    .plugin_global_states
                    .entry(plugin_name.clone())
                    .or_default();
                for (key, value) in state_map {
                    entry.entry(key.clone()).or_insert_with(|| value.clone());
                }
            }

            // Update plugin view states from active split's BufferViewState.plugin_state.
            // If the active split changed, fully repopulate. Otherwise, merge using
            // or_insert to preserve JS-side write-through entries that haven't
            // round-tripped through the command channel yet.
            let active_split_id = self.split_manager.active_split().0 .0;
            let split_changed = snapshot.plugin_view_states_split != active_split_id;
            if split_changed {
                snapshot.plugin_view_states.clear();
                snapshot.plugin_view_states_split = active_split_id;
            }

            // Clean up entries for buffers that are no longer open
            {
                let open_bids: Vec<_> = snapshot.buffers.keys().copied().collect();
                snapshot
                    .plugin_view_states
                    .retain(|bid, _| open_bids.contains(bid));
            }

            // Merge from Rust-side plugin_state (source of truth for persisted state)
            if let Some(active_vs) = self
                .split_view_states
                .get(&self.split_manager.active_split())
            {
                for (buffer_id, buf_state) in &active_vs.keyed_states {
                    if !buf_state.plugin_state.is_empty() {
                        let entry = snapshot.plugin_view_states.entry(*buffer_id).or_default();
                        for (key, value) in &buf_state.plugin_state {
                            // Use or_insert to preserve JS write-through values
                            entry.entry(key.clone()).or_insert_with(|| value.clone());
                        }
                    }
                }
            }
        }
    }

    /// Handle a plugin command - dispatches to specialized handlers in plugin_commands module
    pub fn handle_plugin_command(&mut self, command: PluginCommand) -> AnyhowResult<()> {
        match command {
            // ==================== Text Editing Commands ====================
            PluginCommand::InsertText {
                buffer_id,
                position,
                text,
            } => {
                self.handle_insert_text(buffer_id, position, text);
            }
            PluginCommand::DeleteRange { buffer_id, range } => {
                self.handle_delete_range(buffer_id, range);
            }
            PluginCommand::InsertAtCursor { text } => {
                self.handle_insert_at_cursor(text);
            }
            PluginCommand::DeleteSelection => {
                self.handle_delete_selection();
            }

            // ==================== Overlay Commands ====================
            PluginCommand::AddOverlay {
                buffer_id,
                namespace,
                range,
                options,
            } => {
                self.handle_add_overlay(buffer_id, namespace, range, options);
            }
            PluginCommand::RemoveOverlay { buffer_id, handle } => {
                self.handle_remove_overlay(buffer_id, handle);
            }
            PluginCommand::ClearAllOverlays { buffer_id } => {
                self.handle_clear_all_overlays(buffer_id);
            }
            PluginCommand::ClearNamespace {
                buffer_id,
                namespace,
            } => {
                self.handle_clear_namespace(buffer_id, namespace);
            }
            PluginCommand::ClearOverlaysInRange {
                buffer_id,
                start,
                end,
            } => {
                self.handle_clear_overlays_in_range(buffer_id, start, end);
            }

            // ==================== Virtual Text Commands ====================
            PluginCommand::AddVirtualText {
                buffer_id,
                virtual_text_id,
                position,
                text,
                color,
                use_bg,
                before,
            } => {
                self.handle_add_virtual_text(
                    buffer_id,
                    virtual_text_id,
                    position,
                    text,
                    color,
                    use_bg,
                    before,
                );
            }
            PluginCommand::AddVirtualTextStyled {
                buffer_id,
                virtual_text_id,
                position,
                text,
                fg,
                bg,
                bold,
                italic,
                before,
            } => {
                self.handle_add_virtual_text_styled(
                    buffer_id,
                    virtual_text_id,
                    position,
                    text,
                    fg,
                    bg,
                    bold,
                    italic,
                    before,
                );
            }
            PluginCommand::RemoveVirtualText {
                buffer_id,
                virtual_text_id,
            } => {
                self.handle_remove_virtual_text(buffer_id, virtual_text_id);
            }
            PluginCommand::RemoveVirtualTextsByPrefix { buffer_id, prefix } => {
                self.handle_remove_virtual_texts_by_prefix(buffer_id, prefix);
            }
            PluginCommand::ClearVirtualTexts { buffer_id } => {
                self.handle_clear_virtual_texts(buffer_id);
            }
            PluginCommand::AddVirtualLine {
                buffer_id,
                position,
                text,
                fg_color,
                bg_color,
                above,
                namespace,
                priority,
            } => {
                self.handle_add_virtual_line(
                    buffer_id, position, text, fg_color, bg_color, above, namespace, priority,
                );
            }
            PluginCommand::ClearVirtualTextNamespace {
                buffer_id,
                namespace,
            } => {
                self.handle_clear_virtual_text_namespace(buffer_id, namespace);
            }

            // ==================== Conceal Commands ====================
            PluginCommand::AddConceal {
                buffer_id,
                namespace,
                start,
                end,
                replacement,
            } => {
                self.handle_add_conceal(buffer_id, namespace, start, end, replacement);
            }
            PluginCommand::ClearConcealNamespace {
                buffer_id,
                namespace,
            } => {
                self.handle_clear_conceal_namespace(buffer_id, namespace);
            }
            PluginCommand::ClearConcealsInRange {
                buffer_id,
                start,
                end,
            } => {
                self.handle_clear_conceals_in_range(buffer_id, start, end);
            }

            PluginCommand::AddFold {
                buffer_id,
                start,
                end,
                placeholder,
            } => {
                self.handle_add_fold(buffer_id, start, end, placeholder);
            }
            PluginCommand::ClearFolds { buffer_id } => {
                self.handle_clear_folds(buffer_id);
            }

            // ==================== Soft Break Commands ====================
            PluginCommand::AddSoftBreak {
                buffer_id,
                namespace,
                position,
                indent,
            } => {
                self.handle_add_soft_break(buffer_id, namespace, position, indent);
            }
            PluginCommand::ClearSoftBreakNamespace {
                buffer_id,
                namespace,
            } => {
                self.handle_clear_soft_break_namespace(buffer_id, namespace);
            }
            PluginCommand::ClearSoftBreaksInRange {
                buffer_id,
                start,
                end,
            } => {
                self.handle_clear_soft_breaks_in_range(buffer_id, start, end);
            }

            // ==================== Menu Commands ====================
            PluginCommand::AddMenuItem {
                menu_label,
                item,
                position,
            } => {
                self.handle_add_menu_item(menu_label, item, position);
            }
            PluginCommand::AddMenu { menu, position } => {
                self.handle_add_menu(menu, position);
            }
            PluginCommand::RemoveMenuItem {
                menu_label,
                item_label,
            } => {
                self.handle_remove_menu_item(menu_label, item_label);
            }
            PluginCommand::RemoveMenu { menu_label } => {
                self.handle_remove_menu(menu_label);
            }

            // ==================== Split Commands ====================
            PluginCommand::FocusSplit { split_id } => {
                self.handle_focus_split(split_id);
            }
            PluginCommand::SetSplitBuffer {
                split_id,
                buffer_id,
            } => {
                self.handle_set_split_buffer(split_id, buffer_id);
            }
            PluginCommand::SetSplitScroll { split_id, top_byte } => {
                self.handle_set_split_scroll(split_id, top_byte);
            }
            PluginCommand::RequestHighlights {
                buffer_id,
                range,
                request_id,
            } => {
                self.handle_request_highlights(buffer_id, range, request_id);
            }
            PluginCommand::CloseSplit { split_id } => {
                self.handle_close_split(split_id);
            }
            PluginCommand::SetSplitRatio { split_id, ratio } => {
                self.handle_set_split_ratio(split_id, ratio);
            }
            PluginCommand::SetSplitLabel { split_id, label } => {
                self.split_manager.set_label(LeafId(split_id), label);
            }
            PluginCommand::ClearSplitLabel { split_id } => {
                self.split_manager.clear_label(split_id);
            }
            PluginCommand::GetSplitByLabel { label, request_id } => {
                self.handle_get_split_by_label(label, request_id);
            }
            PluginCommand::DistributeSplitsEvenly { split_ids: _ } => {
                self.handle_distribute_splits_evenly();
            }
            PluginCommand::SetBufferCursor {
                buffer_id,
                position,
            } => {
                self.handle_set_buffer_cursor(buffer_id, position);
            }
            PluginCommand::SetBufferShowCursors { buffer_id, show } => {
                self.handle_set_buffer_show_cursors(buffer_id, show);
            }

            // ==================== View/Layout Commands ====================
            PluginCommand::SetLayoutHints {
                buffer_id,
                split_id,
                range: _,
                hints,
            } => {
                self.handle_set_layout_hints(buffer_id, split_id, hints);
            }
            PluginCommand::SetLineNumbers { buffer_id, enabled } => {
                self.handle_set_line_numbers(buffer_id, enabled);
            }
            PluginCommand::SetViewMode { buffer_id, mode } => {
                self.handle_set_view_mode(buffer_id, &mode);
            }
            PluginCommand::SetLineWrap {
                buffer_id,
                split_id,
                enabled,
            } => {
                self.handle_set_line_wrap(buffer_id, split_id, enabled);
            }
            PluginCommand::SubmitViewTransform {
                buffer_id,
                split_id,
                payload,
            } => {
                self.handle_submit_view_transform(buffer_id, split_id, payload);
            }
            PluginCommand::ClearViewTransform {
                buffer_id: _,
                split_id,
            } => {
                self.handle_clear_view_transform(split_id);
            }
            PluginCommand::SetViewState {
                buffer_id,
                key,
                value,
            } => {
                self.handle_set_view_state(buffer_id, key, value);
            }
            PluginCommand::SetGlobalState {
                plugin_name,
                key,
                value,
            } => {
                self.handle_set_global_state(plugin_name, key, value);
            }
            PluginCommand::RefreshLines { buffer_id } => {
                self.handle_refresh_lines(buffer_id);
            }
            PluginCommand::RefreshAllLines => {
                self.handle_refresh_all_lines();
            }
            PluginCommand::HookCompleted { .. } => {
                // Sentinel processed in render loop; no-op if encountered elsewhere.
            }
            PluginCommand::SetLineIndicator {
                buffer_id,
                line,
                namespace,
                symbol,
                color,
                priority,
            } => {
                self.handle_set_line_indicator(buffer_id, line, namespace, symbol, color, priority);
            }
            PluginCommand::SetLineIndicators {
                buffer_id,
                lines,
                namespace,
                symbol,
                color,
                priority,
            } => {
                self.handle_set_line_indicators(
                    buffer_id, lines, namespace, symbol, color, priority,
                );
            }
            PluginCommand::ClearLineIndicators {
                buffer_id,
                namespace,
            } => {
                self.handle_clear_line_indicators(buffer_id, namespace);
            }
            PluginCommand::SetFileExplorerDecorations {
                namespace,
                decorations,
            } => {
                self.handle_set_file_explorer_decorations(namespace, decorations);
            }
            PluginCommand::ClearFileExplorerDecorations { namespace } => {
                self.handle_clear_file_explorer_decorations(&namespace);
            }

            // ==================== Status/Prompt Commands ====================
            PluginCommand::SetStatus { message } => {
                self.handle_set_status(message);
            }
            PluginCommand::ApplyTheme { theme_name } => {
                self.apply_theme(&theme_name);
            }
            PluginCommand::OverrideThemeColors { overrides } => {
                self.handle_override_theme_colors(overrides);
            }
            PluginCommand::ReloadConfig => {
                self.reload_config();
            }
            PluginCommand::SetSetting { path, value, .. } => {
                self.handle_set_setting(path, value);
            }
            PluginCommand::ReloadThemes { apply_theme } => {
                self.reload_themes();
                if let Some(theme_name) = apply_theme {
                    self.apply_theme(&theme_name);
                }
            }
            PluginCommand::RegisterGrammar {
                language,
                grammar_path,
                extensions,
            } => {
                self.handle_register_grammar(language, grammar_path, extensions);
            }
            PluginCommand::RegisterLanguageConfig { language, config } => {
                self.handle_register_language_config(language, config);
            }
            PluginCommand::RegisterLspServer { language, config } => {
                self.handle_register_lsp_server(language, config);
            }
            PluginCommand::ReloadGrammars { callback_id } => {
                self.handle_reload_grammars(callback_id);
            }
            PluginCommand::StartPrompt { label, prompt_type } => {
                self.handle_start_prompt(label, prompt_type);
            }
            PluginCommand::StartPromptWithInitial {
                label,
                prompt_type,
                initial_value,
            } => {
                self.handle_start_prompt_with_initial(label, prompt_type, initial_value);
            }
            PluginCommand::StartPromptAsync {
                label,
                initial_value,
                callback_id,
            } => {
                self.handle_start_prompt_async(label, initial_value, callback_id);
            }
            PluginCommand::AwaitNextKey { callback_id } => {
                self.handle_await_next_key(callback_id);
            }
            PluginCommand::SetKeyCaptureActive { active } => {
                self.key_capture_active = active;
                if !active {
                    // Capture window closed; any leftover queued keys
                    // were intended for the plugin and should not now
                    // leak into the editor's normal dispatch.
                    self.pending_key_capture_buffer.clear();
                }
            }
            PluginCommand::SetPromptSuggestions { suggestions } => {
                self.handle_set_prompt_suggestions(suggestions);
            }
            PluginCommand::SetPromptInputSync { sync } => {
                if let Some(prompt) = &mut self.prompt {
                    prompt.sync_input_on_navigate = sync;
                }
            }

            // ==================== Command/Mode Registration ====================
            PluginCommand::RegisterCommand { command } => {
                self.handle_register_command(command);
            }
            PluginCommand::UnregisterCommand { name } => {
                self.handle_unregister_command(name);
            }
            PluginCommand::DefineMode {
                name,
                bindings,
                read_only,
                allow_text_input,
                inherit_normal_bindings,
                plugin_name,
            } => {
                self.handle_define_mode(
                    name,
                    bindings,
                    read_only,
                    allow_text_input,
                    inherit_normal_bindings,
                    plugin_name,
                );
            }

            // ==================== File/Navigation Commands ====================
            PluginCommand::OpenFileInBackground { path } => {
                self.handle_open_file_in_background(path);
            }
            PluginCommand::OpenFileAtLocation { path, line, column } => {
                return self.handle_open_file_at_location(path, line, column);
            }
            PluginCommand::OpenFileInSplit {
                split_id,
                path,
                line,
                column,
            } => {
                return self.handle_open_file_in_split(split_id, path, line, column);
            }
            PluginCommand::ShowBuffer { buffer_id } => {
                self.handle_show_buffer(buffer_id);
            }
            PluginCommand::CloseBuffer { buffer_id } => {
                self.handle_close_buffer(buffer_id);
            }

            // ==================== Animation Commands ====================
            PluginCommand::StartAnimationArea { id, rect, kind } => {
                self.handle_start_animation_area(id, rect, kind);
            }
            PluginCommand::StartAnimationVirtualBuffer {
                id,
                buffer_id,
                kind,
            } => {
                self.handle_start_animation_virtual_buffer(id, buffer_id, kind);
            }
            PluginCommand::CancelAnimation { id } => {
                self.animations
                    .cancel(crate::view::animation::AnimationId::from_raw(id));
            }

            // ==================== LSP Commands ====================
            PluginCommand::SendLspRequest {
                language,
                method,
                params,
                request_id,
            } => {
                self.handle_send_lsp_request(language, method, params, request_id);
            }

            // ==================== Clipboard Commands ====================
            PluginCommand::SetClipboard { text } => {
                self.handle_set_clipboard(text);
            }

            // ==================== Async Plugin Commands ====================
            PluginCommand::SpawnProcess {
                command,
                args,
                cwd,
                callback_id,
            } => {
                self.handle_spawn_process(command, args, cwd, callback_id);
            }

            PluginCommand::SpawnHostProcess {
                command,
                args,
                cwd,
                callback_id,
            } => {
                self.handle_spawn_host_process(command, args, cwd, callback_id);
            }

            PluginCommand::KillHostProcess { process_id } => {
                self.handle_kill_host_process(process_id);
            }

            PluginCommand::SetAuthority { payload } => {
                self.handle_set_authority(payload);
            }

            PluginCommand::ClearAuthority => {
                tracing::info!("Plugin cleared authority; restoring local");
                self.clear_authority();
            }

            PluginCommand::SetRemoteIndicatorState { state } => {
                self.handle_set_remote_indicator_state(state);
            }

            PluginCommand::ClearRemoteIndicatorState => {
                self.remote_indicator_override = None;
            }

            PluginCommand::SpawnProcessWait {
                process_id,
                callback_id,
            } => {
                self.handle_spawn_process_wait(process_id, callback_id);
            }

            PluginCommand::Delay {
                callback_id,
                duration_ms,
            } => {
                self.handle_delay(callback_id, duration_ms);
            }

            PluginCommand::SpawnBackgroundProcess {
                process_id,
                command,
                args,
                cwd,
                callback_id,
            } => {
                self.handle_spawn_background_process(process_id, command, args, cwd, callback_id);
            }

            PluginCommand::KillBackgroundProcess { process_id } => {
                self.handle_kill_background_process(process_id);
            }

            // ==================== Virtual Buffer Commands (complex, kept inline) ====================
            PluginCommand::CreateVirtualBuffer {
                name,
                mode,
                read_only,
            } => {
                self.handle_create_virtual_buffer(name, mode, read_only);
            }
            PluginCommand::CreateVirtualBufferWithContent {
                name,
                mode,
                read_only,
                entries,
                show_line_numbers,
                show_cursors,
                editing_disabled,
                hidden_from_tabs,
                request_id,
            } => {
                self.handle_create_virtual_buffer_with_content(
                    name,
                    mode,
                    read_only,
                    entries,
                    show_line_numbers,
                    show_cursors,
                    editing_disabled,
                    hidden_from_tabs,
                    request_id,
                );
            }
            PluginCommand::CreateVirtualBufferInSplit {
                name,
                mode,
                read_only,
                entries,
                ratio,
                direction,
                panel_id,
                show_line_numbers,
                show_cursors,
                editing_disabled,
                line_wrap,
                before,
                request_id,
            } => {
                self.handle_create_virtual_buffer_in_split(
                    name,
                    mode,
                    read_only,
                    entries,
                    ratio,
                    direction,
                    panel_id,
                    show_line_numbers,
                    show_cursors,
                    editing_disabled,
                    line_wrap,
                    before,
                    request_id,
                );
            }
            PluginCommand::SetVirtualBufferContent { buffer_id, entries } => {
                self.handle_set_virtual_buffer_content(buffer_id, entries);
            }
            PluginCommand::GetTextPropertiesAtCursor { buffer_id } => {
                self.handle_get_text_properties_at_cursor(buffer_id);
            }
            PluginCommand::CreateVirtualBufferInExistingSplit {
                name,
                mode,
                read_only,
                entries,
                split_id,
                show_line_numbers,
                show_cursors,
                editing_disabled,
                line_wrap,
                request_id,
            } => {
                self.handle_create_virtual_buffer_in_existing_split(
                    name,
                    mode,
                    read_only,
                    entries,
                    split_id,
                    show_line_numbers,
                    show_cursors,
                    editing_disabled,
                    line_wrap,
                    request_id,
                );
            }

            // ==================== Context Commands ====================
            PluginCommand::SetContext { name, active } => {
                self.handle_set_context(name, active);
            }

            // ==================== Review Diff Commands ====================
            PluginCommand::SetReviewDiffHunks { hunks } => {
                self.review_hunks = hunks;
                tracing::debug!("Set {} review hunks", self.review_hunks.len());
            }

            // ==================== Vi Mode Commands ====================
            PluginCommand::ExecuteAction { action_name } => {
                self.handle_execute_action(action_name);
            }
            PluginCommand::ExecuteActions { actions } => {
                self.handle_execute_actions(actions);
            }
            PluginCommand::GetBufferText {
                buffer_id,
                start,
                end,
                request_id,
            } => {
                self.handle_get_buffer_text(buffer_id, start, end, request_id);
            }
            PluginCommand::GetLineStartPosition {
                buffer_id,
                line,
                request_id,
            } => {
                self.handle_get_line_start_position(buffer_id, line, request_id);
            }
            PluginCommand::GetLineEndPosition {
                buffer_id,
                line,
                request_id,
            } => {
                self.handle_get_line_end_position(buffer_id, line, request_id);
            }
            PluginCommand::GetBufferLineCount {
                buffer_id,
                request_id,
            } => {
                self.handle_get_buffer_line_count(buffer_id, request_id);
            }
            PluginCommand::ScrollToLineCenter {
                split_id,
                buffer_id,
                line,
            } => {
                self.handle_scroll_to_line_center(split_id, buffer_id, line);
            }
            PluginCommand::ScrollBufferToLine { buffer_id, line } => {
                self.handle_scroll_buffer_to_line(buffer_id, line);
            }
            PluginCommand::SetEditorMode { mode } => {
                self.handle_set_editor_mode(mode);
            }

            // ==================== LSP Helper Commands ====================
            PluginCommand::ShowActionPopup {
                popup_id,
                title,
                message,
                actions,
            } => {
                self.handle_show_action_popup(popup_id, title, message, actions);
            }

            PluginCommand::DisableLspForLanguage { language } => {
                self.handle_disable_lsp_for_language(language);
            }

            PluginCommand::RestartLspForLanguage { language } => {
                self.handle_restart_lsp_for_language(language);
            }

            PluginCommand::SetLspRootUri { language, uri } => {
                self.handle_set_lsp_root_uri(language, uri);
            }

            // ==================== Scroll Sync Commands ====================
            PluginCommand::CreateScrollSyncGroup {
                group_id,
                left_split,
                right_split,
            } => {
                self.handle_create_scroll_sync_group(group_id, left_split, right_split);
            }
            PluginCommand::SetScrollSyncAnchors { group_id, anchors } => {
                self.handle_set_scroll_sync_anchors(group_id, anchors);
            }
            PluginCommand::RemoveScrollSyncGroup { group_id } => {
                self.handle_remove_scroll_sync_group(group_id);
            }

            // ==================== Composite Buffer Commands ====================
            PluginCommand::CreateCompositeBuffer {
                name,
                mode,
                layout,
                sources,
                hunks,
                initial_focus_hunk,
                request_id,
            } => {
                self.handle_create_composite_buffer(
                    name,
                    mode,
                    layout,
                    sources,
                    hunks,
                    initial_focus_hunk,
                    request_id,
                );
            }
            PluginCommand::UpdateCompositeAlignment { buffer_id, hunks } => {
                self.handle_update_composite_alignment(buffer_id, hunks);
            }
            PluginCommand::CloseCompositeBuffer { buffer_id } => {
                self.close_composite_buffer(buffer_id);
            }
            PluginCommand::FlushLayout => {
                self.flush_layout();
            }
            PluginCommand::CompositeNextHunk { buffer_id } => {
                let split_id = self.split_manager.active_split();
                self.composite_next_hunk(split_id, buffer_id);
            }
            PluginCommand::CompositePrevHunk { buffer_id } => {
                let split_id = self.split_manager.active_split();
                self.composite_prev_hunk(split_id, buffer_id);
            }

            // ==================== Buffer Groups ====================
            PluginCommand::CreateBufferGroup {
                name,
                mode,
                layout_json,
                request_id,
            } => {
                self.handle_create_buffer_group(name, mode, layout_json, request_id);
            }
            PluginCommand::SetPanelContent {
                group_id,
                panel_name,
                entries,
            } => {
                self.set_panel_content(group_id, panel_name, entries);
            }
            PluginCommand::CloseBufferGroup { group_id } => {
                self.close_buffer_group(group_id);
            }
            PluginCommand::FocusPanel {
                group_id,
                panel_name,
            } => {
                self.focus_panel(group_id, panel_name);
            }

            // ==================== File Operations ====================
            PluginCommand::SaveBufferToPath { buffer_id, path } => {
                self.handle_save_buffer_to_path(buffer_id, path);
            }

            // ==================== Plugin Management ====================
            #[cfg(feature = "plugins")]
            PluginCommand::LoadPlugin { path, callback_id } => {
                self.handle_load_plugin(path, callback_id);
            }
            #[cfg(feature = "plugins")]
            PluginCommand::UnloadPlugin { name, callback_id } => {
                self.handle_unload_plugin(name, callback_id);
            }
            #[cfg(feature = "plugins")]
            PluginCommand::ReloadPlugin { name, callback_id } => {
                self.handle_reload_plugin(name, callback_id);
            }
            #[cfg(feature = "plugins")]
            PluginCommand::ListPlugins { callback_id } => {
                self.handle_list_plugins(callback_id);
            }
            // When plugins feature is disabled, these commands are no-ops
            #[cfg(not(feature = "plugins"))]
            PluginCommand::LoadPlugin { .. }
            | PluginCommand::UnloadPlugin { .. }
            | PluginCommand::ReloadPlugin { .. }
            | PluginCommand::ListPlugins { .. } => {
                tracing::warn!("Plugin management commands require the 'plugins' feature");
            }

            // ==================== Terminal Commands ====================
            PluginCommand::CreateTerminal {
                cwd,
                direction,
                ratio,
                focus,
                persistent,
                request_id,
            } => {
                self.handle_create_terminal(cwd, direction, ratio, focus, persistent, request_id);
            }

            PluginCommand::SendTerminalInput { terminal_id, data } => {
                self.handle_send_terminal_input(terminal_id, data);
            }

            PluginCommand::CloseTerminal { terminal_id } => {
                self.handle_close_terminal(terminal_id);
            }

            PluginCommand::GrepProject {
                pattern,
                fixed_string,
                case_sensitive,
                max_results,
                whole_words,
                callback_id,
            } => {
                self.handle_grep_project(
                    pattern,
                    fixed_string,
                    case_sensitive,
                    max_results,
                    whole_words,
                    callback_id,
                );
            }

            PluginCommand::GrepProjectStreaming {
                pattern,
                fixed_string,
                case_sensitive,
                max_results,
                whole_words,
                search_id,
                callback_id,
            } => {
                self.handle_grep_project_streaming(
                    pattern,
                    fixed_string,
                    case_sensitive,
                    max_results,
                    whole_words,
                    search_id,
                    callback_id,
                );
            }

            PluginCommand::ReplaceInBuffer {
                file_path,
                matches,
                replacement,
                callback_id,
            } => {
                self.handle_replace_in_buffer(file_path, matches, replacement, callback_id);
            }
        }
        Ok(())
    }

    /// Save a buffer to a specific file path (for :w filename)
    fn handle_save_buffer_to_path(&mut self, buffer_id: BufferId, path: std::path::PathBuf) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            // Save to the specified path
            match state.buffer.save_to_file(&path) {
                Ok(()) => {
                    // save_to_file already updates file_path internally via finalize_save
                    // Run on-save actions (formatting, etc.)
                    if let Err(e) = self.finalize_save(Some(path)) {
                        tracing::warn!("Failed to finalize save: {}", e);
                    }
                    tracing::debug!("Saved buffer {:?} to path", buffer_id);
                }
                Err(e) => {
                    self.handle_set_status(format!("Error saving: {}", e));
                    tracing::error!("Failed to save buffer to path: {}", e);
                }
            }
        } else {
            self.handle_set_status(format!("Buffer {:?} not found", buffer_id));
            tracing::warn!("SaveBufferToPath: buffer {:?} not found", buffer_id);
        }
    }

    /// Load a plugin from a file path
    #[cfg(feature = "plugins")]
    fn handle_load_plugin(&mut self, path: std::path::PathBuf, callback_id: JsCallbackId) {
        match self.plugin_manager.load_plugin(&path) {
            Ok(()) => {
                tracing::info!("Loaded plugin from {:?}", path);
                self.plugin_manager
                    .resolve_callback(callback_id, "true".to_string());
            }
            Err(e) => {
                tracing::error!("Failed to load plugin from {:?}: {}", path, e);
                self.plugin_manager
                    .reject_callback(callback_id, format!("{}", e));
            }
        }
    }

    /// Unload a plugin by name
    #[cfg(feature = "plugins")]
    fn handle_unload_plugin(&mut self, name: String, callback_id: JsCallbackId) {
        match self.plugin_manager.unload_plugin(&name) {
            Ok(()) => {
                tracing::info!("Unloaded plugin: {}", name);
                self.plugin_manager
                    .resolve_callback(callback_id, "true".to_string());
            }
            Err(e) => {
                tracing::error!("Failed to unload plugin '{}': {}", name, e);
                self.plugin_manager
                    .reject_callback(callback_id, format!("{}", e));
            }
        }
    }

    /// Reload a plugin by name
    #[cfg(feature = "plugins")]
    fn handle_reload_plugin(&mut self, name: String, callback_id: JsCallbackId) {
        match self.plugin_manager.reload_plugin(&name) {
            Ok(()) => {
                tracing::info!("Reloaded plugin: {}", name);
                self.plugin_manager
                    .resolve_callback(callback_id, "true".to_string());
            }
            Err(e) => {
                tracing::error!("Failed to reload plugin '{}': {}", name, e);
                self.plugin_manager
                    .reject_callback(callback_id, format!("{}", e));
            }
        }
    }

    /// List all loaded plugins
    #[cfg(feature = "plugins")]
    fn handle_list_plugins(&mut self, callback_id: JsCallbackId) {
        let plugins = self.plugin_manager.list_plugins();
        // Serialize to JSON array of { name, path, enabled }
        let json_array: Vec<serde_json::Value> = plugins
            .iter()
            .map(|p| {
                serde_json::json!({
                    "name": p.name,
                    "path": p.path.to_string_lossy(),
                    "enabled": p.enabled
                })
            })
            .collect();
        let json_str = serde_json::to_string(&json_array).unwrap_or_else(|_| "[]".to_string());
        self.plugin_manager.resolve_callback(callback_id, json_str);
    }

    /// Execute an editor action by name (for vi mode plugin)
    fn handle_execute_action(&mut self, action_name: String) {
        use crate::input::keybindings::Action;
        use std::collections::HashMap;

        // Parse the action name into an Action enum
        if let Some(action) = Action::from_str(&action_name, &HashMap::new()) {
            // Execute the action
            if let Err(e) = self.handle_action(action) {
                tracing::warn!("Failed to execute action '{}': {}", action_name, e);
            } else {
                tracing::debug!("Executed action: {}", action_name);
            }
        } else {
            tracing::warn!("Unknown action: {}", action_name);
        }
    }

    /// Execute multiple actions in sequence, each with an optional repeat count
    /// Used by vi mode for count prefix (e.g., "3dw" = delete 3 words)
    fn handle_execute_actions(&mut self, actions: Vec<fresh_core::api::ActionSpec>) {
        use crate::input::keybindings::Action;
        use std::collections::HashMap;

        for action_spec in actions {
            if let Some(action) = Action::from_str(&action_spec.action, &HashMap::new()) {
                // Execute the action `count` times
                for _ in 0..action_spec.count {
                    if let Err(e) = self.handle_action(action.clone()) {
                        tracing::warn!("Failed to execute action '{}': {}", action_spec.action, e);
                        return; // Stop on first error
                    }
                }
                tracing::debug!(
                    "Executed action '{}' {} time(s)",
                    action_spec.action,
                    action_spec.count
                );
            } else {
                tracing::warn!("Unknown action: {}", action_spec.action);
                return; // Stop on unknown action
            }
        }
    }

    /// Get text from a buffer range (for vi mode yank operations)
    fn handle_get_buffer_text(
        &mut self,
        buffer_id: BufferId,
        start: usize,
        end: usize,
        request_id: u64,
    ) {
        let result = if let Some(state) = self.buffers.get_mut(&buffer_id) {
            // Get text from the buffer using the mutable get_text_range method
            let len = state.buffer.len();
            if start <= end && end <= len {
                Ok(state.get_text_range(start, end))
            } else {
                Err(format!(
                    "Invalid range {}..{} for buffer of length {}",
                    start, end, len
                ))
            }
        } else {
            Err(format!("Buffer {:?} not found", buffer_id))
        };

        // Resolve the JavaScript Promise callback directly
        let callback_id = fresh_core::api::JsCallbackId::from(request_id);
        match result {
            Ok(text) => {
                // Serialize text as JSON string
                let json = serde_json::to_string(&text).unwrap_or_else(|_| "null".to_string());
                self.plugin_manager.resolve_callback(callback_id, json);
            }
            Err(error) => {
                self.plugin_manager.reject_callback(callback_id, error);
            }
        }
    }

    /// Set the global editor mode (for vi mode)
    fn handle_set_editor_mode(&mut self, mode: Option<String>) {
        self.editor_mode = mode.clone();
        tracing::debug!("Set editor mode: {:?}", mode);
    }

    /// Get the byte offset of the start of a line in the active buffer
    fn handle_get_line_start_position(&mut self, buffer_id: BufferId, line: u32, request_id: u64) {
        // Use active buffer if buffer_id is 0
        let actual_buffer_id = if buffer_id.0 == 0 {
            self.active_buffer_id()
        } else {
            buffer_id
        };

        let result = if let Some(state) = self.buffers.get_mut(&actual_buffer_id) {
            // Get line start position by iterating through the buffer content
            let line_number = line as usize;
            let buffer_len = state.buffer.len();

            if line_number == 0 {
                // First line always starts at 0
                Some(0)
            } else {
                // Count newlines to find the start of the requested line
                let mut current_line = 0;
                let mut line_start = None;

                // Read buffer content to find newlines using the BufferState's get_text_range
                let content = state.get_text_range(0, buffer_len);
                for (byte_idx, c) in content.char_indices() {
                    if c == '\n' {
                        current_line += 1;
                        if current_line == line_number {
                            // Found the start of the requested line (byte after newline)
                            line_start = Some(byte_idx + 1);
                            break;
                        }
                    }
                }
                line_start
            }
        } else {
            None
        };

        // Resolve the JavaScript Promise callback directly
        let callback_id = fresh_core::api::JsCallbackId::from(request_id);
        // Serialize as JSON (null for None, number for Some)
        let json = serde_json::to_string(&result).unwrap_or_else(|_| "null".to_string());
        self.plugin_manager.resolve_callback(callback_id, json);
    }

    /// Get the byte offset of the end of a line in the active buffer
    /// Returns the position after the last character of the line (before newline)
    fn handle_get_line_end_position(&mut self, buffer_id: BufferId, line: u32, request_id: u64) {
        // Use active buffer if buffer_id is 0
        let actual_buffer_id = if buffer_id.0 == 0 {
            self.active_buffer_id()
        } else {
            buffer_id
        };

        let result = if let Some(state) = self.buffers.get_mut(&actual_buffer_id) {
            let line_number = line as usize;
            let buffer_len = state.buffer.len();

            // Read buffer content to find line boundaries
            let content = state.get_text_range(0, buffer_len);
            let mut current_line = 0;
            let mut line_end = None;

            for (byte_idx, c) in content.char_indices() {
                if c == '\n' {
                    if current_line == line_number {
                        // Found the end of the requested line (position of newline)
                        line_end = Some(byte_idx);
                        break;
                    }
                    current_line += 1;
                }
            }

            // Handle last line (no trailing newline)
            if line_end.is_none() && current_line == line_number {
                line_end = Some(buffer_len);
            }

            line_end
        } else {
            None
        };

        let callback_id = fresh_core::api::JsCallbackId::from(request_id);
        let json = serde_json::to_string(&result).unwrap_or_else(|_| "null".to_string());
        self.plugin_manager.resolve_callback(callback_id, json);
    }

    /// Get the total number of lines in a buffer
    fn handle_get_buffer_line_count(&mut self, buffer_id: BufferId, request_id: u64) {
        // Use active buffer if buffer_id is 0
        let actual_buffer_id = if buffer_id.0 == 0 {
            self.active_buffer_id()
        } else {
            buffer_id
        };

        let result = if let Some(state) = self.buffers.get_mut(&actual_buffer_id) {
            let buffer_len = state.buffer.len();
            let content = state.get_text_range(0, buffer_len);

            // Count lines (number of newlines + 1, unless empty)
            if content.is_empty() {
                Some(1) // Empty buffer has 1 line
            } else {
                let newline_count = content.chars().filter(|&c| c == '\n').count();
                // If file ends with newline, don't count extra line
                let ends_with_newline = content.ends_with('\n');
                if ends_with_newline {
                    Some(newline_count)
                } else {
                    Some(newline_count + 1)
                }
            }
        } else {
            None
        };

        let callback_id = fresh_core::api::JsCallbackId::from(request_id);
        let json = serde_json::to_string(&result).unwrap_or_else(|_| "null".to_string());
        self.plugin_manager.resolve_callback(callback_id, json);
    }

    /// Scroll a split to center a specific line in the viewport
    fn handle_scroll_to_line_center(
        &mut self,
        split_id: SplitId,
        buffer_id: BufferId,
        line: usize,
    ) {
        // Use active split if split_id is 0
        let actual_split_id = if split_id.0 == 0 {
            self.split_manager.active_split()
        } else {
            LeafId(split_id)
        };

        // Use active buffer if buffer_id is 0
        let actual_buffer_id = if buffer_id.0 == 0 {
            self.active_buffer()
        } else {
            buffer_id
        };

        // Get viewport height
        let viewport_height = if let Some(view_state) = self.split_view_states.get(&actual_split_id)
        {
            view_state.viewport.height as usize
        } else {
            return;
        };

        // Calculate the target line to scroll to (center the requested line)
        let lines_above = viewport_height / 2;
        let target_line = line.saturating_sub(lines_above);

        // Get the buffer and scroll
        if let Some(state) = self.buffers.get_mut(&actual_buffer_id) {
            let buffer = &mut state.buffer;
            if let Some(view_state) = self.split_view_states.get_mut(&actual_split_id) {
                view_state.viewport.scroll_to(buffer, target_line);
                // Mark to skip ensure_visible on next render so the scroll isn't undone
                view_state.viewport.set_skip_ensure_visible();
            }
        }
    }

    /// Scroll every split whose active buffer is `buffer_id` so that
    /// `line` is within the viewport. Used by plugin panels (buffer
    /// groups) whose plugin-side "selected row" doesn't drive the
    /// buffer cursor — after updating the selection, the plugin calls
    /// this to bring the selected row into view.
    ///
    /// Walks both the main split tree's leaves AND the inner leaves of
    /// all Grouped subtrees stored in `grouped_subtrees`, because the
    /// latter are not represented in `split_manager`'s tree.
    fn handle_scroll_buffer_to_line(&mut self, buffer_id: BufferId, line: usize) {
        if !self.buffers.contains_key(&buffer_id) {
            return;
        }

        // Collect the leaf ids whose active buffer is `buffer_id`.
        let mut target_leaves: Vec<LeafId> = Vec::new();

        // Main tree: walk its leaves.
        for leaf_id in self.split_manager.root().leaf_split_ids() {
            if let Some(vs) = self.split_view_states.get(&leaf_id) {
                if vs.active_buffer == buffer_id {
                    target_leaves.push(leaf_id);
                }
            }
        }

        // Grouped subtrees: walk each group's inner leaves.
        for (_group_leaf_id, node) in self.grouped_subtrees.iter() {
            if let crate::view::split::SplitNode::Grouped { layout, .. } = node {
                for inner_leaf in layout.leaf_split_ids() {
                    if let Some(vs) = self.split_view_states.get(&inner_leaf) {
                        if vs.active_buffer == buffer_id && !target_leaves.contains(&inner_leaf) {
                            target_leaves.push(inner_leaf);
                        }
                    }
                }
            }
        }

        if target_leaves.is_empty() {
            return;
        }

        let state = match self.buffers.get_mut(&buffer_id) {
            Some(s) => s,
            None => return,
        };

        for leaf_id in target_leaves {
            let Some(view_state) = self.split_view_states.get_mut(&leaf_id) else {
                continue;
            };
            let viewport_height = view_state.viewport.height as usize;
            // Place `line` roughly a third of the viewport from the top so
            // the next few navigation steps don't immediately scroll again.
            let lines_above = viewport_height / 3;
            let target = line.saturating_sub(lines_above);
            view_state.viewport.scroll_to(&mut state.buffer, target);
            view_state.viewport.set_skip_ensure_visible();
        }
    }

    fn handle_spawn_host_process(
        &mut self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
        callback_id: JsCallbackId,
    ) {
        // Bypass the active authority on purpose: this is
        // reserved for plugin internals that must run host-side
        // work (e.g. `devcontainer up`) before the authority
        // they want is even built. Uses the same callback shape
        // as `SpawnProcess` so the plugin-facing API is
        // symmetric.
        //
        // Kill handle: we store a oneshot sender in
        // `host_process_handles` keyed by the callback id. A
        // `KillHostProcess` dispatch sends on it; the spawn
        // task's `tokio::select!` then start_kill()s the
        // child. This lets a plugin cancel a long-running
        // spawn (e.g. "Cancel Startup" on the Remote
        // Indicator popup during `devcontainer up`).
        if let (Some(runtime), Some(bridge)) = (&self.tokio_runtime, &self.async_bridge) {
            use tokio::io::{AsyncReadExt, BufReader};
            use tokio::process::Command as TokioCommand;

            let effective_cwd = cwd.or_else(|| {
                std::env::current_dir()
                    .map(|p| p.to_string_lossy().to_string())
                    .ok()
            });
            let sender = bridge.sender();
            let process_id = callback_id.as_u64();

            let (kill_tx, mut kill_rx) = tokio::sync::oneshot::channel::<()>();
            self.host_process_handles.insert(process_id, kill_tx);

            runtime.spawn(async move {
                let mut cmd = TokioCommand::new(&command);
                cmd.args(&args);
                cmd.stdout(std::process::Stdio::piped());
                cmd.stderr(std::process::Stdio::piped());
                if let Some(ref dir) = effective_cwd {
                    cmd.current_dir(dir);
                }
                let mut child = match cmd.spawn() {
                    Ok(c) => c,
                    Err(e) => {
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = sender.send(AsyncMessage::PluginProcessOutput {
                            process_id,
                            stdout: String::new(),
                            stderr: e.to_string(),
                            exit_code: -1,
                        });
                        return;
                    }
                };

                // Take the pipes out of the Child so the
                // reader tasks own them; then `child.wait()`
                // has exclusive mutable access for the
                // kill-or-exit select. Matches the
                // fresh-plugin-runtime process.rs pattern.
                let stdout_pipe = child.stdout.take();
                let stderr_pipe = child.stderr.take();

                let stdout_fut = async {
                    let mut buf = String::new();
                    if let Some(s) = stdout_pipe {
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = BufReader::new(s).read_to_string(&mut buf).await;
                    }
                    buf
                };
                let stderr_fut = async {
                    let mut buf = String::new();
                    if let Some(s) = stderr_pipe {
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = BufReader::new(s).read_to_string(&mut buf).await;
                    }
                    buf
                };
                let wait_fut = async {
                    tokio::select! {
                        status = child.wait() => {
                            status.map(|s| s.code().unwrap_or(-1)).unwrap_or(-1)
                        }
                        _ = &mut kill_rx => {
                            // Best-effort SIGKILL + reap.
                            // Children of the killed
                            // process may leak (Q-C2).
                            #[allow(clippy::let_underscore_must_use)]
                            let _ = child.start_kill();
                            child
                                .wait()
                                .await
                                .map(|s| s.code().unwrap_or(-1))
                                .unwrap_or(-1)
                        }
                    }
                };
                let (stdout, stderr, exit_code) = tokio::join!(stdout_fut, stderr_fut, wait_fut);

                #[allow(clippy::let_underscore_must_use)]
                let _ = sender.send(AsyncMessage::PluginProcessOutput {
                    process_id,
                    stdout,
                    stderr,
                    exit_code,
                });
            });
        } else {
            self.plugin_manager
                .reject_callback(callback_id, "Async runtime not available".to_string());
        }
    }

    fn handle_spawn_background_process(
        &mut self,
        process_id: u64,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
        callback_id: JsCallbackId,
    ) {
        // Spawn background process with streaming output via tokio
        if let (Some(runtime), Some(bridge)) = (&self.tokio_runtime, &self.async_bridge) {
            use tokio::io::{AsyncBufReadExt, BufReader};
            use tokio::process::Command as TokioCommand;

            let effective_cwd = cwd.unwrap_or_else(|| {
                std::env::current_dir()
                    .map(|p| p.to_string_lossy().to_string())
                    .unwrap_or_else(|_| ".".to_string())
            });

            let sender = bridge.sender();
            let sender_stdout = sender.clone();
            let sender_stderr = sender.clone();
            let callback_id_u64 = callback_id.as_u64();

            // Receiver may be dropped if editor is shutting down
            #[allow(clippy::let_underscore_must_use)]
            let handle = runtime.spawn(async move {
                let mut child = match TokioCommand::new(&command)
                    .args(&args)
                    .current_dir(&effective_cwd)
                    .stdout(std::process::Stdio::piped())
                    .stderr(std::process::Stdio::piped())
                    .spawn()
                {
                    Ok(child) => child,
                    Err(e) => {
                        let _ = sender.send(crate::services::async_bridge::AsyncMessage::Plugin(
                            fresh_core::api::PluginAsyncMessage::ProcessExit {
                                process_id,
                                callback_id: callback_id_u64,
                                exit_code: -1,
                            },
                        ));
                        tracing::error!("Failed to spawn background process: {}", e);
                        return;
                    }
                };

                // Stream stdout
                let stdout = child.stdout.take();
                let stderr = child.stderr.take();
                let pid = process_id;

                // Spawn stdout reader
                if let Some(stdout) = stdout {
                    let sender = sender_stdout;
                    tokio::spawn(async move {
                        let reader = BufReader::new(stdout);
                        let mut lines = reader.lines();
                        while let Ok(Some(line)) = lines.next_line().await {
                            let _ =
                                sender.send(crate::services::async_bridge::AsyncMessage::Plugin(
                                    fresh_core::api::PluginAsyncMessage::ProcessStdout {
                                        process_id: pid,
                                        data: line + "\n",
                                    },
                                ));
                        }
                    });
                }

                // Spawn stderr reader
                if let Some(stderr) = stderr {
                    let sender = sender_stderr;
                    tokio::spawn(async move {
                        let reader = BufReader::new(stderr);
                        let mut lines = reader.lines();
                        while let Ok(Some(line)) = lines.next_line().await {
                            let _ =
                                sender.send(crate::services::async_bridge::AsyncMessage::Plugin(
                                    fresh_core::api::PluginAsyncMessage::ProcessStderr {
                                        process_id: pid,
                                        data: line + "\n",
                                    },
                                ));
                        }
                    });
                }

                // Wait for process to complete
                let exit_code = match child.wait().await {
                    Ok(status) => status.code().unwrap_or(-1),
                    Err(_) => -1,
                };

                let _ = sender.send(crate::services::async_bridge::AsyncMessage::Plugin(
                    fresh_core::api::PluginAsyncMessage::ProcessExit {
                        process_id,
                        callback_id: callback_id_u64,
                        exit_code,
                    },
                ));
            });

            // Store abort handle for potential kill
            self.background_process_handles
                .insert(process_id, handle.abort_handle());
        } else {
            // No runtime - reject immediately
            self.plugin_manager
                .reject_callback(callback_id, "Async runtime not available".to_string());
        }
    }

    fn handle_create_virtual_buffer_with_content(
        &mut self,
        name: String,
        mode: String,
        read_only: bool,
        entries: Vec<fresh_core::text_property::TextPropertyEntry>,
        show_line_numbers: bool,
        show_cursors: bool,
        editing_disabled: bool,
        hidden_from_tabs: bool,
        request_id: Option<u64>,
    ) {
        let buffer_id = self.create_virtual_buffer(name.clone(), mode.clone(), read_only);
        tracing::info!(
            "Created virtual buffer '{}' with mode '{}' (id={:?})",
            name,
            mode,
            buffer_id
        );

        // Apply view options to the buffer
        // TODO: show_line_numbers is duplicated between EditorState.margins and
        // BufferViewState. The renderer reads BufferViewState and overwrites
        // margins each frame via configure_for_line_numbers(), making the margin
        // setting here effectively write-only. Consider removing the margin call
        // and only setting BufferViewState.show_line_numbers.
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.margins.configure_for_line_numbers(show_line_numbers);
            state.show_cursors = show_cursors;
            state.editing_disabled = editing_disabled;
            tracing::debug!(
                        "Set buffer {:?} view options: show_line_numbers={}, show_cursors={}, editing_disabled={}",
                        buffer_id,
                        show_line_numbers,
                        show_cursors,
                        editing_disabled
                    );
        }
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.ensure_buffer_state(buffer_id).show_line_numbers = show_line_numbers;
        }

        // Apply hidden_from_tabs to buffer metadata
        if hidden_from_tabs {
            if let Some(meta) = self.buffer_metadata.get_mut(&buffer_id) {
                meta.hidden_from_tabs = true;
            }
        }

        // Now set the content
        match self.set_virtual_buffer_content(buffer_id, entries) {
            Ok(()) => {
                tracing::debug!("Set virtual buffer content for {:?}", buffer_id);
                // Switch to the new buffer to display it
                self.set_active_buffer(buffer_id);
                tracing::debug!("Switched to virtual buffer {:?}", buffer_id);

                // Send response if request_id is present
                if let Some(req_id) = request_id {
                    tracing::info!(
                                "CreateVirtualBufferWithContent: resolving callback for request_id={}, buffer_id={:?}",
                                req_id,
                                buffer_id
                            );
                    // createVirtualBuffer returns VirtualBufferResult: { bufferId, splitId }
                    let result = fresh_core::api::VirtualBufferResult {
                        buffer_id: buffer_id.0 as u64,
                        split_id: None,
                    };
                    self.plugin_manager.resolve_callback(
                        fresh_core::api::JsCallbackId::from(req_id),
                        serde_json::to_string(&result).unwrap_or_default(),
                    );
                    tracing::info!(
                        "CreateVirtualBufferWithContent: resolve_callback sent for request_id={}",
                        req_id
                    );
                }
            }
            Err(e) => {
                tracing::error!("Failed to set virtual buffer content: {}", e);
            }
        }
    }

    fn handle_create_virtual_buffer_in_split(
        &mut self,
        name: String,
        mode: String,
        read_only: bool,
        entries: Vec<fresh_core::text_property::TextPropertyEntry>,
        ratio: f32,
        direction: Option<String>,
        panel_id: Option<String>,
        show_line_numbers: bool,
        show_cursors: bool,
        editing_disabled: bool,
        line_wrap: Option<bool>,
        before: bool,
        request_id: Option<u64>,
    ) {
        // Check if this panel already exists (for idempotent operations)
        if let Some(pid) = &panel_id {
            if let Some(&existing_buffer_id) = self.panel_ids.get(pid) {
                // Verify the buffer actually exists (defensive check for stale entries)
                if self.buffers.contains_key(&existing_buffer_id) {
                    // Panel exists, just update its content
                    if let Err(e) = self.set_virtual_buffer_content(existing_buffer_id, entries) {
                        tracing::error!("Failed to update panel content: {}", e);
                    } else {
                        tracing::info!("Updated existing panel '{}' content", pid);
                    }

                    // Find and focus the split that contains this buffer
                    let splits = self.split_manager.splits_for_buffer(existing_buffer_id);
                    if let Some(&split_id) = splits.first() {
                        self.split_manager.set_active_split(split_id);
                        // Route through set_pane_buffer so tree + SVS
                        // stay consistent (issue #1620 invariant).
                        self.set_pane_buffer(split_id, existing_buffer_id);
                        tracing::debug!("Focused split {:?} containing panel buffer", split_id);
                    }

                    // Send response with existing buffer ID and split ID via callback resolution
                    if let Some(req_id) = request_id {
                        let result = fresh_core::api::VirtualBufferResult {
                            buffer_id: existing_buffer_id.0 as u64,
                            split_id: splits.first().map(|s| s.0 .0 as u64),
                        };
                        self.plugin_manager.resolve_callback(
                            fresh_core::api::JsCallbackId::from(req_id),
                            serde_json::to_string(&result).unwrap_or_default(),
                        );
                    }
                    return;
                } else {
                    // Buffer no longer exists, remove stale panel_id entry
                    tracing::warn!(
                        "Removing stale panel_id '{}' pointing to non-existent buffer {:?}",
                        pid,
                        existing_buffer_id
                    );
                    self.panel_ids.remove(pid);
                    // Fall through to create a new buffer
                }
            }
        }

        // Capture the source split before creating the buffer —
        // `create_virtual_buffer` unconditionally adds the new buffer
        // as a tab to the currently active split, which is the wrong
        // thing for a panel that lives in its own dedicated split
        // (it would show up as a tab in BOTH splits — see bug #3).
        let source_split_before_create = self.split_manager.active_split();

        // Create the virtual buffer first
        let buffer_id = self.create_virtual_buffer(name.clone(), mode.clone(), read_only);
        tracing::info!(
            "Created virtual buffer '{}' with mode '{}' in split (id={:?})",
            name,
            mode,
            buffer_id
        );

        // Apply view options to the buffer
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.margins.configure_for_line_numbers(show_line_numbers);
            state.show_cursors = show_cursors;
            state.editing_disabled = editing_disabled;
            tracing::debug!(
                        "Set buffer {:?} view options: show_line_numbers={}, show_cursors={}, editing_disabled={}",
                        buffer_id,
                        show_line_numbers,
                        show_cursors,
                        editing_disabled
                    );
        }

        // Store the panel ID mapping if provided
        if let Some(pid) = panel_id {
            self.panel_ids.insert(pid, buffer_id);
        }

        // Set the content
        if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries) {
            tracing::error!("Failed to set virtual buffer content: {}", e);
            return;
        }

        // Determine split direction
        let split_dir = match direction.as_deref() {
            Some("vertical") => crate::model::event::SplitDirection::Vertical,
            _ => crate::model::event::SplitDirection::Horizontal,
        };

        // Create a split with the new buffer
        let created_split_id = match self
            .split_manager
            .split_active_positioned(split_dir, buffer_id, ratio, before)
        {
            Ok(new_split_id) => {
                // The buffer now lives in its own split, so drop its
                // tab from the source split (see bug #3).  Only do
                // this when the new split actually differs from the
                // source split — otherwise we'd leave no split
                // displaying the buffer.
                if new_split_id != source_split_before_create {
                    if let Some(source_view_state) =
                        self.split_view_states.get_mut(&source_split_before_create)
                    {
                        source_view_state.remove_buffer(buffer_id);
                    }
                }
                // Create independent view state for the new split with the buffer in tabs
                let mut view_state = SplitViewState::with_buffer(
                    self.terminal_width,
                    self.terminal_height,
                    buffer_id,
                );
                view_state.apply_config_defaults(
                    self.config.editor.line_numbers,
                    self.config.editor.highlight_current_line,
                    line_wrap.unwrap_or_else(|| self.resolve_line_wrap_for_buffer(buffer_id)),
                    self.config.editor.wrap_indent,
                    self.resolve_wrap_column_for_buffer(buffer_id),
                    self.config.editor.rulers.clone(),
                );
                // Override with plugin-requested show_line_numbers
                view_state.ensure_buffer_state(buffer_id).show_line_numbers = show_line_numbers;
                self.split_view_states.insert(new_split_id, view_state);

                // Focus the new split (the diagnostics panel)
                self.split_manager.set_active_split(new_split_id);
                // NOTE: split tree was updated by split_active, active_buffer derives from it

                tracing::info!(
                    "Created {:?} split with virtual buffer {:?}",
                    split_dir,
                    buffer_id
                );
                Some(new_split_id)
            }
            Err(e) => {
                tracing::error!("Failed to create split: {}", e);
                // Fall back to just switching to the buffer
                self.set_active_buffer(buffer_id);
                None
            }
        };

        // Send response with buffer ID and split ID via callback resolution
        // NOTE: Using VirtualBufferResult type for type-safe JSON serialization
        if let Some(req_id) = request_id {
            tracing::trace!("CreateVirtualBufferInSplit: resolving callback for request_id={}, buffer_id={:?}, split_id={:?}", req_id, buffer_id, created_split_id);
            let result = fresh_core::api::VirtualBufferResult {
                buffer_id: buffer_id.0 as u64,
                split_id: created_split_id.map(|s| s.0 .0 as u64),
            };
            self.plugin_manager.resolve_callback(
                fresh_core::api::JsCallbackId::from(req_id),
                serde_json::to_string(&result).unwrap_or_default(),
            );
        }
    }

    fn handle_create_virtual_buffer_in_existing_split(
        &mut self,
        name: String,
        mode: String,
        read_only: bool,
        entries: Vec<fresh_core::text_property::TextPropertyEntry>,
        split_id: SplitId,
        show_line_numbers: bool,
        show_cursors: bool,
        editing_disabled: bool,
        line_wrap: Option<bool>,
        request_id: Option<u64>,
    ) {
        // Create the virtual buffer
        let buffer_id = self.create_virtual_buffer(name.clone(), mode.clone(), read_only);
        tracing::info!(
            "Created virtual buffer '{}' with mode '{}' for existing split {:?} (id={:?})",
            name,
            mode,
            split_id,
            buffer_id
        );

        // Apply view options to the buffer
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.margins.configure_for_line_numbers(show_line_numbers);
            state.show_cursors = show_cursors;
            state.editing_disabled = editing_disabled;
        }

        // Set the content
        if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries) {
            tracing::error!("Failed to set virtual buffer content: {}", e);
            return;
        }

        // Show the buffer in the target split. set_pane_buffer
        // covers the tree + SVS updates the old code did by hand.
        let leaf_id = LeafId(split_id);
        self.split_manager.set_active_split(leaf_id);
        self.set_pane_buffer(leaf_id, buffer_id);

        // Fall-through to the cursor/open_buffers housekeeping
        // that used to follow the manual switch_buffer. We keep
        // the `if let Some(view_state)` block below — set_pane_buffer
        // already called switch_buffer, but the downstream code
        // also nudges open_buffers and focus_history.
        if let Some(view_state) = self.split_view_states.get_mut(&leaf_id) {
            view_state.switch_buffer(buffer_id);
            view_state.add_buffer(buffer_id);
            view_state.ensure_buffer_state(buffer_id).show_line_numbers = show_line_numbers;

            // Apply line_wrap setting if provided
            if let Some(wrap) = line_wrap {
                view_state.active_state_mut().viewport.line_wrap_enabled = wrap;
            }
        }

        tracing::info!(
            "Displayed virtual buffer {:?} in split {:?}",
            buffer_id,
            split_id
        );

        // Send response with buffer ID and split ID via callback resolution
        if let Some(req_id) = request_id {
            let result = fresh_core::api::VirtualBufferResult {
                buffer_id: buffer_id.0 as u64,
                split_id: Some(split_id.0 as u64),
            };
            self.plugin_manager.resolve_callback(
                fresh_core::api::JsCallbackId::from(req_id),
                serde_json::to_string(&result).unwrap_or_default(),
            );
        }
    }

    fn handle_show_action_popup(
        &mut self,
        popup_id: String,
        title: String,
        message: String,
        actions: Vec<fresh_core::api::ActionPopupAction>,
    ) {
        tracing::info!(
            "Action popup requested: id={}, title={}, actions={}",
            popup_id,
            title,
            actions.len()
        );

        // Build popup list items from actions
        let items: Vec<crate::model::event::PopupListItemData> = actions
            .iter()
            .map(|action| crate::model::event::PopupListItemData {
                text: action.label.clone(),
                detail: None,
                icon: None,
                data: Some(action.id.clone()),
            })
            .collect();

        // The popup_id lives on the popup itself via its
        // `PopupResolver::PluginAction` — no side-channel stack.
        // Drop the incoming `actions` vec; its ids are already
        // encoded as each list item's `data` field below.
        drop(actions);

        // Create popup with message + action list
        let popup_data = crate::model::event::PopupData {
            kind: crate::model::event::PopupKindHint::List,
            title: Some(title),
            description: Some(message),
            transient: false,
            content: crate::model::event::PopupContentData::List { items, selected: 0 },
            position: crate::model::event::PopupPositionData::BottomRight,
            width: 60,
            max_height: 15,
            bordered: true,
        };

        // Action popups are buffer-independent notifications; route
        // them to the editor-level popup stack so they remain visible
        // (and dismissible) regardless of which buffer is focused —
        // including virtual buffers like the Dashboard that own the
        // whole split.
        //
        // The resolver carries the popup_id so confirm/cancel fires
        // `action_popup_result` for exactly THIS popup, even when
        // multiple plugin popups are stacked concurrently.
        let mut popup_obj = crate::state::convert_popup_data_to_popup(&popup_data);
        popup_obj.resolver = crate::view::popup::PopupResolver::PluginAction {
            popup_id: popup_id.clone(),
        };
        self.global_popups.show(popup_obj);
        tracing::info!(
            "Action popup shown: id={}, stack_depth={}",
            popup_id,
            self.global_popups.all().len()
        );
    }

    fn handle_create_terminal(
        &mut self,
        cwd: Option<String>,
        direction: Option<String>,
        ratio: Option<f32>,
        focus: Option<bool>,
        persistent: bool,
        request_id: u64,
    ) {
        let (cols, rows) = self.get_terminal_dimensions();

        // Set up async bridge for terminal manager if not already done
        if let Some(ref bridge) = self.async_bridge {
            self.terminal_manager.set_async_bridge(bridge.clone());
        }

        // Determine working directory
        let working_dir = cwd
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|| self.working_dir.clone());

        // Prepare persistent storage paths
        let terminal_root = self.dir_context.terminal_dir_for(&working_dir);
        if let Err(e) = self.authority.filesystem.create_dir_all(&terminal_root) {
            tracing::warn!("Failed to create terminal directory: {}", e);
        }
        let predicted_terminal_id = self.terminal_manager.next_terminal_id();
        // Ephemeral terminals get a per-spawn suffix on their backing
        // files so there is no possibility of picking up the scrollback
        // that a previous run (with the same numeric terminal ID) wrote
        // to `fresh-terminal-N.{txt,log}`. Persistent terminals keep
        // the stable `fresh-terminal-N.*` name so workspace restore
        // can still find them.
        let name_stem = if persistent {
            format!("fresh-terminal-{}", predicted_terminal_id.0)
        } else {
            let nanos = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_nanos())
                .unwrap_or(0);
            format!("fresh-terminal-eph-{}-{}", predicted_terminal_id.0, nanos)
        };
        let log_path = terminal_root.join(format!("{}.log", name_stem));
        let backing_path = terminal_root.join(format!("{}.txt", name_stem));
        self.terminal_backing_files
            .insert(predicted_terminal_id, backing_path);
        let backing_path_for_spawn = self
            .terminal_backing_files
            .get(&predicted_terminal_id)
            .cloned();

        match self.terminal_manager.spawn(
            cols,
            rows,
            Some(working_dir),
            Some(log_path.clone()),
            backing_path_for_spawn,
            self.resolved_terminal_wrapper(),
        ) {
            Ok(terminal_id) => {
                // Track log file path
                self.terminal_log_files
                    .insert(terminal_id, log_path.clone());
                // Fix up backing path if the predicted ID didn't match
                // the one the terminal manager handed out. Persistent
                // terminals re-derive the stable `fresh-terminal-N.txt`
                // name so the workspace restore path can find them;
                // ephemeral terminals just keep the already-spawned
                // file (it has a nanos-unique name either way) and
                // rebind the HashMap key to the real ID.
                if terminal_id != predicted_terminal_id {
                    let existing = self.terminal_backing_files.remove(&predicted_terminal_id);
                    let fixed_backing = if persistent {
                        terminal_root.join(format!("fresh-terminal-{}.txt", terminal_id.0))
                    } else {
                        existing.unwrap_or_else(|| terminal_root.join(format!("{}.txt", name_stem)))
                    };
                    self.terminal_backing_files
                        .insert(terminal_id, fixed_backing);
                }
                if !persistent {
                    self.ephemeral_terminals.insert(terminal_id);
                }

                // Pick buffer-attachment strategy based on whether the
                // plugin asked for its own split:
                //
                // - direction = Some: use `_detached` so the buffer
                //   isn't also added as a tab to the user's active
                //   split. The new split below owns it exclusively,
                //   so when the user closes that split the terminal
                //   disappears entirely instead of leaving a ghost
                //   tab behind in the main split.
                // - direction = None: use `_attached` — the plugin
                //   is intentionally placing the terminal as a new
                //   tab in the active split, which is the whole
                //   point of the no-split branch.
                let active_split = self.split_manager.active_split();
                let buffer_id = if direction.is_some() {
                    self.create_terminal_buffer_detached(terminal_id)
                } else {
                    self.create_terminal_buffer_attached(terminal_id, active_split)
                };

                let created_split_id = if let Some(dir_str) = direction.as_deref() {
                    let split_dir = match dir_str {
                        "horizontal" => crate::model::event::SplitDirection::Horizontal,
                        _ => crate::model::event::SplitDirection::Vertical,
                    };

                    let split_ratio = ratio.unwrap_or(0.5);
                    match self
                        .split_manager
                        .split_active(split_dir, buffer_id, split_ratio)
                    {
                        Ok(new_split_id) => {
                            let mut view_state = SplitViewState::with_buffer(
                                self.terminal_width,
                                self.terminal_height,
                                buffer_id,
                            );
                            view_state.apply_config_defaults(
                                self.config.editor.line_numbers,
                                self.config.editor.highlight_current_line,
                                false,
                                false,
                                None,
                                self.config.editor.rulers.clone(),
                            );
                            // Terminal output is ANSI-sequenced and
                            // assumes a fixed column count; wrapping
                            // would mangle cursor positioning.
                            view_state.viewport.line_wrap_enabled = false;
                            self.split_view_states.insert(new_split_id, view_state);

                            if focus.unwrap_or(true) {
                                self.split_manager.set_active_split(new_split_id);
                            }

                            tracing::info!(
                                "Created {:?} split for terminal {:?} with buffer {:?}",
                                split_dir,
                                terminal_id,
                                buffer_id
                            );
                            Some(new_split_id)
                        }
                        Err(e) => {
                            tracing::error!(
                                "Failed to create split for terminal: {}; \
                                         falling back to active split",
                                e
                            );
                            // The buffer was created detached. Split
                            // creation failed, so attach it to the
                            // active split as a graceful fallback
                            // rather than leaving an orphan buffer.
                            if let Some(view_state) = self.split_view_states.get_mut(&active_split)
                            {
                                view_state.add_buffer(buffer_id);
                                view_state.viewport.line_wrap_enabled = false;
                            }
                            self.set_active_buffer(buffer_id);
                            None
                        }
                    }
                } else {
                    // No split — just switch to the terminal buffer in the active split
                    self.set_active_buffer(buffer_id);
                    None
                };

                // Resize terminal to match actual split content area
                self.resize_visible_terminals();

                // Resolve the callback with TerminalResult
                let result = fresh_core::api::TerminalResult {
                    buffer_id: buffer_id.0 as u64,
                    terminal_id: terminal_id.0 as u64,
                    split_id: created_split_id.map(|s| s.0 .0 as u64),
                };
                self.plugin_manager.resolve_callback(
                    fresh_core::api::JsCallbackId::from(request_id),
                    serde_json::to_string(&result).unwrap_or_default(),
                );

                tracing::info!(
                    "Plugin created terminal {:?} with buffer {:?}",
                    terminal_id,
                    buffer_id
                );
            }
            Err(e) => {
                tracing::error!("Failed to create terminal for plugin: {}", e);
                self.plugin_manager.reject_callback(
                    fresh_core::api::JsCallbackId::from(request_id),
                    format!("Failed to create terminal: {}", e),
                );
            }
        }
    }
    // ==================== Extracted handlers for previously inline match arms ====================

    fn handle_get_split_by_label(&mut self, label: String, request_id: u64) {
        let split_id = self.split_manager.find_split_by_label(&label);
        let callback_id = fresh_core::api::JsCallbackId::from(request_id);
        let json =
            serde_json::to_string(&split_id.map(|s| s.0 .0)).unwrap_or_else(|_| "null".to_string());
        self.plugin_manager.resolve_callback(callback_id, json);
    }

    fn handle_set_buffer_show_cursors(&mut self, buffer_id: BufferId, show: bool) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.show_cursors = show;
        } else {
            tracing::warn!("SetBufferShowCursors: buffer {:?} not found", buffer_id);
        }
    }

    fn handle_override_theme_colors(
        &mut self,
        overrides: std::collections::HashMap<String, [u8; 3]>,
    ) {
        let pairs = overrides
            .into_iter()
            .map(|(k, [r, g, b])| (k, ratatui::style::Color::Rgb(r, g, b)));
        let applied = self.theme.override_colors(pairs);
        if applied > 0 {
            // Diagnostics / semantic overlays bake RGB at creation time — rebuild
            // them so the override is visible everywhere on the next frame.
            self.reapply_all_overlays();
        }
    }

    fn handle_await_next_key(&mut self, callback_id: fresh_core::api::JsCallbackId) {
        // If keys arrived during a key-capture window while no callback was
        // pending, drain the front-most buffered key and resolve immediately.
        // Otherwise enqueue the callback for the next live keypress.
        if let Some(payload) = self.pending_key_capture_buffer.pop_front() {
            let json = serde_json::to_string(&payload).unwrap_or_else(|_| "null".to_string());
            self.plugin_manager.resolve_callback(callback_id, json);
        } else {
            self.pending_next_key_callbacks.push_back(callback_id);
        }
    }

    fn handle_spawn_process(
        &mut self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
        callback_id: fresh_core::api::JsCallbackId,
    ) {
        if let (Some(runtime), Some(bridge)) = (&self.tokio_runtime, &self.async_bridge) {
            let effective_cwd = cwd.or_else(|| {
                std::env::current_dir()
                    .map(|p| p.to_string_lossy().to_string())
                    .ok()
            });
            let sender = bridge.sender();
            let spawner = self.authority.process_spawner.clone();
            runtime.spawn(async move {
                #[allow(clippy::let_underscore_must_use)]
                match spawner.spawn(command, args, effective_cwd).await {
                    Ok(result) => {
                        let _ = sender.send(AsyncMessage::PluginProcessOutput {
                            process_id: callback_id.as_u64(),
                            stdout: result.stdout,
                            stderr: result.stderr,
                            exit_code: result.exit_code,
                        });
                    }
                    Err(e) => {
                        let _ = sender.send(AsyncMessage::PluginProcessOutput {
                            process_id: callback_id.as_u64(),
                            stdout: String::new(),
                            stderr: e.to_string(),
                            exit_code: -1,
                        });
                    }
                }
            });
        } else {
            self.plugin_manager
                .reject_callback(callback_id, "Async runtime not available".to_string());
        }
    }

    fn handle_kill_host_process(&mut self, process_id: u64) {
        // Removing from the map gives us the oneshot sender. Firing it signals
        // the spawn task to start_kill() the child and reap. Unknown IDs are
        // intentionally silent — the process may have already exited.
        if let Some(tx) = self.host_process_handles.remove(&process_id) {
            #[allow(clippy::let_underscore_must_use)]
            let _ = tx.send(());
            tracing::debug!("KillHostProcess: sent kill for process_id={}", process_id);
        } else {
            tracing::debug!(
                "KillHostProcess: unknown process_id={} (already exited?)",
                process_id
            );
        }
    }

    fn handle_set_authority(&mut self, payload: serde_json::Value) {
        // Payload is opaque at the fresh-core layer; the concrete schema lives
        // in services::authority::AuthorityPayload so core stays ignorant of backend kinds.
        match serde_json::from_value::<crate::services::authority::AuthorityPayload>(payload) {
            Ok(parsed) => {
                match crate::services::authority::Authority::from_plugin_payload(parsed) {
                    Ok(auth) => {
                        tracing::info!("Plugin installed new authority");
                        self.install_authority(auth);
                    }
                    Err(e) => {
                        tracing::warn!("setAuthority: invalid payload: {}", e);
                        self.set_status_message(format!("setAuthority rejected: {}", e));
                    }
                }
            }
            Err(e) => {
                tracing::warn!("setAuthority: failed to parse payload: {}", e);
                self.set_status_message(format!("setAuthority rejected: {}", e));
            }
        }
    }

    fn handle_set_remote_indicator_state(&mut self, state: serde_json::Value) {
        // Opaque JSON at the fresh-core boundary; the concrete schema
        // (RemoteIndicatorOverride) lives in the view crate.
        match serde_json::from_value::<crate::view::ui::status_bar::RemoteIndicatorOverride>(state)
        {
            Ok(over) => {
                self.remote_indicator_override = Some(over);
            }
            Err(e) => {
                tracing::warn!("setRemoteIndicatorState: invalid payload: {}", e);
                self.set_status_message(format!("setRemoteIndicatorState rejected: {}", e));
            }
        }
    }

    fn handle_spawn_process_wait(
        &mut self,
        process_id: u64,
        callback_id: fresh_core::api::JsCallbackId,
    ) {
        tracing::warn!(
            "SpawnProcessWait not fully implemented - process_id={}",
            process_id
        );
        self.plugin_manager.reject_callback(
            callback_id,
            format!(
                "SpawnProcessWait not yet fully implemented for process_id={}",
                process_id
            ),
        );
    }

    fn handle_delay(&mut self, callback_id: fresh_core::api::JsCallbackId, duration_ms: u64) {
        if let (Some(runtime), Some(bridge)) = (&self.tokio_runtime, &self.async_bridge) {
            let sender = bridge.sender();
            let callback_id_u64 = callback_id.as_u64();
            runtime.spawn(async move {
                tokio::time::sleep(tokio::time::Duration::from_millis(duration_ms)).await;
                #[allow(clippy::let_underscore_must_use)]
                let _ = sender.send(crate::services::async_bridge::AsyncMessage::Plugin(
                    fresh_core::api::PluginAsyncMessage::DelayComplete {
                        callback_id: callback_id_u64,
                    },
                ));
            });
        } else {
            std::thread::sleep(std::time::Duration::from_millis(duration_ms));
            self.plugin_manager
                .resolve_callback(callback_id, "null".to_string());
        }
    }

    fn handle_kill_background_process(&mut self, process_id: u64) {
        if let Some(handle) = self.background_process_handles.remove(&process_id) {
            handle.abort();
            tracing::debug!("Killed background process {}", process_id);
        }
    }

    fn handle_create_virtual_buffer(&mut self, name: String, mode: String, read_only: bool) {
        let buffer_id = self.create_virtual_buffer(name.clone(), mode.clone(), read_only);
        tracing::info!(
            "Created virtual buffer '{}' with mode '{}' (id={:?})",
            name,
            mode,
            buffer_id
        );
        // TODO: Return buffer_id to plugin via callback or hook
    }

    fn handle_set_virtual_buffer_content(
        &mut self,
        buffer_id: BufferId,
        entries: Vec<fresh_core::text_property::TextPropertyEntry>,
    ) {
        match self.set_virtual_buffer_content(buffer_id, entries) {
            Ok(()) => {
                tracing::debug!("Set virtual buffer content for {:?}", buffer_id);
            }
            Err(e) => {
                tracing::error!("Failed to set virtual buffer content: {}", e);
            }
        }
    }

    fn handle_get_text_properties_at_cursor(&self, buffer_id: BufferId) {
        if let Some(state) = self.buffers.get(&buffer_id) {
            let cursor_pos = self
                .split_view_states
                .values()
                .find_map(|vs| vs.buffer_state(buffer_id))
                .map(|bs| bs.cursors.primary().position)
                .unwrap_or(0);
            let properties = state.text_properties.get_at(cursor_pos);
            tracing::debug!(
                "Text properties at cursor in {:?}: {} properties found",
                buffer_id,
                properties.len()
            );
            // TODO: Fire hook with properties data for plugins to consume
        }
    }

    fn handle_set_context(&mut self, name: String, active: bool) {
        if active {
            self.active_custom_contexts.insert(name.clone());
            tracing::debug!("Set custom context: {}", name);
        } else {
            self.active_custom_contexts.remove(&name);
            tracing::debug!("Unset custom context: {}", name);
        }
    }

    fn handle_disable_lsp_for_language(&mut self, language: String) {
        tracing::info!("Disabling LSP for language: {}", language);
        if let Some(ref mut lsp) = self.lsp {
            lsp.shutdown_server(&language);
            tracing::info!("Stopped LSP server for {}", language);
        }
        if let Some(lsp_configs) = self.config_mut().lsp.get_mut(&language) {
            for c in lsp_configs.as_mut_slice() {
                c.enabled = false;
                c.auto_start = false;
            }
            tracing::info!("Disabled LSP config for {}", language);
        }
        if let Err(e) = self.save_config() {
            tracing::error!("Failed to save config: {}", e);
            self.status_message = Some(format!(
                "LSP disabled for {} (config save failed)",
                language
            ));
        } else {
            self.status_message = Some(format!("LSP disabled for {}", language));
        }
        self.warning_domains.lsp.clear();
    }

    fn handle_restart_lsp_for_language(&mut self, language: String) {
        tracing::info!("Plugin restarting LSP for language: {}", language);
        let file_path = self
            .buffer_metadata
            .get(&self.active_buffer())
            .and_then(|meta| meta.file_path().cloned());
        let success = if let Some(ref mut lsp) = self.lsp {
            let (ok, msg) = lsp.manual_restart(&language, file_path.as_deref());
            self.status_message = Some(msg);
            ok
        } else {
            self.status_message = Some("No LSP manager available".to_string());
            false
        };
        if success {
            self.reopen_buffers_for_language(&language);
        }
    }

    fn handle_set_lsp_root_uri(&mut self, language: String, uri: String) {
        tracing::info!("Plugin setting LSP root URI for {}: {}", language, uri);
        match uri.parse::<lsp_types::Uri>() {
            Ok(parsed_uri) => {
                if let Some(ref mut lsp) = self.lsp {
                    let restarted = lsp.set_language_root_uri(&language, parsed_uri);
                    if restarted {
                        self.status_message = Some(format!(
                            "LSP root updated for {} (restarting server)",
                            language
                        ));
                    } else {
                        self.status_message = Some(format!("LSP root set for {}", language));
                    }
                }
            }
            Err(e) => {
                tracing::error!("Invalid LSP root URI '{}': {}", uri, e);
                self.status_message = Some(format!("Invalid LSP root URI: {}", e));
            }
        }
    }

    fn handle_create_scroll_sync_group(
        &mut self,
        group_id: crate::view::scroll_sync::ScrollSyncGroupId,
        left_split: SplitId,
        right_split: SplitId,
    ) {
        let success =
            self.scroll_sync_manager
                .create_group_with_id(group_id, left_split, right_split);
        if success {
            tracing::debug!(
                "Created scroll sync group {} for splits {:?} and {:?}",
                group_id,
                left_split,
                right_split
            );
        } else {
            tracing::warn!(
                "Failed to create scroll sync group {} (ID already exists)",
                group_id
            );
        }
    }

    fn handle_set_scroll_sync_anchors(
        &mut self,
        group_id: crate::view::scroll_sync::ScrollSyncGroupId,
        anchors: Vec<(usize, usize)>,
    ) {
        use crate::view::scroll_sync::SyncAnchor;
        let anchor_count = anchors.len();
        let sync_anchors: Vec<SyncAnchor> = anchors
            .into_iter()
            .map(|(left_line, right_line)| SyncAnchor {
                left_line,
                right_line,
            })
            .collect();
        self.scroll_sync_manager.set_anchors(group_id, sync_anchors);
        tracing::debug!(
            "Set {} anchors for scroll sync group {}",
            anchor_count,
            group_id
        );
    }

    fn handle_remove_scroll_sync_group(
        &mut self,
        group_id: crate::view::scroll_sync::ScrollSyncGroupId,
    ) {
        if self.scroll_sync_manager.remove_group(group_id) {
            tracing::debug!("Removed scroll sync group {}", group_id);
        } else {
            tracing::warn!("Scroll sync group {} not found", group_id);
        }
    }

    fn handle_create_buffer_group(
        &mut self,
        name: String,
        mode: String,
        layout_json: String,
        request_id: Option<u64>,
    ) {
        match self.create_buffer_group(name, mode, layout_json) {
            Ok(result) => {
                if let Some(req_id) = request_id {
                    let json = serde_json::to_string(&result).unwrap_or_default();
                    self.plugin_manager
                        .resolve_callback(fresh_core::api::JsCallbackId::from(req_id), json);
                }
            }
            Err(e) => {
                tracing::error!("Failed to create buffer group: {}", e);
            }
        }
    }

    fn handle_send_terminal_input(
        &mut self,
        terminal_id: crate::services::terminal::TerminalId,
        data: String,
    ) {
        if let Some(handle) = self.terminal_manager.get(terminal_id) {
            handle.write(data.as_bytes());
            tracing::trace!(
                "Plugin sent {} bytes to terminal {:?}",
                data.len(),
                terminal_id
            );
        } else {
            tracing::warn!(
                "Plugin tried to send input to non-existent terminal {:?}",
                terminal_id
            );
        }
    }

    fn handle_close_terminal(&mut self, terminal_id: crate::services::terminal::TerminalId) {
        let buffer_to_close = self
            .terminal_buffers
            .iter()
            .find(|(_, &tid)| tid == terminal_id)
            .map(|(&bid, _)| bid);
        if let Some(buffer_id) = buffer_to_close {
            if let Err(e) = self.close_buffer(buffer_id) {
                tracing::warn!("Failed to close terminal buffer: {}", e);
            }
            tracing::info!("Plugin closed terminal {:?}", terminal_id);
        } else {
            self.terminal_manager.close(terminal_id);
            tracing::info!("Plugin closed terminal {:?} (no buffer found)", terminal_id);
        }
    }
}

#[cfg(test)]
mod tests {
    //! Focused tests for the SpawnHostProcess kill mechanism.
    //!
    //! These don't exercise the full `handle_plugin_command` dispatcher
    //! (which would require scaffolding an Editor with a real tokio
    //! runtime and async_bridge); they replicate the inner
    //! `tokio::select!` pattern directly on a real subprocess. A
    //! regression in the select arms or in the kill-then-wait
    //! sequencing would reproduce here.
    //!
    //! The dispatcher-level integration coverage comes from the e2e
    //! attach-cancel test in `tests/e2e/` — this unit test is the
    //! lower-level pin.
    use tokio::io::{AsyncReadExt, BufReader};
    use tokio::process::Command as TokioCommand;
    use tokio::time::{timeout, Duration};

    /// A long-sleep child that runs `tokio::select! { wait | kill_rx }`
    /// terminates when the kill channel fires, and the terminal exit
    /// code reflects signal termination (non-zero / None).
    ///
    /// Spawns `sleep` directly rather than through `sh -c` so SIGKILL
    /// reaches the process whose pipe our reader futures hold —
    /// `sh -c sleep` leaks the sleep child on SIGKILL (Q-C2), the
    /// pipe stays open, and the reader future hangs. That's a
    /// deliberate known limitation of start_kill; this test
    /// exercises the clean path.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn kill_via_oneshot_terminates_long_running_child() {
        let mut cmd = TokioCommand::new("sleep");
        cmd.args(["30"]);
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::piped());

        let mut child = cmd.spawn().expect("spawn sh -c sleep 30");
        let pid = child.id().expect("child has a pid");

        let (kill_tx, mut kill_rx) = tokio::sync::oneshot::channel::<()>();
        let stdout_pipe = child.stdout.take();
        let stderr_pipe = child.stderr.take();

        let stdout_fut = async {
            let mut buf = String::new();
            if let Some(s) = stdout_pipe {
                #[allow(clippy::let_underscore_must_use)]
                let _ = BufReader::new(s).read_to_string(&mut buf).await;
            }
            buf
        };
        let stderr_fut = async {
            let mut buf = String::new();
            if let Some(s) = stderr_pipe {
                #[allow(clippy::let_underscore_must_use)]
                let _ = BufReader::new(s).read_to_string(&mut buf).await;
            }
            buf
        };
        let wait_fut = async {
            tokio::select! {
                status = child.wait() => {
                    status.map(|s| s.code().unwrap_or(-1)).unwrap_or(-1)
                }
                _ = &mut kill_rx => {
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = child.start_kill();
                    child
                        .wait()
                        .await
                        .map(|s| s.code().unwrap_or(-1))
                        .unwrap_or(-1)
                }
            }
        };

        // Give the shell a moment to install itself — firing kill
        // against an not-yet-existent child is still valid (SIGKILL
        // to a zombie is a no-op) but we want to actually exercise
        // the running-child path.
        tokio::time::sleep(Duration::from_millis(50)).await;
        kill_tx.send(()).expect("kill channel send");

        let result = timeout(Duration::from_secs(5), async {
            tokio::join!(stdout_fut, stderr_fut, wait_fut)
        })
        .await;

        let (_stdout, _stderr, exit_code) = result.expect(
            "kill path must resolve within 5s — if this times out the \
             select! arm order or kill-then-wait logic is broken",
        );
        // The cross-platform invariant is "the child did not complete
        // its 30s sleep" — i.e. the exit code is non-success. Platform
        // specifics:
        //   - Unix: `start_kill()` sends SIGKILL; `ExitStatus::code()`
        //     returns None for signal-terminated processes, which our
        //     dispatcher maps to -1 via `.unwrap_or(-1)`.
        //   - Windows: `start_kill()` calls `TerminateProcess(..., 1)`;
        //     `code()` returns `Some(1)`, mapped to 1 by the same
        //     `.unwrap_or(-1)`.
        // A successful 30s sleep would yield 0 — that's the
        // regression case we're guarding against.
        assert_ne!(
            exit_code, 0,
            "killed child must exit non-success (got 0 — did the \
             kill arm fire too late, or did sleep somehow complete?)"
        );

        // Sanity: on Unix the child must be gone. `kill -0 <pid>`
        // returns 0 iff the process still exists; we expect non-zero
        // (No such process) after wait(). This catches a zombie /
        // leaked child that would indicate we skipped the wait() on
        // the kill path. Skipped on Windows — `kill` isn't available
        // and `tasklist` output parsing is more noise than signal
        // for this one-shot check; the wait() having returned is
        // already evidence of reap there.
        #[cfg(unix)]
        {
            let still_alive = std::process::Command::new("kill")
                .args(["-0", &pid.to_string()])
                .status()
                .map(|s| s.success())
                .unwrap_or(false);
            assert!(
                !still_alive,
                "process {pid} must be reaped after wait() — a still-\
                 alive check means the kill path leaked the child"
            );
        }
        #[cfg(not(unix))]
        {
            // Touch `pid` so the unused-variable lint doesn't fire on
            // non-Unix builds.
            let _ = pid;
        }
    }
}
