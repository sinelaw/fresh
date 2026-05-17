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

use super::window::Window;
use super::{Editor, FloatingWidgetState, FLOATING_PANEL_BUFFER_ID};

/// Returns the byte offset of the start (want_end=false) or end (want_end=true)
/// of `line` (0-indexed) within `content`. Returns `None` when `line` is out of
/// range. The "end" position is the byte index of the terminating `\n`; for the
/// last line with no trailing newline it is `buffer_len`.
fn buffer_line_byte_offset(
    content: &str,
    buffer_len: usize,
    line: usize,
    want_end: bool,
) -> Option<usize> {
    if !want_end && line == 0 {
        return Some(0);
    }
    let mut current_line = 0usize;
    for (byte_idx, c) in content.char_indices() {
        if c == '\n' {
            if want_end && current_line == line {
                return Some(byte_idx);
            }
            current_line += 1;
            if !want_end && current_line == line {
                return Some(byte_idx + 1);
            }
        }
    }
    if want_end && current_line == line {
        Some(buffer_len)
    } else {
        None
    }
}

/// Walk a `Tree`'s flat `nodes` and return the absolute indices of
/// nodes that are currently visible — i.e. every ancestor is in
/// `expanded`. Mirrors the renderer's filter so dispatcher and
/// renderer agree on what's selectable.
/// First `Tree` or `List` widget key in `spec`, scanning in
/// declaration order. Used by mouse-wheel routing to pick which
/// widget inside a panel absorbs the scroll.
fn find_scrollable_widget_key(spec: &fresh_core::api::WidgetSpec) -> Option<String> {
    use fresh_core::api::WidgetSpec;
    match spec {
        WidgetSpec::Tree { key: Some(k), .. } | WidgetSpec::List { key: Some(k), .. }
            if !k.is_empty() =>
        {
            return Some(k.clone());
        }
        _ => {}
    }
    spec.children().find_map(find_scrollable_widget_key)
}

fn collect_visible_tree_indices(
    nodes: &[fresh_core::api::TreeNode],
    item_keys: &[String],
    expanded: &std::collections::HashSet<String>,
) -> Vec<usize> {
    let mut ancestor_open: Vec<bool> = Vec::new();
    let mut visible: Vec<usize> = Vec::with_capacity(nodes.len());
    for (i, node) in nodes.iter().enumerate() {
        let depth = node.depth as usize;
        ancestor_open.truncate(depth);
        if ancestor_open.iter().all(|open| *open) {
            visible.push(i);
        }
        let key = item_keys.get(i).cloned().unwrap_or_default();
        let is_open = if node.has_children {
            !key.is_empty() && expanded.contains(&key)
        } else {
            true
        };
        ancestor_open.push(is_open);
    }
    visible
}

impl Editor {
    /// Update the plugin state snapshot with current editor state.
    ///
    /// Per-window snapshot population (active buffer, splits, view
    /// states, cursors, diagnostics, folding ranges, plugin view
    /// states) lives in [`Window::populate_plugin_state_snapshot`].
    /// This function adds the editor-wide fields that no single Window
    /// owns (clipboard, the full `windows` list, the memoized config
    /// JSON cache, `user_config_raw`, and `plugin_global_state`).
    #[cfg(feature = "plugins")]
    pub(super) fn update_plugin_state_snapshot(&mut self) {
        let Some(snapshot_handle) = self.plugin_manager.read().unwrap().state_snapshot_handle()
        else {
            return;
        };
        let mut snapshot = snapshot_handle.write().unwrap();

        self.active_window_mut()
            .populate_plugin_state_snapshot(&mut snapshot);

        // Editor-wide fields below — these reach state outside any
        // single Window.

        snapshot.clipboard = self.clipboard.get_internal().to_string();
        snapshot.working_dir = self.working_dir.clone();

        // Total terminal dimensions (full screen, not the active
        // split's viewport). Plugins read this via `getScreenSize()`
        // to size floating overlays against the whole terminal.
        snapshot.terminal_width = self.terminal_width;
        snapshot.terminal_height = self.terminal_height;

        // Authority label tracks `Editor::authority` (the active
        // authority). It can't be sourced from `Window::resources.authority`
        // because `set_boot_authority` replaces `self.authority` by value
        // — the per-window resource clones still point at the previous
        // authority handle. Reading from `Editor` keeps the snapshot in
        // lockstep with the canonical seat.
        snapshot.authority_label = self.authority.display_label.clone();

        // Publish the session list so plugins (Orchestrator, etc.)
        // see updates from createWindow/closeWindow without
        // a separate notification path. Sorted by id for
        // deterministic order — `next_window_id` is monotonic
        // so this is "creation order".
        let mut session_infos: Vec<fresh_core::api::WindowInfo> = self
            .windows
            .values()
            .map(|s| {
                let slot = s.plugin_state.get("orchestrator");
                let project_path = slot
                    .and_then(|m| m.get("project_path"))
                    .and_then(|v| v.as_str())
                    .map(std::path::PathBuf::from);
                let shared_worktree = slot
                    .and_then(|m| m.get("shared_worktree"))
                    .and_then(|v| v.as_bool())
                    .unwrap_or(false);
                fresh_core::api::WindowInfo {
                    id: s.id,
                    label: s.label.clone(),
                    root: s.root.clone(),
                    project_path,
                    shared_worktree,
                }
            })
            .collect();
        session_infos.sort_by_key(|s| s.id.0);
        snapshot.windows = session_infos;
        snapshot.active_window_id = self.active_window;

        // Reserialize config only when the underlying `Arc<Config>`
        // pointer has actually moved since the last refresh —
        // `Arc::ptr_eq` vs `config_snapshot_anchor` is a sound cache
        // key because the anchor keeps `self.config`'s strong count
        // at ≥ 2, forcing every `Arc::make_mut` on the editor side
        // to CoW into a new allocation. On idle (no config mutation),
        // this branch is skipped entirely and the snapshot update is
        // a refcount bump.
        if !Arc::ptr_eq(&self.config, &self.config_snapshot_anchor) {
            let json = serde_json::to_value(&*self.config).unwrap_or(serde_json::Value::Null);
            self.config_cached_json = Arc::new(json);
            self.config_snapshot_anchor = Arc::clone(&self.config);
        }
        snapshot.config = Arc::clone(&self.config_cached_json);

        // Cached raw user config file contents (not merged with defaults).
        // Lets plugins distinguish user-set from default values.
        snapshot.user_config = Arc::clone(&self.user_config_raw);

        // Merge plugin global states from Rust-side store.
        // `or_insert` preserves JS-side write-through entries.
        for (plugin_name, state_map) in &self.plugin_global_state {
            let entry = snapshot
                .plugin_global_states
                .entry(plugin_name.clone())
                .or_default();
            for (key, value) in state_map {
                entry.entry(key.clone()).or_insert_with(|| value.clone());
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
                gutter_glyph,
                gutter_color,
                text_overlays,
            } => {
                self.handle_add_virtual_line(
                    buffer_id,
                    position,
                    text,
                    fg_color,
                    bg_color,
                    above,
                    namespace,
                    priority,
                    gutter_glyph,
                    gutter_color,
                    text_overlays,
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
            PluginCommand::SetFoldingRanges { buffer_id, ranges } => {
                self.handle_set_folding_ranges(buffer_id, ranges);
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
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_manager_mut())
                    .expect("active window must have a populated split layout")
                    .set_label(LeafId(split_id), label);
            }
            PluginCommand::ClearSplitLabel { split_id } => {
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_manager_mut())
                    .expect("active window must have a populated split layout")
                    .clear_label(split_id);
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
            PluginCommand::SetWindowState {
                plugin_name,
                key,
                value,
            } => {
                self.handle_set_session_state(plugin_name, key, value);
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
                self.active_window_mut()
                    .handle_set_file_explorer_decorations(namespace, decorations);
            }
            PluginCommand::ClearFileExplorerDecorations { namespace } => {
                self.active_window_mut()
                    .handle_clear_file_explorer_decorations(&namespace);
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
            PluginCommand::StartPrompt {
                label,
                prompt_type,
                floating_overlay,
            } => {
                self.handle_start_prompt(label, prompt_type, floating_overlay);
            }
            PluginCommand::StartPromptWithInitial {
                label,
                prompt_type,
                initial_value,
                floating_overlay,
            } => {
                self.handle_start_prompt_with_initial(
                    label,
                    prompt_type,
                    initial_value,
                    floating_overlay,
                );
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
                self.active_window_mut().key_capture_active = active;
                if !active {
                    // Capture window closed; any leftover queued keys
                    // were intended for the plugin and should not now
                    // leak into the editor's normal dispatch.
                    self.active_window_mut().pending_key_capture_buffer.clear();
                }
            }
            PluginCommand::SetPromptSuggestions { suggestions } => {
                self.handle_set_prompt_suggestions(suggestions);
            }
            PluginCommand::SetPromptInputSync { sync } => {
                if let Some(prompt) = &mut self.active_window_mut().prompt {
                    prompt.sync_input_on_navigate = sync;
                }
            }
            PluginCommand::SetPromptTitle { title } => {
                if let Some(prompt) = &mut self.active_window_mut().prompt {
                    prompt.title = title;
                }
            }
            PluginCommand::SetPromptFooter { footer } => {
                if let Some(prompt) = &mut self.active_window_mut().prompt {
                    prompt.footer = footer;
                }
            }
            PluginCommand::SetPromptSelectedIndex { index } => {
                if let Some(prompt) = &mut self.active_window_mut().prompt {
                    let len = prompt.suggestions.len();
                    if len > 0 {
                        let clamped = (index as usize).min(len - 1);
                        prompt.selected_suggestion = Some(clamped);
                    }
                }
            }

            // ==================== Session lifecycle ====================
            // See docs/internal/orchestrator-sessions-design.md.
            PluginCommand::CreateWindow { root, label } => {
                if !root.is_absolute() {
                    tracing::warn!(
                        "CreateWindow rejected: root must be absolute, got {:?}",
                        root
                    );
                } else {
                    let _ = self.create_window_at(root, label);
                }
            }
            PluginCommand::SetActiveWindow { id } => {
                self.set_active_window(id);
            }
            PluginCommand::CloseWindow { id } => {
                let _ = self.close_window(id);
            }
            PluginCommand::PrewarmWindow { id } => {
                self.prewarm_window(id);
            }

            // ==================== File watching ====================
            PluginCommand::WatchPath {
                path,
                recursive,
                request_id,
            } => {
                let result = if let Some(ref bridge) = self.async_bridge {
                    self.file_watcher_manager.watch(bridge, &path, recursive)
                } else {
                    Err(
                        "watchPath: no async bridge — file watching is unavailable in this build"
                            .to_string(),
                    )
                };
                self.last_watch_response_for_test = Some((request_id, result.clone()));
                self.send_plugin_response(fresh_core::api::PluginResponse::WatchPathRegistered {
                    request_id,
                    result,
                });
            }
            PluginCommand::UnwatchPath { handle } => {
                self.file_watcher_manager.unwatch(handle);
            }

            PluginCommand::PreviewWindowInRect { id } => {
                // Validate: only honour if the session exists and
                // is not the active one (no point previewing the
                // session whose UI is already on screen).
                self.preview_window_id = match id {
                    Some(sid) if sid != self.active_window && self.windows.contains_key(&sid) => {
                        Some(sid)
                    }
                    _ => None,
                };
            }

            // ==================== Command/Mode Registration ====================
            PluginCommand::RegisterCommand { command } => {
                self.handle_register_command(command);
            }
            PluginCommand::RegisterStatusBarElement {
                plugin_name,
                token_name,
                title,
            } => {
                if let Err(e) = self.register_status_bar_element(&plugin_name, &token_name, &title)
                {
                    tracing::warn!("Failed to register statusbar element: {}", e);
                }
            }
            PluginCommand::SetStatusBarValue {
                buffer_id,
                key,
                value,
            } => {
                if let Err(e) =
                    self.set_status_bar_value(fresh_core::BufferId(buffer_id as usize), &key, value)
                {
                    tracing::warn!("Failed to set statusbar value: {}", e);
                }
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
            PluginCommand::OpenFileInBackground { path, window_id } => {
                let route_to_inactive = match window_id {
                    Some(id) if id != self.active_window && self.windows.contains_key(&id) => {
                        Some(id)
                    }
                    _ => None,
                };
                if let Some(target) = route_to_inactive {
                    self.handle_open_file_in_inactive_session(target, path);
                } else {
                    self.handle_open_file_in_background(path);
                }
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
            PluginCommand::CloseOtherBuffersInSplit {
                buffer_id,
                split_id,
            } => {
                self.handle_close_other_buffers_in_split(buffer_id, split_id);
            }
            PluginCommand::CloseAllBuffersInSplit { split_id } => {
                self.handle_close_all_buffers_in_split(split_id);
            }
            PluginCommand::CloseBuffersToRightInSplit {
                buffer_id,
                split_id,
            } => {
                self.handle_close_buffers_to_right_in_split(buffer_id, split_id);
            }
            PluginCommand::CloseBuffersToLeftInSplit {
                buffer_id,
                split_id,
            } => {
                self.handle_close_buffers_to_left_in_split(buffer_id, split_id);
            }

            PluginCommand::MoveTabLeft => {
                self.handle_move_tab_left();
            }
            PluginCommand::MoveTabRight => {
                self.handle_move_tab_right();
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
                self.active_window_mut()
                    .animations
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
                stdout_to,
                callback_id,
            } => {
                self.handle_spawn_process(command, args, cwd, stdout_to, callback_id);
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
                role,
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
                    role,
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
                self.active_window_mut().review_hunks = hunks;
                tracing::debug!(
                    "Set {} review hunks",
                    self.active_window_mut().review_hunks.len()
                );
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
            PluginCommand::OpenFileStreaming { path, request_id } => {
                self.handle_open_file_streaming(path, request_id);
            }
            PluginCommand::RefreshBufferFromDisk {
                buffer_id,
                request_id,
            } => {
                self.handle_refresh_buffer_from_disk(buffer_id, request_id);
            }
            PluginCommand::SetBufferGroupPanelBuffer {
                group_id,
                panel_name,
                buffer_id,
                request_id,
            } => {
                self.handle_set_buffer_group_panel_buffer(
                    group_id, panel_name, buffer_id, request_id,
                );
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

            PluginCommand::SetLspMenuContributions {
                plugin_id,
                language,
                items,
            } => {
                self.handle_set_lsp_menu_contributions(plugin_id, language, items);
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
                self.active_window_mut().close_composite_buffer(buffer_id);
            }
            PluginCommand::FlushLayout => {
                self.flush_layout();
            }
            PluginCommand::CompositeNextHunk { buffer_id } => {
                let split_id = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();
                self.active_window_mut()
                    .composite_next_hunk(split_id, buffer_id);
            }
            PluginCommand::CompositePrevHunk { buffer_id } => {
                let split_id = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();
                self.active_window_mut()
                    .composite_prev_hunk(split_id, buffer_id);
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
                window_id,
                command,
                title,
                request_id,
            } => {
                self.handle_create_terminal(
                    cwd, direction, ratio, focus, persistent, window_id, command, title, request_id,
                );
            }

            PluginCommand::SendTerminalInput { terminal_id, data } => {
                self.handle_send_terminal_input(terminal_id, data);
            }

            PluginCommand::CloseTerminal { terminal_id } => {
                self.handle_close_terminal(terminal_id);
            }

            PluginCommand::SignalWindow { id, signal } => {
                self.handle_signal_window(id, &signal);
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

            PluginCommand::BeginSearch {
                pattern,
                fixed_string,
                case_sensitive,
                max_results,
                whole_words,
                handle_id,
            } => {
                self.handle_begin_search(
                    pattern,
                    fixed_string,
                    case_sensitive,
                    max_results,
                    whole_words,
                    handle_id,
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

            PluginCommand::MountWidgetPanel {
                panel_id,
                buffer_id,
                spec,
            } => {
                self.handle_mount_widget_panel(panel_id, buffer_id, spec);
            }

            PluginCommand::UpdateWidgetPanel { panel_id, spec } => {
                self.handle_update_widget_panel(panel_id, spec);
            }

            PluginCommand::UnmountWidgetPanel { panel_id } => {
                self.handle_unmount_widget_panel(panel_id);
            }

            PluginCommand::WidgetCommand { panel_id, action } => {
                self.handle_widget_command(panel_id, action);
            }

            PluginCommand::WidgetMutate { panel_id, mutation } => {
                self.handle_widget_mutate(panel_id, mutation);
            }

            PluginCommand::MountFloatingWidget {
                panel_id,
                spec,
                width_pct,
                height_pct,
            } => {
                self.handle_mount_floating_widget(panel_id, spec, width_pct, height_pct);
            }

            PluginCommand::UpdateFloatingWidget { panel_id, spec } => {
                self.handle_update_floating_widget(panel_id, spec);
            }

            PluginCommand::UnmountFloatingWidget { panel_id } => {
                self.handle_unmount_floating_widget(panel_id);
            }
        }
        Ok(())
    }

    /// Save a buffer to a specific file path (for :w filename)
    fn handle_save_buffer_to_path(&mut self, buffer_id: BufferId, path: std::path::PathBuf) {
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
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
        match self.plugin_manager.read().unwrap().load_plugin(&path) {
            Ok(()) => {
                tracing::info!("Loaded plugin from {:?}", path);
                self.plugin_manager
                    .read()
                    .unwrap()
                    .resolve_callback(callback_id, "true".to_string());
            }
            Err(e) => {
                tracing::error!("Failed to load plugin from {:?}: {}", path, e);
                self.plugin_manager
                    .read()
                    .unwrap()
                    .reject_callback(callback_id, format!("{}", e));
            }
        }
    }

    /// Unload a plugin by name
    #[cfg(feature = "plugins")]
    fn handle_unload_plugin(&mut self, name: String, callback_id: JsCallbackId) {
        // Drop the write guard before the read lock below (match-scrutinee
        // temporaries would otherwise live until end-of-match).
        let result = self.plugin_manager.write().unwrap().unload_plugin(&name);
        match result {
            Ok(()) => {
                tracing::info!("Unloaded plugin: {}", name);
                self.plugin_manager
                    .read()
                    .unwrap()
                    .resolve_callback(callback_id, "true".to_string());
            }
            Err(e) => {
                tracing::error!("Failed to unload plugin '{}': {}", name, e);
                self.plugin_manager
                    .read()
                    .unwrap()
                    .reject_callback(callback_id, format!("{}", e));
            }
        }
    }

    /// Reload a plugin by name
    #[cfg(feature = "plugins")]
    fn handle_reload_plugin(&mut self, name: String, callback_id: JsCallbackId) {
        match self.plugin_manager.read().unwrap().reload_plugin(&name) {
            Ok(()) => {
                tracing::info!("Reloaded plugin: {}", name);
                self.plugin_manager
                    .read()
                    .unwrap()
                    .resolve_callback(callback_id, "true".to_string());
            }
            Err(e) => {
                tracing::error!("Failed to reload plugin '{}': {}", name, e);
                self.plugin_manager
                    .read()
                    .unwrap()
                    .reject_callback(callback_id, format!("{}", e));
            }
        }
    }

    /// List all loaded plugins
    #[cfg(feature = "plugins")]
    fn handle_list_plugins(&mut self, callback_id: JsCallbackId) {
        let plugins = self.plugin_manager.read().unwrap().list_plugins();
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
        self.plugin_manager
            .read()
            .unwrap()
            .resolve_callback(callback_id, json_str);
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
        let result = if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
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
                self.plugin_manager
                    .read()
                    .unwrap()
                    .resolve_callback(callback_id, json);
            }
            Err(error) => {
                self.plugin_manager
                    .read()
                    .unwrap()
                    .reject_callback(callback_id, error);
            }
        }
    }

    /// Set the global editor mode (for vi mode)
    fn handle_set_editor_mode(&mut self, mode: Option<String>) {
        self.active_window_mut().editor_mode = mode.clone();
        tracing::debug!("Set editor mode: {:?}", mode);
    }

    /// Normalize a plugin-supplied `BufferId`: treat id 0 as "use the active buffer".
    fn resolve_buffer_id(&self, buffer_id: BufferId) -> BufferId {
        if buffer_id.0 == 0 {
            self.active_buffer()
        } else {
            buffer_id
        }
    }

    /// Serialize `value` as JSON and resolve `request_id` as a JS Promise callback.
    fn resolve_json_callback<T: serde::Serialize>(&mut self, request_id: u64, value: T) {
        let callback_id = fresh_core::api::JsCallbackId::from(request_id);
        let json = serde_json::to_string(&value).unwrap_or_else(|_| "null".to_string());
        self.plugin_manager
            .read()
            .unwrap()
            .resolve_callback(callback_id, json);
    }

    /// Get the byte offset of the start of a line in the active buffer
    fn handle_get_line_start_position(&mut self, buffer_id: BufferId, line: u32, request_id: u64) {
        let actual_buffer_id = self.resolve_buffer_id(buffer_id);
        let result = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&actual_buffer_id)
            .and_then(|state| {
                let len = state.buffer.len();
                let content = state.get_text_range(0, len);
                buffer_line_byte_offset(&content, len, line as usize, false)
            });
        self.resolve_json_callback(request_id, result);
    }

    /// Get the byte offset of the end of a line (position of its terminating newline,
    /// or `buffer_len` for the last line without a trailing newline).
    fn handle_get_line_end_position(&mut self, buffer_id: BufferId, line: u32, request_id: u64) {
        let actual_buffer_id = self.resolve_buffer_id(buffer_id);
        let result = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&actual_buffer_id)
            .and_then(|state| {
                let len = state.buffer.len();
                let content = state.get_text_range(0, len);
                buffer_line_byte_offset(&content, len, line as usize, true)
            });
        self.resolve_json_callback(request_id, result);
    }

    /// Get the total number of lines in a buffer
    fn handle_get_buffer_line_count(&mut self, buffer_id: BufferId, request_id: u64) {
        let actual_buffer_id = self.resolve_buffer_id(buffer_id);

        let result = if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&actual_buffer_id)
        {
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

        self.resolve_json_callback(request_id, result);
    }

    /// Open `path` as a regular buffer for plugin-driven streaming
    /// display. The file is created (empty) if missing.
    ///
    /// Routes through the same `open_file_no_focus` orchestrator that
    /// `editor.openFile` uses, so the buffer gets the full setup
    /// (encoding/binary detection, language detection, buffer settings,
    /// margin config, per-split BufferViewState defaults). This is
    /// critical for things like the scrollbar's visual-row index —
    /// bypassing this setup and going straight to `BufferData::Unloaded`
    /// breaks `line_count()` and any code that depends on it.
    ///
    /// Designed for buffers that will be filled by a concurrent
    /// `spawnProcess` with `stdoutTo`. Pair with `RefreshBufferFromDisk`
    /// to grow the buffer as the file is written; `extend_streaming`
    /// (called by that path) counts newlines in the appended region
    /// so the buffer's line index stays correct as it grows.
    fn handle_open_file_streaming(&mut self, path: std::path::PathBuf, request_id: u64) {
        // Ensure the file exists at 0 bytes if missing, so the open
        // path has something to load.
        if !self.authority.filesystem.exists(&path) {
            if let Some(parent) = path.parent() {
                if !parent.as_os_str().is_empty() {
                    if let Err(e) = std::fs::create_dir_all(parent) {
                        tracing::warn!(
                            "openFileStreaming: failed to create parent dir {:?}: {}",
                            parent,
                            e
                        );
                        self.resolve_json_callback::<Option<u64>>(request_id, None);
                        return;
                    }
                }
            }
            if let Err(e) = std::fs::write(&path, b"") {
                tracing::warn!(
                    "openFileStreaming: failed to create empty file at {:?}: {}",
                    path,
                    e
                );
                self.resolve_json_callback::<Option<u64>>(request_id, None);
                return;
            }
        }

        // Use the same orchestrator that backs `editor.openFile`. This
        // ensures the buffer is set up identically to a user-opened
        // file (settings, language, view-state defaults, line indexing).
        let buffer_id = match self.open_file_no_focus(&path) {
            Ok(id) => id,
            Err(e) => {
                tracing::warn!(
                    "openFileStreaming: open_file_no_focus failed for {:?}: {}",
                    path,
                    e
                );
                self.resolve_json_callback::<Option<u64>>(request_id, None);
                return;
            }
        };

        // Plugin-managed surfaces (typically buffer-group panel
        // targets) shouldn't show up in quick-switch / tab strip, and
        // shouldn't be auto-reverted on file change — the plugin is
        // driving the file's contents itself via `extend_streaming`.
        if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(&buffer_id) {
            meta.hidden_from_tabs = true;
            meta.auto_revert_enabled = false;
        }
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        if let Some(vs) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .get_mut(&active_split)
        {
            use crate::view::split::TabTarget;
            vs.open_buffers
                .retain(|t| !matches!(t, TabTarget::Buffer(b) if *b == buffer_id));
        }

        self.resolve_json_callback(request_id, Some(buffer_id.0));
    }

    /// Re-point a buffer-group's panel at a different buffer id.
    /// Delegates to `BufferGroupOps::set_buffer_group_panel_buffer`.
    fn handle_set_buffer_group_panel_buffer(
        &mut self,
        group_id: usize,
        panel_name: String,
        buffer_id: BufferId,
        request_id: u64,
    ) {
        let actual_buffer_id = self.resolve_buffer_id(buffer_id);
        let ok = self.set_buffer_group_panel_buffer(group_id, panel_name, actual_buffer_id);
        self.resolve_json_callback(request_id, ok);
    }

    /// Re-stat the file backing `buffer_id` and extend the buffer if
    /// the file has grown. No-op if the buffer has no file path or the
    /// file didn't grow. Resolves with the new total byte length.
    fn handle_refresh_buffer_from_disk(&mut self, buffer_id: BufferId, request_id: u64) {
        let actual_buffer_id = self.resolve_buffer_id(buffer_id);

        let path = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, _)| ())
            .and_then(|_| {
                self.windows
                    .get(&self.active_window)?
                    .buffers
                    .get(&actual_buffer_id)?
                    .buffer
                    .file_path()
                    .map(|p| p.to_path_buf())
            });

        let Some(path) = path else {
            // No file path — nothing to refresh.
            self.resolve_json_callback::<Option<usize>>(request_id, None);
            return;
        };

        let new_size = match self.authority.filesystem.metadata(&path) {
            Ok(m) => m.size as usize,
            Err(_) => {
                self.resolve_json_callback::<Option<usize>>(request_id, None);
                return;
            }
        };

        let new_total = if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&actual_buffer_id)
        {
            let old = state.buffer.total_bytes();
            if new_size > old {
                state.buffer.extend_streaming(&path, new_size);
            }
            state.buffer.total_bytes()
        } else {
            self.resolve_json_callback::<Option<usize>>(request_id, None);
            return;
        };

        self.resolve_json_callback(request_id, Some(new_total));
    }

    /// Scroll a split to center a specific line in the viewport
    fn handle_scroll_to_line_center(
        &mut self,
        split_id: SplitId,
        buffer_id: BufferId,
        line: usize,
    ) {
        let actual_split_id = if split_id.0 == 0 {
            self.windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .active_split()
        } else {
            LeafId(split_id)
        };
        let actual_buffer_id = self.resolve_buffer_id(buffer_id);

        // Get viewport height
        let viewport_height = if let Some(view_state) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&actual_split_id)
        {
            view_state.viewport.height as usize
        } else {
            return;
        };

        // Calculate the target line to scroll to (center the requested line)
        let lines_above = viewport_height / 2;
        let target_line = line.saturating_sub(lines_above);

        self.active_window_mut().scroll_split_viewport_to(
            actual_buffer_id,
            actual_split_id,
            target_line,
            true,
        );
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
        if !self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .contains_key(&buffer_id)
        {
            return;
        }

        // Collect the leaf ids whose active buffer is `buffer_id`.
        let mut target_leaves: Vec<LeafId> = Vec::new();

        // Main tree: walk its leaves.
        for leaf_id in self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .root()
            .leaf_split_ids()
        {
            if let Some(vs) = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
                .get(&leaf_id)
            {
                if vs.active_buffer == buffer_id {
                    target_leaves.push(leaf_id);
                }
            }
        }

        // Grouped subtrees: walk each group's inner leaves.
        for (_group_leaf_id, node) in self.active_window().grouped_subtrees.iter() {
            if let crate::view::split::SplitNode::Grouped { layout, .. } = node {
                for inner_leaf in layout.leaf_split_ids() {
                    if let Some(vs) = self
                        .windows
                        .get(&self.active_window)
                        .and_then(|w| w.buffers.splits())
                        .map(|(_, vs)| vs)
                        .expect("active window must have a populated split layout")
                        .get(&inner_leaf)
                    {
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

        self.active_window_mut()
            .scroll_buffer_to_line_in_splits(buffer_id, &target_leaves, line);
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
                use crate::services::process_hidden::HideWindow;
                let mut cmd = TokioCommand::new(&command);
                cmd.args(&args);
                cmd.stdout(std::process::Stdio::piped());
                cmd.stderr(std::process::Stdio::piped());
                cmd.hide_window();
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
                .read()
                .unwrap()
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
                use crate::services::process_hidden::HideWindow;
                let mut child = match TokioCommand::new(&command)
                    .args(&args)
                    .current_dir(&effective_cwd)
                    .stdout(std::process::Stdio::piped())
                    .stderr(std::process::Stdio::piped())
                    .hide_window()
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
                .read()
                .unwrap()
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
        let buffer_id =
            self.active_window_mut()
                .create_virtual_buffer(name.clone(), mode.clone(), read_only);
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
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
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
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        if let Some(view_state) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .get_mut(&active_split)
        {
            view_state.ensure_buffer_state(buffer_id).show_line_numbers = show_line_numbers;
        }

        // Apply hidden_from_tabs to buffer metadata
        if hidden_from_tabs {
            if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(&buffer_id) {
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
                    self.plugin_manager.read().unwrap().resolve_callback(
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
        role: Option<String>,
        request_id: Option<u64>,
    ) {
        // Resolve the role string. Unknown roles are silently dropped
        // (forward-compat for plugins targeting newer cores).
        let split_role: Option<crate::view::split::SplitRole> = match role.as_deref() {
            Some("utility_dock") => Some(crate::view::split::SplitRole::UtilityDock),
            _ => None,
        };

        // Utility-dock fast path (issue #1796 / Section 2 of the design):
        // if a leaf with this role already exists, swap its active
        // buffer instead of spawning a fresh split. The buffer is
        // created normally, registered in `panel_ids`, and added as a
        // tab in the dock leaf.
        if let Some(target_role) = split_role {
            if let Some(dock_leaf) = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .find_leaf_by_role(target_role)
            {
                // Capture the source split *before* create_virtual_buffer
                // tabs the new buffer into it; we drop that phantom tab
                // after the dock attach so the buffer only shows in the
                // dock.
                let source_split_before_create = self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(mgr, _)| mgr)
                    .expect("active window must have a populated split layout")
                    .active_split();
                let buffer_id = self.active_window_mut().create_virtual_buffer(
                    name.clone(),
                    mode.clone(),
                    read_only,
                );
                if let Some(state) = self
                    .windows
                    .get_mut(&self.active_window)
                    .map(|w| &mut w.buffers)
                    .expect("active window present")
                    .get_mut(&buffer_id)
                {
                    state.margins.configure_for_line_numbers(show_line_numbers);
                    state.show_cursors = show_cursors;
                    state.editing_disabled = editing_disabled;
                }
                if let Some(pid) = &panel_id {
                    self.panel_ids_mut().insert(pid.clone(), buffer_id);
                }
                if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries) {
                    tracing::error!("Failed to set virtual buffer content (dock route): {}", e);
                    return;
                }

                // Swap the dock leaf's active buffer to the new one and
                // add it as a tab so the user can flip between
                // dock-resident utilities (Diagnostics ↔ Quickfix etc.).
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_manager_mut())
                    .expect("active window must have a populated split layout")
                    .set_active_split(dock_leaf);
                self.active_window_mut()
                    .set_pane_buffer(dock_leaf, buffer_id);

                // Drop the phantom tab from the source split.
                if dock_leaf != source_split_before_create {
                    if let Some(source_view_state) = self
                        .windows
                        .get_mut(&self.active_window)
                        .and_then(|w| w.split_view_states_mut())
                        .expect("active window must have a populated split layout")
                        .get_mut(&source_split_before_create)
                    {
                        source_view_state.remove_buffer(buffer_id);
                    }
                }

                if let Some(req_id) = request_id {
                    let result = fresh_core::api::VirtualBufferResult {
                        buffer_id: buffer_id.0 as u64,
                        split_id: Some(dock_leaf.0 .0 as u64),
                    };
                    self.plugin_manager.read().unwrap().resolve_callback(
                        fresh_core::api::JsCallbackId::from(req_id),
                        serde_json::to_string(&result).unwrap_or_default(),
                    );
                }
                tracing::info!(
                    "Routed virtual buffer '{}' into existing utility dock {:?}",
                    name,
                    dock_leaf
                );
                return;
            }
            // No dock yet — fall through to normal split creation,
            // then tag the new leaf with the requested role at the end.
        }

        // Check if this panel already exists (for idempotent operations)
        if let Some(pid) = &panel_id {
            if let Some(&existing_buffer_id) = self.panel_ids().get(pid) {
                // Verify the buffer actually exists (defensive check for stale entries)
                if self
                    .windows
                    .get(&self.active_window)
                    .map(|w| &w.buffers)
                    .expect("active window present")
                    .contains_key(&existing_buffer_id)
                {
                    // Panel exists, just update its content
                    if let Err(e) = self.set_virtual_buffer_content(existing_buffer_id, entries) {
                        tracing::error!("Failed to update panel content: {}", e);
                    } else {
                        tracing::info!("Updated existing panel '{}' content", pid);
                    }

                    // Find and focus the split that contains this buffer
                    let splits = self
                        .windows
                        .get(&self.active_window)
                        .and_then(|w| w.buffers.splits())
                        .map(|(mgr, _)| mgr)
                        .expect("active window must have a populated split layout")
                        .splits_for_buffer(existing_buffer_id);
                    if let Some(&split_id) = splits.first() {
                        self.windows
                            .get_mut(&self.active_window)
                            .and_then(|w| w.split_manager_mut())
                            .expect("active window must have a populated split layout")
                            .set_active_split(split_id);
                        // Route through set_pane_buffer so tree + SVS
                        // stay consistent (issue #1620 invariant).
                        self.active_window_mut()
                            .set_pane_buffer(split_id, existing_buffer_id);
                        tracing::debug!("Focused split {:?} containing panel buffer", split_id);
                    }

                    // Send response with existing buffer ID and split ID via callback resolution
                    if let Some(req_id) = request_id {
                        let result = fresh_core::api::VirtualBufferResult {
                            buffer_id: existing_buffer_id.0 as u64,
                            split_id: splits.first().map(|s| s.0 .0 as u64),
                        };
                        self.plugin_manager.read().unwrap().resolve_callback(
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
                    self.panel_ids_mut().remove(pid);
                    // Fall through to create a new buffer
                }
            }
        }

        // Capture the source split before creating the buffer —
        // `create_virtual_buffer` unconditionally adds the new buffer
        // as a tab to the currently active split, which is the wrong
        // thing for a panel that lives in its own dedicated split
        // (it would show up as a tab in BOTH splits — see bug #3).
        let source_split_before_create = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();

        // Create the virtual buffer first
        let buffer_id =
            self.active_window_mut()
                .create_virtual_buffer(name.clone(), mode.clone(), read_only);
        tracing::info!(
            "Created virtual buffer '{}' with mode '{}' in split (id={:?})",
            name,
            mode,
            buffer_id
        );

        // Apply view options to the buffer
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
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
            self.panel_ids_mut().insert(pid, buffer_id);
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

        // Create a split with the new buffer. When the caller asked
        // for `role = "utility_dock"` and no dock leaf exists yet,
        // split at the *root* so the dock spans the full width below
        // any pre-existing side-by-side panes — splitting the active
        // leaf would nest the dock under whichever pane was focused.
        let created_split_id =
            match if split_role == Some(crate::view::split::SplitRole::UtilityDock) {
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_manager_mut())
                    .expect("active window must have a populated split layout")
                    .split_root_positioned(split_dir, buffer_id, ratio, before)
            } else {
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_manager_mut())
                    .expect("active window must have a populated split layout")
                    .split_active_positioned(split_dir, buffer_id, ratio, before)
            } {
                Ok(new_split_id) => {
                    // The buffer now lives in its own split, so drop its
                    // tab from the source split (see bug #3).  Only do
                    // this when the new split actually differs from the
                    // source split — otherwise we'd leave no split
                    // displaying the buffer.
                    if new_split_id != source_split_before_create {
                        if let Some(source_view_state) = self
                            .windows
                            .get_mut(&self.active_window)
                            .and_then(|w| w.split_view_states_mut())
                            .expect("active window must have a populated split layout")
                            .get_mut(&source_split_before_create)
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
                        line_wrap.unwrap_or_else(|| {
                            self.active_window().resolve_line_wrap_for_buffer(buffer_id)
                        }),
                        self.config.editor.wrap_indent,
                        self.active_window()
                            .resolve_wrap_column_for_buffer(buffer_id),
                        self.config.editor.rulers.clone(),
                    );
                    // Override with plugin-requested show_line_numbers
                    view_state.ensure_buffer_state(buffer_id).show_line_numbers = show_line_numbers;
                    self.windows
                        .get_mut(&self.active_window)
                        .and_then(|w| w.split_view_states_mut())
                        .expect("active window must have a populated split layout")
                        .insert(new_split_id, view_state);

                    // Focus the new split (the diagnostics panel)
                    self.windows
                        .get_mut(&self.active_window)
                        .and_then(|w| w.split_manager_mut())
                        .expect("active window must have a populated split layout")
                        .set_active_split(new_split_id);
                    // NOTE: split tree was updated by split_active, active_buffer derives from it

                    // If a role was requested but no dock existed (we fell
                    // through the fast-path above), tag the freshly created
                    // leaf so the next utility lands here. Clear any stale
                    // role from elsewhere first to preserve the
                    // one-leaf-per-role invariant.
                    if let Some(target_role) = split_role {
                        self.windows
                            .get_mut(&self.active_window)
                            .and_then(|w| w.split_manager_mut())
                            .expect("active window must have a populated split layout")
                            .clear_role(target_role);
                        self.windows
                            .get_mut(&self.active_window)
                            .and_then(|w| w.split_manager_mut())
                            .expect("active window must have a populated split layout")
                            .set_leaf_role(new_split_id, Some(target_role));
                        tracing::info!(
                            "Tagged new dock leaf {:?} with role {:?}",
                            new_split_id,
                            target_role
                        );
                    }

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
            self.plugin_manager.read().unwrap().resolve_callback(
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
        let buffer_id =
            self.active_window_mut()
                .create_virtual_buffer(name.clone(), mode.clone(), read_only);
        tracing::info!(
            "Created virtual buffer '{}' with mode '{}' for existing split {:?} (id={:?})",
            name,
            mode,
            split_id,
            buffer_id
        );

        // Apply view options to the buffer
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
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
        self.windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .set_active_split(leaf_id);
        self.active_window_mut().set_pane_buffer(leaf_id, buffer_id);

        // Fall-through to the cursor/open_buffers housekeeping
        // that used to follow the manual switch_buffer. We keep
        // the `if let Some(view_state)` block below — set_pane_buffer
        // already called switch_buffer, but the downstream code
        // also nudges open_buffers and focus_history.
        if let Some(view_state) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .get_mut(&leaf_id)
        {
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
            self.plugin_manager.read().unwrap().resolve_callback(
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

        // `convert_popup_data_to_popup` hardcodes a default dark
        // background because it has no theme handle (it's called from
        // `EditorState::apply` too). Restamp the active theme's
        // `popup_bg` / `popup_border_fg` here so plugin popups don't
        // render as a near-black rectangle on top of a light theme —
        // #1941 issue 2.
        {
            let theme = self.theme();
            popup_obj.background_style = ratatui::style::Style::default().bg(theme.popup_bg);
            popup_obj.border_style = ratatui::style::Style::default().fg(theme.popup_border_fg);
        }

        // Dismiss any built-in LSP-status popup that the editor put
        // on `active_state().popups` in response to the same click —
        // the plugin's popup is the contextual answer and stacking
        // ours underneath leaves two popups for one user gesture
        // (#1941 issue 1). Done here (rather than at the
        // `show_lsp_status_popup` call site) because plugin handlers
        // run *asynchronously*: by the time the `ShowActionPopup`
        // command reaches us, the LSP-Servers popup has already
        // landed. Re-run on every plugin push (not just the first
        // dedup'd one) because rapid repeated clicks can re-add the
        // LSP-Servers popup between consecutive plugin commands.
        while self
            .active_state()
            .popups
            .top()
            .is_some_and(|p| matches!(p.resolver, crate::view::popup::PopupResolver::LspStatus))
        {
            self.active_state_mut().popups.hide();
        }

        // Dedup by `popup_id`: if a previous `showActionPopup` with
        // the same id is still on the stack (common: repeated
        // indicator clicks fire `lsp_status_clicked` over and over,
        // each one re-pushing "rust-lsp-help"), replace it in place
        // instead of stacking another copy. Without this, dismissing
        // one reveals the same popup underneath — #1941 issue 4.
        let existing_idx = self.global_popups.all().iter().position(|p| {
            matches!(
                &p.resolver,
                crate::view::popup::PopupResolver::PluginAction { popup_id: id } if id == &popup_id,
            )
        });
        if let Some(idx) = existing_idx {
            if let Some(slot) = self.global_popups.get_mut(idx) {
                *slot = popup_obj;
            }
        } else {
            self.global_popups.show(popup_obj);
        }
        tracing::info!(
            "Action popup shown: id={}, stack_depth={}",
            popup_id,
            self.global_popups.all().len()
        );
    }

    /// Install (or replace, or clear) a plugin's contributions for the
    /// LSP-Servers popup. Passing an empty `items` removes any
    /// previous contribution from this `plugin_id` for this
    /// `language`. Mirrors the editor-side half of
    /// `PluginCommand::SetLspMenuContributions`.
    ///
    /// If the LSP-Servers popup is currently open for this language,
    /// refresh it in place so the new rows show up immediately
    /// rather than only on the next click.
    fn handle_set_lsp_menu_contributions(
        &mut self,
        plugin_id: String,
        language: String,
        items: Vec<fresh_core::api::LspMenuItem>,
    ) {
        let key = (language.clone(), plugin_id.clone());
        if items.is_empty() {
            self.active_window_mut().lsp_menu_contributions.remove(&key);
        } else {
            self.active_window_mut()
                .lsp_menu_contributions
                .insert(key, items);
        }
        // If the popup is on screen right now, re-render it so the
        // change is immediately visible — the alternative is "next
        // click sees it" which feels unresponsive when the plugin
        // is reacting to an event the user just triggered.
        self.refresh_lsp_status_popup_if_open();
    }

    fn handle_create_terminal(
        &mut self,
        cwd: Option<String>,
        direction: Option<String>,
        ratio: Option<f32>,
        focus: Option<bool>,
        persistent: bool,
        target_session_id: Option<fresh_core::WindowId>,
        command: Option<Vec<String>>,
        title: Option<String>,
        request_id: u64,
    ) {
        // Resolve target window. Explicit `windowId` wins when the
        // window exists; otherwise we operate on the active window.
        // Both cases route through `Window::create_plugin_terminal`
        // so spawning into an inactive session reuses the same code
        // path — no separate migration helper, no half-state leaks
        // between windows.
        let target_id = target_session_id
            .filter(|id| self.windows.contains_key(id))
            .unwrap_or(self.active_window);
        let is_active_target = target_id == self.active_window;

        let cwd_buf = cwd.map(std::path::PathBuf::from);
        let split_direction = direction.as_deref().map(|d| match d {
            "horizontal" => crate::model::event::SplitDirection::Horizontal,
            _ => crate::model::event::SplitDirection::Vertical,
        });

        // Capture the editor-active buffer before the spawn so we
        // can detect whether `Window::create_plugin_terminal`'s
        // per-window mutations also flipped the editor-active buffer
        // (only possible when `is_active_target`). If it did, the
        // `buffer_activated` plugin hook needs to fire here at the
        // Editor level — the Window method only mutates per-window
        // state.
        let prev_active = if is_active_target {
            Some(self.active_window().active_buffer())
        } else {
            None
        };

        let result = {
            let target = self
                .windows
                .get_mut(&target_id)
                .expect("target window present (existence checked above)");
            target.create_plugin_terminal(
                cwd_buf,
                split_direction,
                ratio,
                focus.unwrap_or(true),
                persistent,
                command,
                title.filter(|t| !t.is_empty()),
            )
        };
        match result {
            Ok((terminal_id, buffer_id, created_split_id)) => {
                if is_active_target {
                    let new_active = self.active_window().active_buffer();
                    if prev_active != Some(new_active) {
                        #[cfg(feature = "plugins")]
                        self.update_plugin_state_snapshot();
                        #[cfg(feature = "plugins")]
                        self.plugin_manager.read().unwrap().run_hook(
                            "buffer_activated",
                            crate::services::plugins::hooks::HookArgs::BufferActivated {
                                buffer_id: new_active,
                            },
                        );
                    }
                }
                let api_result = fresh_core::api::TerminalResult {
                    buffer_id: buffer_id.0 as u64,
                    terminal_id: terminal_id.0 as u64,
                    split_id: created_split_id.map(|s| s.0 .0 as u64),
                };
                self.plugin_manager.read().unwrap().resolve_callback(
                    fresh_core::api::JsCallbackId::from(request_id),
                    serde_json::to_string(&api_result).unwrap_or_default(),
                );
                tracing::info!(
                    "Plugin created terminal {:?} with buffer {:?} in window {:?}",
                    terminal_id,
                    buffer_id,
                    target_id
                );
            }
            Err(e) => {
                tracing::error!("Failed to create terminal for plugin: {e}");
                self.plugin_manager.read().unwrap().reject_callback(
                    fresh_core::api::JsCallbackId::from(request_id),
                    format!("Failed to create terminal: {e}"),
                );
            }
        }
    }

    // ==================== Extracted handlers for previously inline match arms ====================

    fn handle_get_split_by_label(&mut self, label: String, request_id: u64) {
        let split_id = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .find_split_by_label(&label);
        let callback_id = fresh_core::api::JsCallbackId::from(request_id);
        let json =
            serde_json::to_string(&split_id.map(|s| s.0 .0)).unwrap_or_else(|_| "null".to_string());
        self.plugin_manager
            .read()
            .unwrap()
            .resolve_callback(callback_id, json);
    }

    fn handle_set_buffer_show_cursors(&mut self, buffer_id: BufferId, show: bool) {
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
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
        let applied = self.theme.write().unwrap().override_colors(pairs);
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
        if let Some(payload) = self
            .active_window_mut()
            .pending_key_capture_buffer
            .pop_front()
        {
            let json = serde_json::to_string(&payload).unwrap_or_else(|_| "null".to_string());
            self.plugin_manager
                .read()
                .unwrap()
                .resolve_callback(callback_id, json);
        } else {
            self.active_window_mut()
                .pending_next_key_callbacks
                .push_back(callback_id);
        }
    }

    fn handle_spawn_process(
        &mut self,
        command: String,
        args: Vec<String>,
        cwd: Option<String>,
        stdout_to: Option<std::path::PathBuf>,
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

            // Kill plumbing: register a oneshot keyed by process_id, same
            // pattern as handle_spawn_host_process. JS calls
            // `_killHostProcess(id)` → `handle_kill_host_process` fires
            // the tx; the spawner's `spawn_cancellable` races against rx.
            let process_id = callback_id.as_u64();
            let (kill_tx, kill_rx) = tokio::sync::oneshot::channel::<()>();
            self.host_process_handles.insert(process_id, kill_tx);

            runtime.spawn(async move {
                #[allow(clippy::let_underscore_must_use)]
                let outcome = spawner
                    .spawn_cancellable(command, args, effective_cwd, stdout_to, kill_rx)
                    .await;
                match outcome {
                    Ok(result) => {
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = sender.send(AsyncMessage::PluginProcessOutput {
                            process_id,
                            stdout: result.stdout,
                            stderr: result.stderr,
                            exit_code: result.exit_code,
                        });
                    }
                    Err(e) => {
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = sender.send(AsyncMessage::PluginProcessOutput {
                            process_id,
                            stdout: String::new(),
                            stderr: e.to_string(),
                            exit_code: -1,
                        });
                    }
                }
            });
        } else {
            self.plugin_manager
                .read()
                .unwrap()
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
        self.plugin_manager.read().unwrap().reject_callback(
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
                .read()
                .unwrap()
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
        let buffer_id =
            self.active_window_mut()
                .create_virtual_buffer(name.clone(), mode.clone(), read_only);
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

    fn handle_mount_widget_panel(
        &mut self,
        panel_id: u64,
        buffer_id: BufferId,
        spec: fresh_core::api::WidgetSpec,
    ) {
        // Mount = clean slate. Instance state and focus key reset
        // so a plugin that re-mounts (e.g. reopening a panel with
        // a fresh prefill) sees its spec values take effect. To
        // *preserve* state across renders, the plugin uses Update.
        let prev = std::collections::HashMap::new();
        let prev_focus = String::new();
        let panel_width = self.widget_panel_width(buffer_id);
        let out = crate::widgets::render_spec(&spec, &prev, &prev_focus, panel_width);
        let focus_cursor = out.focus_cursor;
        self.widget_registry.mount(
            panel_id,
            buffer_id,
            spec,
            out.hits,
            out.instance_states,
            out.focus_key,
            out.tabbable,
        );
        let entries = out.entries;
        if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries.clone()) {
            tracing::error!(
                "Failed to render mounted widget panel {} into {:?}: {}",
                panel_id,
                buffer_id,
                e
            );
        } else {
            tracing::debug!(
                "Mounted widget panel {} into buffer {:?}",
                panel_id,
                buffer_id
            );
        }
        self.apply_widget_focus_cursor(buffer_id, &entries, focus_cursor);
    }

    fn handle_update_widget_panel(&mut self, panel_id: u64, spec: fresh_core::api::WidgetSpec) {
        let prev = match self.widget_registry.instance_states(panel_id) {
            Some(s) => s.clone(),
            None => {
                tracing::debug!(
                    "UpdateWidgetPanel for unknown panel {} ignored (not mounted)",
                    panel_id
                );
                return;
            }
        };
        let prev_focus = self
            .widget_registry
            .focus_key(panel_id)
            .map(|s| s.to_string())
            .unwrap_or_default();
        let buffer_id_for_width = self
            .widget_registry
            .buffer_and_spec(panel_id)
            .map(|(b, _)| b)
            .unwrap_or(BufferId(0));
        let panel_width = self.widget_panel_width(buffer_id_for_width);
        let out = crate::widgets::render_spec(&spec, &prev, &prev_focus, panel_width);
        let focus_cursor = out.focus_cursor;
        let entries = out.entries;
        match self.widget_registry.update(
            panel_id,
            spec,
            out.hits,
            out.instance_states,
            out.focus_key,
            out.tabbable,
        ) {
            Ok(buffer_id) => {
                if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries.clone()) {
                    tracing::error!("Failed to render updated widget panel {}: {}", panel_id, e);
                }
                self.apply_widget_focus_cursor(buffer_id, &entries, focus_cursor);
            }
            Err(()) => {
                tracing::debug!(
                    "UpdateWidgetPanel for unknown panel {} ignored (not mounted)",
                    panel_id
                );
            }
        }
    }

    /// Apply a `RenderOutput`'s focus-cursor position to the panel
    /// buffer + every split rendering it. When a `TextInput` is
    /// focused, the dispatcher flips `show_cursors=true` and moves
    /// the primary cursor to the right byte. When no TextInput is
    /// focused, the cursor is hidden (`show_cursors=false`) — the
    /// focused widget's own bg overlay shows where focus is.
    ///
    /// Must be called *after* `set_virtual_buffer_content` so the
    /// buffer's text matches the row/byte coordinates the renderer
    /// produced.
    fn apply_widget_focus_cursor(
        &mut self,
        buffer_id: BufferId,
        entries: &[fresh_core::text_property::TextPropertyEntry],
        focus_cursor: Option<crate::widgets::FocusCursor>,
    ) {
        let absolute_byte = focus_cursor.map(|fc| {
            let row = fc.buffer_row as usize;
            let prefix: usize = entries.iter().take(row).map(|e| e.text.len()).sum();
            prefix + fc.byte_in_row as usize
        });

        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
            state.show_cursors = absolute_byte.is_some();
        }

        if let Some(byte) = absolute_byte {
            for vs in self
                .windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_view_states_mut())
                .expect("active window must have a populated split layout")
                .values_mut()
            {
                if vs.buffer_state(buffer_id).is_some() {
                    let cursor = vs.cursors.primary_mut();
                    cursor.position = byte;
                }
            }
        }
    }

    /// Best-effort width for a buffer's containing split. Returns
    /// the most recent `SplitViewState::viewport.width` for any
    /// split rendering this buffer; falls back to terminal width
    /// when the buffer hasn't been rendered yet (e.g. mid-mount).
    /// Subtracts 2 columns to account for gutter/scrollbar/border
    /// padding the renderer adds — leaving the right edge clear
    /// instead of pushing content into the chrome. This is what
    /// flex `Spacer`s inside `Row` use to size their fill.
    fn widget_panel_width(&self, buffer_id: BufferId) -> u32 {
        let raw = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .values()
            .find(|vs| vs.buffer_state(buffer_id).is_some() && vs.viewport.width > 0)
            .map(|vs| vs.viewport.width as u32)
            .unwrap_or_else(|| self.terminal_width.max(1) as u32);
        // Reserve 2 cols for gutter/scrollbar/border. Saturate to
        // avoid 0 width on tiny panels.
        raw.saturating_sub(2).max(10)
    }

    /// Re-render an existing widget panel after an in-host state
    /// change (focus advance, scroll move, etc.) without the plugin
    /// re-emitting the spec. Reads the panel's current spec from
    /// the registry, runs `render_spec` against the (possibly
    /// updated) prev state / focus key, writes the result back.
    pub(super) fn rerender_widget_panel(&mut self, panel_id: u64) {
        // The spec already lives in the registry — mutations (e.g.
        // `append_tree_nodes_in_spec`) edit it in place. Borrow it for
        // render, then write back only the side-effects (hits, instance
        // states, focus key, tabbable). The previous shape cloned the
        // whole spec out, rendered, then moved it back — for a Tree
        // with 5 000 nodes that's a multi-MB deep clone per IPC, which
        // dominates the host's per-mutation cost during a streaming
        // search.
        let (buffer_id, is_floating, panel_width, out_pieces) = {
            let (buffer_id, spec) = match self.widget_registry.buffer_and_spec_ref(panel_id) {
                Some(s) => s,
                None => return,
            };
            let prev = self
                .widget_registry
                .instance_states(panel_id)
                .cloned()
                .unwrap_or_default();
            let prev_focus = self
                .widget_registry
                .focus_key(panel_id)
                .map(|s| s.to_string())
                .unwrap_or_default();
            let is_floating = buffer_id == FLOATING_PANEL_BUFFER_ID;
            let panel_width = if is_floating {
                self.floating_panel_inner_width()
            } else {
                self.widget_panel_width(buffer_id)
            };
            let out = crate::widgets::render_spec(spec, &prev, &prev_focus, panel_width);
            (buffer_id, is_floating, panel_width, out)
        };
        let _ = panel_width;
        let focus_cursor = out_pieces.focus_cursor;
        let entries = out_pieces.entries;
        let embeds = out_pieces.embeds;
        let overlays = out_pieces.overlays;
        if self
            .widget_registry
            .update_side_effects(
                panel_id,
                out_pieces.hits,
                out_pieces.instance_states,
                out_pieces.focus_key,
                out_pieces.tabbable,
            )
            .is_err()
        {
            tracing::warn!("rerender_widget_panel({}) lost panel mid-call", panel_id);
            return;
        }
        if is_floating {
            if let Some(fwp) = self.floating_widget_panel.as_mut() {
                if fwp.panel_id == panel_id {
                    fwp.entries = entries;
                    fwp.focus_cursor = focus_cursor;
                    fwp.embeds = embeds;
                    fwp.overlays = overlays;
                }
            }
            return;
        }
        if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries.clone()) {
            tracing::error!("rerender_widget_panel({}) failed: {}", panel_id, e);
        }
        self.apply_widget_focus_cursor(buffer_id, &entries, focus_cursor);
    }

    /// Apply a `WidgetMutation` in place, then re-render the panel.
    /// This is the IPC fast path: the plugin doesn't re-transmit
    /// the full spec; it sends one targeted change. The host
    /// mutates the registry's spec / instance state and re-renders
    /// against the just-mutated state.
    fn handle_widget_mutate(&mut self, panel_id: u64, mutation: fresh_core::api::WidgetMutation) {
        use fresh_core::api::WidgetMutation;

        // Look up the panel; bail if unknown.
        if self.widget_registry.get(panel_id).is_none() {
            tracing::debug!(
                "WidgetMutate for unknown panel {} ignored (not mounted)",
                panel_id
            );
            return;
        }

        match mutation {
            WidgetMutation::SetValue {
                widget_key,
                value,
                cursor_byte,
            } => {
                // Value+cursor live in instance state for the unified
                // Text widget. Preserve `scroll` and `multiline` from
                // the previous editor across the mutation so
                // multi-line viewport offsets don't snap on a
                // plugin-driven update; the renderer re-clamps next
                // render anyway.
                if let Some(panel) = self.widget_registry.get_mut(panel_id) {
                    let (scroll, multiline) = match panel.instance_states.get(&widget_key) {
                        Some(crate::widgets::WidgetInstanceState::Text { editor, scroll }) => {
                            (*scroll, editor.multiline)
                        }
                        _ => (0u32, true),
                    };
                    let mut editor = if multiline {
                        crate::primitives::text_edit::TextEdit::with_text(&value)
                    } else {
                        crate::primitives::text_edit::TextEdit::single_line_with_text(&value)
                    };
                    let target = match cursor_byte {
                        Some(c) if c >= 0 => (c as usize).min(value.len()),
                        _ => value.len(),
                    };
                    editor.set_cursor_from_flat(target);
                    panel.instance_states.insert(
                        widget_key,
                        crate::widgets::WidgetInstanceState::Text { editor, scroll },
                    );
                }
            }
            WidgetMutation::SetChecked {
                widget_key,
                checked,
            } => {
                // Toggle checked lives in the spec (not instance
                // state). Walk the spec, find the Toggle by key,
                // mutate.
                if let Some(panel) = self.widget_registry.get_mut(panel_id) {
                    crate::widgets::set_toggle_checked_in_spec(
                        &mut panel.spec,
                        &widget_key,
                        checked,
                    );
                }
            }
            WidgetMutation::SetSelectedIndex { widget_key, index } => {
                // List selected_index lives in instance state.
                if let Some(panel) = self.widget_registry.get_mut(panel_id) {
                    let prev_scroll = match panel.instance_states.get(&widget_key) {
                        Some(crate::widgets::WidgetInstanceState::List {
                            scroll_offset, ..
                        }) => *scroll_offset,
                        _ => 0,
                    };
                    panel.instance_states.insert(
                        widget_key,
                        crate::widgets::WidgetInstanceState::List {
                            scroll_offset: prev_scroll,
                            selected_index: index,
                        },
                    );
                }
            }
            WidgetMutation::SetItems {
                widget_key,
                items,
                item_keys,
            } => {
                // List items live in the spec.
                if let Some(panel) = self.widget_registry.get_mut(panel_id) {
                    crate::widgets::set_list_items_in_spec(
                        &mut panel.spec,
                        &widget_key,
                        items,
                        item_keys,
                    );
                }
            }
            WidgetMutation::SetExpandedKeys { widget_key, keys } => {
                // Tree expanded_keys lives in instance state.
                if let Some(panel) = self.widget_registry.get_mut(panel_id) {
                    let (prev_scroll, prev_sel) = match panel.instance_states.get(&widget_key) {
                        Some(crate::widgets::WidgetInstanceState::Tree {
                            scroll_offset,
                            selected_index,
                            ..
                        }) => (*scroll_offset, *selected_index),
                        _ => (0, -1),
                    };
                    let expanded: std::collections::HashSet<String> = keys.into_iter().collect();
                    panel.instance_states.insert(
                        widget_key,
                        crate::widgets::WidgetInstanceState::Tree {
                            scroll_offset: prev_scroll,
                            selected_index: prev_sel,
                            expanded_keys: expanded,
                        },
                    );
                }
            }
            WidgetMutation::SetCheckedKeys {
                widget_key,
                checked,
                keys,
            } => {
                // Tree node `checked` lives in the spec (not instance
                // state) — the plugin is the source of truth and can
                // re-derive the boolean from its model on every spec
                // emit. The mutator just stamps the new value into the
                // matching nodes so the next render reflects it
                // immediately, without round-tripping through the
                // plugin.
                if let Some(panel) = self.widget_registry.get_mut(panel_id) {
                    crate::widgets::set_tree_checked_keys_in_spec(
                        &mut panel.spec,
                        &widget_key,
                        checked,
                        &keys,
                    );
                }
            }
            WidgetMutation::AppendTreeNodes {
                widget_key,
                new_nodes,
                new_item_keys,
            } => {
                if let Some(panel) = self.widget_registry.get_mut(panel_id) {
                    crate::widgets::append_tree_nodes_in_spec(
                        &mut panel.spec,
                        &widget_key,
                        new_nodes,
                        new_item_keys,
                    );
                }
            }
            WidgetMutation::SetRawEntries {
                widget_key,
                entries,
            } => {
                if let Some(panel) = self.widget_registry.get_mut(panel_id) {
                    crate::widgets::set_raw_entries_in_spec(&mut panel.spec, &widget_key, entries);
                }
            }
        }

        // Re-render with the mutated state. `rerender_widget_panel`
        // reads the registry's current spec + instance state and
        // pushes the result through the buffer.
        self.rerender_widget_panel(panel_id);
    }

    pub(super) fn handle_widget_command(
        &mut self,
        panel_id: u64,
        action: fresh_core::api::WidgetAction,
    ) {
        use fresh_core::api::WidgetAction;
        match action {
            WidgetAction::FocusAdvance { delta } => {
                self.handle_widget_focus_advance(panel_id, delta);
            }
            WidgetAction::Activate => {
                self.handle_widget_activate(panel_id);
            }
            WidgetAction::SelectMove { delta } => {
                self.handle_widget_select_move(panel_id, delta);
            }
            WidgetAction::TextInputKey { key } => {
                self.handle_widget_text_key(panel_id, &key);
            }
            WidgetAction::TextInputChar { text } => {
                self.handle_widget_text_char(panel_id, &text);
            }
            WidgetAction::Key { key } => {
                self.handle_widget_key(panel_id, &key);
            }
        }
    }

    fn handle_widget_key(&mut self, panel_id: u64, key: &str) {
        // Smart key dispatch — route to the right specialized
        // handler based on focused widget kind. See WidgetAction::Key
        // doc for the dispatch table.
        let panel = match self.widget_registry.get(panel_id) {
            Some(p) => p,
            None => return,
        };
        let focus_key = panel.focus_key.clone();
        let widget = if focus_key.is_empty() {
            None
        } else {
            crate::widgets::find_widget_by_key(&panel.spec, &focus_key)
        };
        match key {
            "Tab" => self.handle_widget_focus_advance(panel_id, 1),
            "Shift+Tab" => self.handle_widget_focus_advance(panel_id, -1),
            "Up" | "Down" => {
                let delta = if key == "Up" { -1 } else { 1 };
                match widget {
                    Some(fresh_core::api::WidgetSpec::List { .. }) => {
                        self.handle_widget_select_move(panel_id, delta);
                    }
                    Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                        self.handle_widget_tree_select_move(panel_id, delta);
                    }
                    Some(fresh_core::api::WidgetSpec::Text { rows, .. }) if *rows > 1 => {
                        // Multi-line Text: line nav. Single-line
                        // is filtered out — TextEdit::move_up /
                        // move_down would no-op on the single
                        // line, but skipping the dispatch keeps
                        // the change-event quiet.
                        self.handle_widget_text_key(panel_id, key);
                    }
                    _ => {
                        // Picker-style nav: when the focused widget
                        // doesn't have a meaningful Up/Down (single-
                        // line Text, Button, Toggle, or no focus),
                        // route the arrow to the first scrollable
                        // widget in the panel. Lets a filter input
                        // stay focused for typing while arrows
                        // navigate the adjacent list.
                        let scrollable = self
                            .widget_registry
                            .get(panel_id)
                            .and_then(|p| find_scrollable_widget_key(&p.spec));
                        if let Some(target_key) = scrollable {
                            let target_kind = self.widget_registry.get(panel_id).and_then(|p| {
                                crate::widgets::find_widget_by_key(&p.spec, &target_key).cloned()
                            });
                            match target_kind {
                                Some(fresh_core::api::WidgetSpec::List { .. }) => {
                                    self.handle_widget_select_move_for_key(
                                        panel_id,
                                        &target_key,
                                        delta,
                                    );
                                }
                                Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                                    self.handle_widget_tree_select_move_for_key(
                                        panel_id,
                                        &target_key,
                                        delta,
                                    );
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            "PageUp" | "PageDown" => {
                // Page step = visible_rows - 1 (one row of overlap so
                // the user keeps a visual anchor across pages). Ignored
                // for non-scrollable widgets.
                let page = match widget {
                    Some(fresh_core::api::WidgetSpec::List { visible_rows, .. })
                    | Some(fresh_core::api::WidgetSpec::Tree { visible_rows, .. }) => {
                        visible_rows.saturating_sub(1).max(1) as i32
                    }
                    _ => 0,
                };
                if page == 0 {
                    return;
                }
                let delta = if key == "PageUp" { -page } else { page };
                match widget {
                    Some(fresh_core::api::WidgetSpec::List { .. }) => {
                        self.handle_widget_select_move(panel_id, delta);
                    }
                    Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                        self.handle_widget_tree_select_move(panel_id, delta);
                    }
                    _ => {}
                }
            }
            "Left" | "Right" => match widget {
                Some(fresh_core::api::WidgetSpec::Text { .. }) => {
                    self.handle_widget_text_key(panel_id, key);
                }
                Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                    self.handle_widget_tree_lateral(panel_id, key == "Right");
                }
                _ => {}
            },
            "Backspace" | "Delete" | "Home" | "End" => match widget {
                Some(fresh_core::api::WidgetSpec::Text { .. }) => {
                    self.handle_widget_text_key(panel_id, key);
                }
                _ => {}
            },
            "Enter" => match widget {
                Some(fresh_core::api::WidgetSpec::Button { .. })
                | Some(fresh_core::api::WidgetSpec::Toggle { .. }) => {
                    self.handle_widget_activate(panel_id);
                }
                Some(fresh_core::api::WidgetSpec::List { .. }) => {
                    self.fire_list_activate(panel_id, &focus_key);
                }
                Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                    self.fire_tree_activate(panel_id, &focus_key);
                }
                Some(fresh_core::api::WidgetSpec::Text { rows, .. }) => {
                    if *rows > 1 {
                        // Multi-line: Enter inserts a newline at the
                        // cursor. Plugins that want Enter to submit
                        // can intercept it in their mode binding
                        // before dispatching through the smart-key
                        // router.
                        self.handle_widget_text_key(panel_id, "Enter");
                    } else if let Some(target_key) = self
                        .widget_registry
                        .get(panel_id)
                        .and_then(|p| find_scrollable_widget_key(&p.spec))
                    {
                        // Picker-style activate: a single-line filter
                        // input paired with a List/Tree fires that
                        // scrollable's activate event on Enter, so the
                        // user can type-then-Enter without tabbing
                        // focus to the list.
                        let kind = self.widget_registry.get(panel_id).and_then(|p| {
                            crate::widgets::find_widget_by_key(&p.spec, &target_key).cloned()
                        });
                        match kind {
                            Some(fresh_core::api::WidgetSpec::List { .. }) => {
                                self.fire_list_activate(panel_id, &target_key);
                            }
                            Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                                self.fire_tree_activate(panel_id, &target_key);
                            }
                            _ => {}
                        }
                    } else {
                        // Form-like UX: Enter commits the field and
                        // moves to the next tabbable widget.
                        self.handle_widget_focus_advance(panel_id, 1);
                    }
                }
                _ => {}
            },
            "Space" => match widget {
                Some(fresh_core::api::WidgetSpec::Button { .. })
                | Some(fresh_core::api::WidgetSpec::Toggle { .. }) => {
                    self.handle_widget_activate(panel_id);
                }
                Some(fresh_core::api::WidgetSpec::Text { .. }) => {
                    self.handle_widget_text_char(panel_id, " ");
                }
                Some(fresh_core::api::WidgetSpec::List { .. }) => {
                    self.fire_list_activate(panel_id, &focus_key);
                }
                Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                    // On a checkable Tree, Space is the conventional
                    // checkbox key — fire `toggle` for the focused row
                    // (matching what a click on its `[v]`/`[ ]` glyph
                    // would do). Falls back to `activate` for trees
                    // that aren't checkable, or rows that don't have
                    // a checkbox glyph (`checked: None`).
                    if !self.fire_tree_toggle_if_checkable(panel_id, &focus_key) {
                        self.fire_tree_activate(panel_id, &focus_key);
                    }
                }
                _ => {}
            },
            _ => {} // unrecognised key — quietly ignore
        }
    }

    fn handle_widget_focus_advance(&mut self, panel_id: u64, delta: i32) {
        let panel = match self.widget_registry.get(panel_id) {
            Some(p) => p,
            None => return,
        };
        if panel.tabbable.is_empty() {
            return;
        }
        let cur_idx = panel
            .tabbable
            .iter()
            .position(|k| k == &panel.focus_key)
            .unwrap_or(0) as i32;
        let n = panel.tabbable.len() as i32;
        let new_idx = ((cur_idx + delta) % n + n) % n;
        let new_key = panel.tabbable[new_idx as usize].clone();
        self.widget_registry.set_focus_key(panel_id, new_key);
        self.rerender_widget_panel(panel_id);
    }

    fn handle_widget_activate(&mut self, panel_id: u64) {
        // Fire `widget_event` based on the focused widget's kind.
        // Button → "activate"; Toggle → "toggle" (with the
        // computed-new payload); other kinds: no-op.
        let panel = match self.widget_registry.get(panel_id) {
            Some(p) => p,
            None => return,
        };
        let focus_key = panel.focus_key.clone();
        if focus_key.is_empty() {
            return;
        }
        let widget = crate::widgets::find_widget_by_key(&panel.spec, &focus_key);
        let (event_type, payload) = match widget {
            Some(fresh_core::api::WidgetSpec::Button { .. }) => ("activate", serde_json::json!({})),
            Some(fresh_core::api::WidgetSpec::Toggle { checked, .. }) => {
                ("toggle", serde_json::json!({ "checked": !checked }))
            }
            _ => return,
        };
        if self
            .plugin_manager
            .read()
            .unwrap()
            .has_hook_handlers("widget_event")
        {
            self.plugin_manager.read().unwrap().run_hook(
                "widget_event",
                fresh_core::hooks::HookArgs::WidgetEvent {
                    panel_id,
                    widget_key: focus_key,
                    event_type: event_type.to_string(),
                    payload,
                },
            );
        }
    }

    /// Fire a `widget_event { event_type: "activate", payload: {
    /// index, key } }` for the focused List, using its instance-state
    /// selection (or spec selection on first render). The plugin's
    /// activate handler does the actual user-visible thing — open
    /// the matched file, expand/collapse a tree node, etc.
    fn fire_list_activate(&mut self, panel_id: u64, focus_key: &str) {
        let panel = match self.widget_registry.get(panel_id) {
            Some(p) => p,
            None => return,
        };
        let widget = crate::widgets::find_widget_by_key(&panel.spec, focus_key);
        let (spec_sel, item_keys) = match widget {
            Some(fresh_core::api::WidgetSpec::List {
                selected_index,
                item_keys,
                ..
            }) => (*selected_index, item_keys.clone()),
            _ => return,
        };
        let sel = match panel.instance_states.get(focus_key) {
            Some(crate::widgets::WidgetInstanceState::List { selected_index, .. }) => {
                *selected_index
            }
            _ => spec_sel,
        };
        if sel < 0 {
            return;
        }
        let item_key = item_keys.get(sel as usize).cloned().unwrap_or_default();
        if self
            .plugin_manager
            .read()
            .unwrap()
            .has_hook_handlers("widget_event")
        {
            self.plugin_manager.read().unwrap().run_hook(
                "widget_event",
                fresh_core::hooks::HookArgs::WidgetEvent {
                    panel_id,
                    widget_key: focus_key.to_string(),
                    event_type: "activate".into(),
                    payload: serde_json::json!({
                        "index": sel,
                        "key": item_key,
                    }),
                },
            );
        }
    }

    fn handle_widget_select_move(&mut self, panel_id: u64, delta: i32) {
        let focus_key = match self.widget_registry.get(panel_id) {
            Some(p) => p.focus_key.clone(),
            None => return,
        };
        if focus_key.is_empty() {
            return;
        }
        self.handle_widget_select_move_for_key(panel_id, &focus_key, delta);
    }

    /// Same as [`handle_widget_select_move`] but targets an explicit
    /// `List` widget key instead of the panel's focused widget. Used
    /// by the picker-style smart-key dispatch — `Up`/`Down` on a
    /// focused filter input route to the first scrollable widget in
    /// the panel without changing focus.
    fn handle_widget_select_move_for_key(&mut self, panel_id: u64, widget_key: &str, delta: i32) {
        let panel = match self.widget_registry.get(panel_id) {
            Some(p) => p,
            None => return,
        };
        let widget = crate::widgets::find_widget_by_key(&panel.spec, widget_key);
        let (spec_sel, total, item_keys) = match widget {
            Some(fresh_core::api::WidgetSpec::List {
                selected_index,
                items,
                item_keys,
                ..
            }) => (*selected_index, items.len() as i32, item_keys.clone()),
            _ => return,
        };
        if total == 0 {
            return;
        }
        let cur_sel = match panel.instance_states.get(widget_key) {
            Some(crate::widgets::WidgetInstanceState::List { selected_index, .. }) => {
                *selected_index
            }
            _ => spec_sel,
        };
        let raw = if cur_sel < 0 { 0 } else { cur_sel + delta };
        let new_sel = raw.clamp(0, total - 1);
        let new_key = item_keys.get(new_sel as usize).cloned().unwrap_or_default();
        if let Some(panel_mut) = self.widget_registry.get_mut(panel_id) {
            let cur_scroll = match panel_mut.instance_states.get(widget_key) {
                Some(crate::widgets::WidgetInstanceState::List { scroll_offset, .. }) => {
                    *scroll_offset
                }
                _ => 0,
            };
            panel_mut.instance_states.insert(
                widget_key.to_string(),
                crate::widgets::WidgetInstanceState::List {
                    scroll_offset: cur_scroll,
                    selected_index: new_sel,
                },
            );
        }
        self.rerender_widget_panel(panel_id);
        if self
            .plugin_manager
            .read()
            .unwrap()
            .has_hook_handlers("widget_event")
        {
            self.plugin_manager.read().unwrap().run_hook(
                "widget_event",
                fresh_core::hooks::HookArgs::WidgetEvent {
                    panel_id,
                    widget_key: widget_key.to_string(),
                    event_type: "select".into(),
                    payload: serde_json::json!({ "index": new_sel, "key": new_key }),
                },
            );
        }
    }

    /// Move the focused Tree's selection up/down, skipping
    /// descendants of collapsed nodes. Selection is the *absolute*
    /// `nodes` index; we walk the visible-flat order to find the
    /// neighbour. Mirrors the List handler shape but tree-aware.
    fn handle_widget_tree_select_move(&mut self, panel_id: u64, delta: i32) {
        let focus_key = match self.widget_registry.get(panel_id) {
            Some(p) => p.focus_key.clone(),
            None => return,
        };
        if focus_key.is_empty() {
            return;
        }
        self.handle_widget_tree_select_move_for_key(panel_id, &focus_key, delta);
    }

    /// Tree counterpart of [`handle_widget_select_move_for_key`].
    fn handle_widget_tree_select_move_for_key(
        &mut self,
        panel_id: u64,
        widget_key: &str,
        delta: i32,
    ) {
        let panel = match self.widget_registry.get(panel_id) {
            Some(p) => p,
            None => return,
        };
        let widget = crate::widgets::find_widget_by_key(&panel.spec, widget_key);
        let (spec_sel, nodes, item_keys) = match widget {
            Some(fresh_core::api::WidgetSpec::Tree {
                selected_index,
                nodes,
                item_keys,
                ..
            }) => (*selected_index, nodes.clone(), item_keys.clone()),
            _ => return,
        };
        if nodes.is_empty() {
            return;
        }
        let (cur_sel, cur_scroll, expanded) = match panel.instance_states.get(widget_key) {
            Some(crate::widgets::WidgetInstanceState::Tree {
                selected_index,
                scroll_offset,
                expanded_keys,
            }) => (*selected_index, *scroll_offset, expanded_keys.clone()),
            _ => (spec_sel, 0u32, std::collections::HashSet::<String>::new()),
        };
        let visible_indices = collect_visible_tree_indices(&nodes, &item_keys, &expanded);
        if visible_indices.is_empty() {
            return;
        }
        let cur_pos = if cur_sel < 0 {
            if delta > 0 {
                -1
            } else {
                visible_indices.len() as i32
            }
        } else {
            visible_indices
                .iter()
                .position(|&v| v as i32 == cur_sel)
                .map(|p| p as i32)
                .unwrap_or(-1)
        };
        let new_pos = (cur_pos + delta).clamp(0, (visible_indices.len() as i32) - 1);
        let new_abs = visible_indices[new_pos as usize];
        let new_key = item_keys.get(new_abs).cloned().unwrap_or_default();
        if let Some(panel_mut) = self.widget_registry.get_mut(panel_id) {
            panel_mut.instance_states.insert(
                widget_key.to_string(),
                crate::widgets::WidgetInstanceState::Tree {
                    scroll_offset: cur_scroll,
                    selected_index: new_abs as i32,
                    expanded_keys: expanded,
                },
            );
        }
        self.rerender_widget_panel(panel_id);
        if self
            .plugin_manager
            .read()
            .unwrap()
            .has_hook_handlers("widget_event")
        {
            self.plugin_manager.read().unwrap().run_hook(
                "widget_event",
                fresh_core::hooks::HookArgs::WidgetEvent {
                    panel_id,
                    widget_key: widget_key.to_string(),
                    event_type: "select".into(),
                    payload: serde_json::json!({ "index": new_abs as i64, "key": new_key }),
                },
            );
        }
    }

    /// Mouse-wheel scroll over a widget panel buffer. Finds the
    /// first `Tree`/`List` in any panel rendering into `buffer_id`
    /// and shifts its viewport by `delta` rows. Drags the selection
    /// to stay inside the new visible window so the renderer's
    /// auto-scroll doesn't snap the offset back. No focus change,
    /// no `widget_event` fires — wheel is viewport navigation, not
    /// selection.
    ///
    /// Returns `true` if any panel consumed the scroll.
    pub(super) fn handle_widget_panel_wheel(
        &mut self,
        buffer_id: crate::model::event::BufferId,
        delta: i32,
    ) -> bool {
        let panels = self.widget_registry.panels_for_buffer(buffer_id);
        let mut consumed = false;
        for panel_id in panels {
            let spec = match self.widget_registry.get(panel_id) {
                Some(p) => p.spec.clone(),
                None => continue,
            };
            let Some(widget_key) = find_scrollable_widget_key(&spec) else {
                continue;
            };
            let widget = crate::widgets::find_widget_by_key(&spec, &widget_key);
            match widget {
                Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                    self.handle_widget_tree_wheel(panel_id, &widget_key, delta);
                    consumed = true;
                }
                Some(fresh_core::api::WidgetSpec::List { .. }) => {
                    self.handle_widget_list_wheel(panel_id, &widget_key, delta);
                    consumed = true;
                }
                _ => {}
            }
        }
        consumed
    }

    /// Shift a Tree's `scroll_offset` by `delta` rows. If the
    /// selection would fall outside the new viewport, drag it to
    /// the edge so the renderer's keep-selection-visible logic
    /// doesn't snap the offset back.
    fn handle_widget_tree_wheel(&mut self, panel_id: u64, widget_key: &str, delta: i32) {
        let panel = match self.widget_registry.get(panel_id) {
            Some(p) => p,
            None => return,
        };
        let widget = crate::widgets::find_widget_by_key(&panel.spec, widget_key);
        let (visible_rows, nodes, item_keys) = match widget {
            Some(fresh_core::api::WidgetSpec::Tree {
                visible_rows,
                nodes,
                item_keys,
                ..
            }) => (*visible_rows, nodes.clone(), item_keys.clone()),
            _ => return,
        };
        if nodes.is_empty() {
            return;
        }
        let (cur_sel, cur_scroll, expanded) = match panel.instance_states.get(widget_key) {
            Some(crate::widgets::WidgetInstanceState::Tree {
                selected_index,
                scroll_offset,
                expanded_keys,
            }) => (*selected_index, *scroll_offset, expanded_keys.clone()),
            _ => (-1, 0, std::collections::HashSet::<String>::new()),
        };
        let visible_indices = collect_visible_tree_indices(&nodes, &item_keys, &expanded);
        if visible_indices.is_empty() {
            return;
        }
        let visible = visible_rows.max(1);
        let total_visible = visible_indices.len() as u32;
        let max_scroll = total_visible.saturating_sub(visible);
        let new_scroll = (cur_scroll as i32 + delta).clamp(0, max_scroll as i32) as u32;
        if new_scroll == cur_scroll {
            return;
        }
        // Drag selection to stay inside the new viewport.
        let cur_pos: Option<u32> = if cur_sel >= 0 {
            visible_indices
                .iter()
                .position(|&v| v as i32 == cur_sel)
                .map(|p| p as u32)
        } else {
            None
        };
        let new_sel_abs = match cur_pos {
            Some(pos) if pos < new_scroll => visible_indices[new_scroll as usize] as i32,
            Some(pos) if pos >= new_scroll + visible => {
                visible_indices[(new_scroll + visible - 1) as usize] as i32
            }
            _ => cur_sel,
        };
        if let Some(panel_mut) = self.widget_registry.get_mut(panel_id) {
            panel_mut.instance_states.insert(
                widget_key.to_string(),
                crate::widgets::WidgetInstanceState::Tree {
                    scroll_offset: new_scroll,
                    selected_index: new_sel_abs,
                    expanded_keys: expanded,
                },
            );
        }
        self.rerender_widget_panel(panel_id);
    }

    /// List counterpart of `handle_widget_tree_wheel`.
    fn handle_widget_list_wheel(&mut self, panel_id: u64, widget_key: &str, delta: i32) {
        let panel = match self.widget_registry.get(panel_id) {
            Some(p) => p,
            None => return,
        };
        let widget = crate::widgets::find_widget_by_key(&panel.spec, widget_key);
        let (visible_rows, total) = match widget {
            Some(fresh_core::api::WidgetSpec::List {
                visible_rows,
                items,
                ..
            }) => (*visible_rows, items.len() as u32),
            _ => return,
        };
        if total == 0 {
            return;
        }
        let (cur_sel, cur_scroll) = match panel.instance_states.get(widget_key) {
            Some(crate::widgets::WidgetInstanceState::List {
                selected_index,
                scroll_offset,
            }) => (*selected_index, *scroll_offset),
            _ => (-1, 0),
        };
        let visible = visible_rows.max(1);
        let max_scroll = total.saturating_sub(visible);
        let new_scroll = (cur_scroll as i32 + delta).clamp(0, max_scroll as i32) as u32;
        if new_scroll == cur_scroll {
            return;
        }
        let new_sel = if cur_sel < 0 {
            cur_sel
        } else if (cur_sel as u32) < new_scroll {
            new_scroll as i32
        } else if (cur_sel as u32) >= new_scroll + visible {
            (new_scroll + visible - 1) as i32
        } else {
            cur_sel
        };
        if let Some(panel_mut) = self.widget_registry.get_mut(panel_id) {
            panel_mut.instance_states.insert(
                widget_key.to_string(),
                crate::widgets::WidgetInstanceState::List {
                    scroll_offset: new_scroll,
                    selected_index: new_sel,
                },
            );
        }
        self.rerender_widget_panel(panel_id);
    }

    /// Right/Left arrow on a focused Tree.
    ///
    /// * Right: if the selected node has children and is collapsed,
    ///   expand it. Else no-op.
    /// * Left: if the selected node has children and is expanded,
    ///   collapse it. Else move selection up to the parent.
    ///
    /// Both update host instance state, re-render, and (when a
    /// change happened) fire `widget_event { event_type: "expand" }`.
    fn handle_widget_tree_lateral(&mut self, panel_id: u64, is_right: bool) {
        let panel = match self.widget_registry.get(panel_id) {
            Some(p) => p,
            None => return,
        };
        let focus_key = panel.focus_key.clone();
        if focus_key.is_empty() {
            return;
        }
        let widget = crate::widgets::find_widget_by_key(&panel.spec, &focus_key);
        let (spec_sel, nodes, item_keys) = match widget {
            Some(fresh_core::api::WidgetSpec::Tree {
                selected_index,
                nodes,
                item_keys,
                ..
            }) => (*selected_index, nodes.clone(), item_keys.clone()),
            _ => return,
        };
        if nodes.is_empty() {
            return;
        }
        let (cur_sel, cur_scroll, mut expanded) = match panel.instance_states.get(&focus_key) {
            Some(crate::widgets::WidgetInstanceState::Tree {
                selected_index,
                scroll_offset,
                expanded_keys,
            }) => (*selected_index, *scroll_offset, expanded_keys.clone()),
            _ => (spec_sel, 0u32, std::collections::HashSet::<String>::new()),
        };
        if cur_sel < 0 {
            return;
        }
        let sel_idx = cur_sel as usize;
        let node = match nodes.get(sel_idx) {
            Some(n) => n,
            None => return,
        };
        let key = item_keys.get(sel_idx).cloned().unwrap_or_default();
        let was_expanded = !key.is_empty() && expanded.contains(&key);

        let mut new_sel = cur_sel;
        let mut expansion_changed: Option<bool> = None; // Some(new_state)
        if is_right {
            if node.has_children && !was_expanded && !key.is_empty() {
                expanded.insert(key.clone());
                expansion_changed = Some(true);
            }
        } else if node.has_children && was_expanded && !key.is_empty() {
            expanded.remove(&key);
            expansion_changed = Some(false);
        } else if let Some(parent_idx) = crate::widgets::tree_parent_index(&nodes, sel_idx) {
            new_sel = parent_idx as i32;
        }
        // No change → bail (don't fire spurious select/expand).
        if expansion_changed.is_none() && new_sel == cur_sel {
            return;
        }
        let final_key = item_keys.get(new_sel as usize).cloned().unwrap_or_default();
        if let Some(panel_mut) = self.widget_registry.get_mut(panel_id) {
            panel_mut.instance_states.insert(
                focus_key.clone(),
                crate::widgets::WidgetInstanceState::Tree {
                    scroll_offset: cur_scroll,
                    selected_index: new_sel,
                    expanded_keys: expanded,
                },
            );
        }
        self.rerender_widget_panel(panel_id);
        if self
            .plugin_manager
            .read()
            .unwrap()
            .has_hook_handlers("widget_event")
        {
            if let Some(now_expanded) = expansion_changed {
                self.plugin_manager.read().unwrap().run_hook(
                    "widget_event",
                    fresh_core::hooks::HookArgs::WidgetEvent {
                        panel_id,
                        widget_key: focus_key.clone(),
                        event_type: "expand".into(),
                        payload: serde_json::json!({
                            "index": cur_sel as i64,
                            "key": key,
                            "expanded": now_expanded,
                        }),
                    },
                );
            } else if new_sel != cur_sel {
                self.plugin_manager.read().unwrap().run_hook(
                    "widget_event",
                    fresh_core::hooks::HookArgs::WidgetEvent {
                        panel_id,
                        widget_key: focus_key,
                        event_type: "select".into(),
                        payload: serde_json::json!({
                            "index": new_sel as i64,
                            "key": final_key,
                        }),
                    },
                );
            }
        }
    }

    /// Toggle a Tree node's expansion state, re-render, and fire
    /// `widget_event { event_type: "expand" }`. Used by the click
    /// handler when the user clicks the disclosure column.
    pub(crate) fn handle_widget_tree_expand_toggle(
        &mut self,
        panel_id: u64,
        widget_key: &str,
        item_key: &str,
    ) {
        if widget_key.is_empty() || item_key.is_empty() {
            return;
        }
        let now_expanded = {
            let panel = match self.widget_registry.get_mut(panel_id) {
                Some(p) => p,
                None => return,
            };
            let (cur_scroll, cur_sel, mut expanded) = match panel.instance_states.get(widget_key) {
                Some(crate::widgets::WidgetInstanceState::Tree {
                    scroll_offset,
                    selected_index,
                    expanded_keys,
                }) => (*scroll_offset, *selected_index, expanded_keys.clone()),
                _ => (0u32, -1i32, std::collections::HashSet::<String>::new()),
            };
            let next = if expanded.contains(item_key) {
                expanded.remove(item_key);
                false
            } else {
                expanded.insert(item_key.to_string());
                true
            };
            panel.instance_states.insert(
                widget_key.to_string(),
                crate::widgets::WidgetInstanceState::Tree {
                    scroll_offset: cur_scroll,
                    selected_index: cur_sel,
                    expanded_keys: expanded,
                },
            );
            next
        };
        self.rerender_widget_panel(panel_id);
        if self
            .plugin_manager
            .read()
            .unwrap()
            .has_hook_handlers("widget_event")
        {
            self.plugin_manager.read().unwrap().run_hook(
                "widget_event",
                fresh_core::hooks::HookArgs::WidgetEvent {
                    panel_id,
                    widget_key: widget_key.to_string(),
                    event_type: "expand".into(),
                    payload: serde_json::json!({
                        "key": item_key,
                        "expanded": now_expanded,
                    }),
                },
            );
        }
    }

    /// Fire `widget_event { event_type: "activate" }` for the focused
    /// Tree's currently-selected node. Mirrors `fire_list_activate`
    /// — the plugin's handler decides what "activate" means
    /// (open the file, run an action, etc.).
    /// If the focused Tree row is checkable (parent tree has
    /// `checkable: true` *and* the row's `checked` is `Some(_)`),
    /// fire `widget_event { event_type: "toggle" }` with the
    /// inverted value and return `true`. Otherwise return `false`
    /// so the caller falls back to `activate`.
    ///
    /// Mirrors what a click on the row's `[v]`/`[ ]` glyph would
    /// do — Space is the conventional checkbox key, so on a
    /// checkable tree Space toggles instead of activating.
    fn fire_tree_toggle_if_checkable(&mut self, panel_id: u64, focus_key: &str) -> bool {
        let panel = match self.widget_registry.get(panel_id) {
            Some(p) => p,
            None => return false,
        };
        let widget = crate::widgets::find_widget_by_key(&panel.spec, focus_key);
        let (spec_sel, nodes, item_keys, checkable) = match widget {
            Some(fresh_core::api::WidgetSpec::Tree {
                selected_index,
                nodes,
                item_keys,
                checkable,
                ..
            }) => (*selected_index, nodes, item_keys.clone(), *checkable),
            _ => return false,
        };
        if !checkable {
            return false;
        }
        let sel = match panel.instance_states.get(focus_key) {
            Some(crate::widgets::WidgetInstanceState::Tree { selected_index, .. }) => {
                *selected_index
            }
            _ => spec_sel,
        };
        if sel < 0 {
            return false;
        }
        let cur_checked = match nodes.get(sel as usize).and_then(|n| n.checked) {
            Some(b) => b,
            None => return false, // No checkbox glyph on this row — let activate fire.
        };
        let new_checked = !cur_checked;
        let item_key = item_keys.get(sel as usize).cloned().unwrap_or_default();
        if self
            .plugin_manager
            .read()
            .unwrap()
            .has_hook_handlers("widget_event")
        {
            self.plugin_manager.read().unwrap().run_hook(
                "widget_event",
                fresh_core::hooks::HookArgs::WidgetEvent {
                    panel_id,
                    widget_key: focus_key.to_string(),
                    event_type: "toggle".into(),
                    payload: serde_json::json!({
                        "index": sel,
                        "key": item_key,
                        "checked": new_checked,
                    }),
                },
            );
        }
        true
    }

    fn fire_tree_activate(&mut self, panel_id: u64, focus_key: &str) {
        let panel = match self.widget_registry.get(panel_id) {
            Some(p) => p,
            None => return,
        };
        let widget = crate::widgets::find_widget_by_key(&panel.spec, focus_key);
        let (spec_sel, item_keys) = match widget {
            Some(fresh_core::api::WidgetSpec::Tree {
                selected_index,
                item_keys,
                ..
            }) => (*selected_index, item_keys.clone()),
            _ => return,
        };
        let sel = match panel.instance_states.get(focus_key) {
            Some(crate::widgets::WidgetInstanceState::Tree { selected_index, .. }) => {
                *selected_index
            }
            _ => spec_sel,
        };
        if sel < 0 {
            return;
        }
        let item_key = item_keys.get(sel as usize).cloned().unwrap_or_default();
        if self
            .plugin_manager
            .read()
            .unwrap()
            .has_hook_handlers("widget_event")
        {
            self.plugin_manager.read().unwrap().run_hook(
                "widget_event",
                fresh_core::hooks::HookArgs::WidgetEvent {
                    panel_id,
                    widget_key: focus_key.to_string(),
                    event_type: "activate".into(),
                    payload: serde_json::json!({
                        "index": sel,
                        "key": item_key,
                    }),
                },
            );
        }
    }

    /// Walk every panel rendering into `buffer_id` and return the
    /// first one whose currently-focused widget is a `Text`.
    /// Returns `None` when no such panel exists (e.g. when the
    /// buffer is a regular text buffer, or the panel has focus on
    /// a `Button` / `List` / etc.).
    ///
    /// This is the universal hook the clipboard ops use to route
    /// Paste / Copy / Cut / Select-All to a focused widget text
    /// field instead of the underlying buffer. Same idea as the
    /// existing Prompt and FileExplorer branches in the clipboard
    /// path, generalised: any plugin-mounted Text widget that has
    /// focus wins over the underlying buffer.
    pub(super) fn focused_text_widget_panel_for_buffer(
        &self,
        buffer_id: crate::model::event::BufferId,
    ) -> Option<u64> {
        for panel_id in self.widget_registry.panels_for_buffer(buffer_id) {
            let panel = self.widget_registry.get(panel_id)?;
            if panel.focus_key.is_empty() {
                continue;
            }
            let widget = crate::widgets::find_widget_by_key(&panel.spec, &panel.focus_key);
            if matches!(widget, Some(fresh_core::api::WidgetSpec::Text { .. })) {
                return Some(panel_id);
            }
        }
        None
    }

    /// Read the currently-selected text from the focused `Text`
    /// widget on the given panel, or `None` when nothing is
    /// selected (no anchor, or anchor == cursor). Used by the
    /// host-side Copy / Cut routing path.
    pub(super) fn focused_widget_selected_text(&self, panel_id: u64) -> Option<String> {
        let panel = self.widget_registry.get(panel_id)?;
        if panel.focus_key.is_empty() {
            return None;
        }
        match panel.instance_states.get(&panel.focus_key) {
            Some(crate::widgets::WidgetInstanceState::Text { editor, .. }) => {
                editor.selected_text()
            }
            _ => None,
        }
    }

    /// Select-all in the focused widget Text. Returns true when
    /// applied (focus was a Text widget). The op fires a `change`
    /// event only if the selection range actually changed; an
    /// already-fully-selected widget is a no-op.
    pub(super) fn handle_widget_select_all(&mut self, panel_id: u64) -> bool {
        // SelectAll moves the cursor to end-of-value and sets anchor
        // at start — `with_focused_text_editor` will skip re-render
        // when nothing changed, which is fine.
        self.with_focused_text_editor(panel_id, |editor| editor.select_all())
    }

    /// Copy the focused widget Text's current selection to the
    /// internal clipboard. Returns true when copy ran (even when
    /// the selection was empty — the action is consumed either way
    /// so it doesn't fall through to the buffer's copy path).
    pub(super) fn handle_widget_copy(&mut self, panel_id: u64) -> bool {
        if self.widget_registry.get(panel_id).is_none() {
            return false;
        }
        if let Some(text) = self.focused_widget_selected_text(panel_id) {
            self.clipboard.copy(text);
        }
        true
    }

    /// Cut the focused widget Text's current selection — copy then
    /// delete. With no selection, this is a no-op consume.
    pub(super) fn handle_widget_cut(&mut self, panel_id: u64) -> bool {
        if self.widget_registry.get(panel_id).is_none() {
            return false;
        }
        if let Some(text) = self.focused_widget_selected_text(panel_id) {
            self.clipboard.copy(text);
            self.with_focused_text_editor(panel_id, |editor| {
                editor.delete_selection();
            });
        }
        true
    }

    /// Insert `text` at the focused widget Text's cursor (replacing
    /// any active selection). Used by the host-side Paste routing
    /// path; `text` is already line-ending-normalised by the
    /// caller (CRLF / CR → LF). `TextEdit::insert_str` strips
    /// embedded newlines when the editor is single-line.
    pub(super) fn handle_widget_insert_str(&mut self, panel_id: u64, text: &str) -> bool {
        if self.widget_registry.get(panel_id).is_none() {
            return false;
        }
        let owned = text.to_string();
        self.with_focused_text_editor(panel_id, move |editor| {
            editor.insert_str(&owned);
        });
        true
    }

    /// Ensure `panel.instance_states[focus_key]` is a seeded
    /// `Text { editor, .. }` for the focused widget. If instance
    /// state already has the entry, no-op. If not, seeds from the
    /// spec's `value` / `cursor_byte` / `rows`. Returns true on
    /// success (focus is a Text widget that's now in instance state),
    /// false otherwise.
    fn ensure_focused_text_seeded(&mut self, panel_id: u64, focus_key: &str) -> bool {
        let panel = match self.widget_registry.get_mut(panel_id) {
            Some(p) => p,
            None => return false,
        };
        if matches!(
            panel.instance_states.get(focus_key),
            Some(crate::widgets::WidgetInstanceState::Text { .. })
        ) {
            return true;
        }
        let widget = crate::widgets::find_widget_by_key(&panel.spec, focus_key);
        let (value, cursor_byte, multiline) = match widget {
            Some(fresh_core::api::WidgetSpec::Text {
                value,
                cursor_byte,
                rows,
                ..
            }) => (value.clone(), *cursor_byte, *rows > 1),
            _ => return false,
        };
        let mut editor = if multiline {
            crate::primitives::text_edit::TextEdit::with_text(&value)
        } else {
            crate::primitives::text_edit::TextEdit::single_line_with_text(&value)
        };
        let seed = if cursor_byte < 0 {
            value.len()
        } else {
            (cursor_byte as usize).min(value.len())
        };
        editor.set_cursor_from_flat(seed);
        panel.instance_states.insert(
            focus_key.to_string(),
            crate::widgets::WidgetInstanceState::Text { editor, scroll: 0 },
        );
        true
    }

    /// Apply a mutating operation to the focused `Text` widget's
    /// `TextEdit`. Handles seeding the editor from the spec on first
    /// touch, no-op detection (skips rerender + change event), and
    /// firing the `widget_event` "change" hook with the post-state.
    ///
    /// Returns true when the op ran *and* produced a visible change.
    pub(super) fn with_focused_text_editor<F>(&mut self, panel_id: u64, op: F) -> bool
    where
        F: FnOnce(&mut crate::primitives::text_edit::TextEdit),
    {
        let focus_key = match self.widget_registry.get(panel_id) {
            Some(p) if !p.focus_key.is_empty() => p.focus_key.clone(),
            _ => return false,
        };
        if !self.ensure_focused_text_seeded(panel_id, &focus_key) {
            return false;
        }
        let (before_value, before_cursor) = {
            let panel = self.widget_registry.get(panel_id).unwrap();
            match panel.instance_states.get(&focus_key) {
                Some(crate::widgets::WidgetInstanceState::Text { editor, .. }) => {
                    (editor.value(), editor.flat_cursor_byte())
                }
                _ => return false,
            }
        };
        {
            let panel = self.widget_registry.get_mut(panel_id).unwrap();
            match panel.instance_states.get_mut(&focus_key) {
                Some(crate::widgets::WidgetInstanceState::Text { editor, .. }) => op(editor),
                _ => return false,
            }
        }
        let (after_value, after_cursor) = {
            let panel = self.widget_registry.get(panel_id).unwrap();
            match panel.instance_states.get(&focus_key) {
                Some(crate::widgets::WidgetInstanceState::Text { editor, .. }) => {
                    (editor.value(), editor.flat_cursor_byte())
                }
                _ => return false,
            }
        };
        if after_value == before_value && after_cursor == before_cursor {
            return false;
        }
        self.rerender_widget_panel(panel_id);
        if self
            .plugin_manager
            .read()
            .unwrap()
            .has_hook_handlers("widget_event")
        {
            self.plugin_manager.read().unwrap().run_hook(
                "widget_event",
                fresh_core::hooks::HookArgs::WidgetEvent {
                    panel_id,
                    widget_key: focus_key.clone(),
                    event_type: "change".into(),
                    payload: serde_json::json!({
                        "value": after_value,
                        "cursorByte": after_cursor as i64,
                    }),
                },
            );
        }
        true
    }

    /// Apply a non-printable editing key to the focused text widget
    /// by dispatching to the corresponding `TextEdit` method. The
    /// single/multi-line discriminator is carried by `TextEdit`'s
    /// `multiline` field, so the same set of methods serves both
    /// kinds — single-line just no-ops on Up/Down/Enter.
    fn handle_widget_text_key(&mut self, panel_id: u64, key: &str) {
        self.with_focused_text_editor(panel_id, |editor| match key {
            "Backspace" => editor.backspace(),
            "Delete" => editor.delete(),
            "Left" => editor.move_left(),
            "Right" => editor.move_right(),
            "Up" => editor.move_up(),
            "Down" => editor.move_down(),
            "Home" => editor.move_home(),
            "End" => editor.move_end(),
            "Enter" => editor.insert_char('\n'),
            _ => { /* unknown key — no-op */ }
        });
    }

    /// Insert printable / IME-committed text at the focused text
    /// widget's cursor. Same path for single-line and multi-line —
    /// `TextEdit::insert_str` strips `\n` automatically when the
    /// editor was constructed single-line. `text` may be a single
    /// codepoint, a grapheme cluster, or a multi-codepoint IME
    /// commit; `insert_str` handles each identically.
    fn handle_widget_text_char(&mut self, panel_id: u64, text: &str) {
        if text.is_empty() {
            return;
        }
        let text = text.to_string();
        self.with_focused_text_editor(panel_id, move |editor| {
            editor.insert_str(&text);
        });
    }

    fn handle_unmount_widget_panel(&mut self, panel_id: u64) {
        match self.widget_registry.unmount(panel_id) {
            Some(buffer_id) => {
                tracing::debug!(
                    "Unmounted widget panel {} (was rendering into {:?})",
                    panel_id,
                    buffer_id
                );
                // Buffer lifetime is owned by the plugin (it created the
                // virtual buffer before mounting). The plugin is
                // responsible for closing/clearing it; we only forget our
                // panel state.
            }
            None => {
                tracing::debug!("UnmountWidgetPanel for unknown panel {} ignored", panel_id);
            }
        }
    }

    fn handle_mount_floating_widget(
        &mut self,
        panel_id: u64,
        spec: fresh_core::api::WidgetSpec,
        width_pct: u8,
        height_pct: u8,
    ) {
        let width_pct = width_pct.clamp(1, 100);
        let height_pct = height_pct.clamp(1, 100);
        if let Some(existing) = self.floating_widget_panel.take() {
            if existing.panel_id != panel_id {
                let _ = self.widget_registry.unmount(existing.panel_id);
            }
        }
        self.floating_widget_panel = Some(FloatingWidgetState {
            panel_id,
            width_pct,
            height_pct,
            entries: Vec::new(),
            focus_cursor: None,
            embeds: Vec::new(),
            overlays: Vec::new(),
            last_inner_rect: None,
        });
        let prev = std::collections::HashMap::new();
        let prev_focus = String::new();
        let panel_width = self.floating_panel_inner_width();
        let out = crate::widgets::render_spec(&spec, &prev, &prev_focus, panel_width);
        let focus_cursor = out.focus_cursor;
        let entries = out.entries;
        let embeds = out.embeds;
        let overlays = out.overlays;
        self.widget_registry.mount(
            panel_id,
            FLOATING_PANEL_BUFFER_ID,
            spec,
            out.hits,
            out.instance_states,
            out.focus_key,
            out.tabbable,
        );
        if let Some(fwp) = self.floating_widget_panel.as_mut() {
            fwp.entries = entries;
            fwp.focus_cursor = focus_cursor;
            fwp.embeds = embeds;
            fwp.overlays = overlays;
        }
        tracing::debug!(
            "Mounted floating widget panel {} ({}%x{}%)",
            panel_id,
            width_pct,
            height_pct
        );
    }

    fn handle_update_floating_widget(&mut self, panel_id: u64, spec: fresh_core::api::WidgetSpec) {
        match self.floating_widget_panel.as_ref() {
            Some(fwp) if fwp.panel_id == panel_id => {}
            _ => {
                tracing::debug!(
                    "UpdateFloatingWidget for unknown / mismatched panel {} ignored",
                    panel_id
                );
                return;
            }
        }
        let prev = self
            .widget_registry
            .instance_states(panel_id)
            .cloned()
            .unwrap_or_default();
        let prev_focus = self
            .widget_registry
            .focus_key(panel_id)
            .map(|s| s.to_string())
            .unwrap_or_default();
        let panel_width = self.floating_panel_inner_width();
        let out = crate::widgets::render_spec(&spec, &prev, &prev_focus, panel_width);
        let focus_cursor = out.focus_cursor;
        let entries = out.entries;
        let embeds = out.embeds;
        let overlays = out.overlays;
        if self
            .widget_registry
            .update(
                panel_id,
                spec,
                out.hits,
                out.instance_states,
                out.focus_key,
                out.tabbable,
            )
            .is_err()
        {
            tracing::debug!(
                "UpdateFloatingWidget for unknown panel {} ignored (not in registry)",
                panel_id
            );
            return;
        }
        if let Some(fwp) = self.floating_widget_panel.as_mut() {
            fwp.entries = entries;
            fwp.focus_cursor = focus_cursor;
            fwp.embeds = embeds;
            fwp.overlays = overlays;
        }
    }

    fn handle_unmount_floating_widget(&mut self, panel_id: u64) {
        match self.floating_widget_panel.as_ref() {
            Some(fwp) if fwp.panel_id == panel_id => {}
            _ => {
                tracing::debug!(
                    "UnmountFloatingWidget for unknown / mismatched panel {} ignored",
                    panel_id
                );
                return;
            }
        }
        self.floating_widget_panel = None;
        let _ = self.widget_registry.unmount(panel_id);
        tracing::debug!("Unmounted floating widget panel {}", panel_id);
    }

    /// Inner-rect column budget for a floating panel render — the
    /// terminal width × `width_pct`, minus 2 cols for the frame
    /// border. Mirrors the `widget_panel_width` reservation; never
    /// goes below 10 cols so flex spacers don't collapse to zero on
    /// narrow terminals.
    pub(super) fn floating_panel_inner_width(&self) -> u32 {
        let term_w = self.terminal_width.max(1) as u32;
        let pct = self
            .floating_widget_panel
            .as_ref()
            .map(|f| f.width_pct.clamp(1, 100) as u32)
            .unwrap_or(80);
        let w = (term_w * pct) / 100;
        w.saturating_sub(2).max(10)
    }

    fn handle_get_text_properties_at_cursor(&self, buffer_id: BufferId) {
        if let Some(state) = self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .get(&buffer_id)
        {
            let cursor_pos = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
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
            self.active_window_mut()
                .active_custom_contexts
                .insert(name.clone());
            tracing::debug!("Set custom context: {}", name);
        } else {
            self.active_window_mut()
                .active_custom_contexts
                .remove(&name);
            tracing::debug!("Unset custom context: {}", name);
        }
    }

    fn handle_disable_lsp_for_language(&mut self, language: String) {
        tracing::info!("Disabling LSP for language: {}", language);
        let __active_id = self.active_window;
        if let Some(lsp) = self
            .windows
            .get_mut(&__active_id)
            .and_then(|w| w.lsp.as_mut())
        {
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
            self.active_window_mut().status_message = Some(format!(
                "LSP disabled for {} (config save failed)",
                language
            ));
        } else {
            self.active_window_mut().status_message =
                Some(format!("LSP disabled for {}", language));
        }
        self.active_window_mut().warning_domains.lsp.clear();
    }

    fn handle_restart_lsp_for_language(&mut self, language: String) {
        tracing::info!("Plugin restarting LSP for language: {}", language);
        let file_path = self
            .active_window()
            .buffer_metadata
            .get(&self.active_buffer())
            .and_then(|meta| meta.file_path().cloned());
        let __active_id = self.active_window;
        let success = if let Some(lsp) = self
            .windows
            .get_mut(&__active_id)
            .and_then(|w| w.lsp.as_mut())
        {
            let (ok, msg) = lsp.manual_restart(&language, file_path.as_deref());
            self.active_window_mut().status_message = Some(msg);
            ok
        } else {
            self.active_window_mut().status_message = Some("No LSP manager available".to_string());
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
                let __active_id = self.active_window;
                if let Some(lsp) = self
                    .windows
                    .get_mut(&__active_id)
                    .and_then(|w| w.lsp.as_mut())
                {
                    let restarted = lsp.set_language_root_uri(&language, parsed_uri);
                    if restarted {
                        self.active_window_mut().status_message = Some(format!(
                            "LSP root updated for {} (restarting server)",
                            language
                        ));
                    } else {
                        self.active_window_mut().status_message =
                            Some(format!("LSP root set for {}", language));
                    }
                }
            }
            Err(e) => {
                tracing::error!("Invalid LSP root URI '{}': {}", uri, e);
                self.active_window_mut().status_message =
                    Some(format!("Invalid LSP root URI: {}", e));
            }
        }
    }

    fn handle_create_scroll_sync_group(
        &mut self,
        group_id: crate::view::scroll_sync::ScrollSyncGroupId,
        left_split: SplitId,
        right_split: SplitId,
    ) {
        let success = self
            .active_window_mut()
            .scroll_sync_manager
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
        self.active_window_mut()
            .scroll_sync_manager
            .set_anchors(group_id, sync_anchors);
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
        if self
            .active_window_mut()
            .scroll_sync_manager
            .remove_group(group_id)
        {
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
                        .read()
                        .unwrap()
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
        if let Some(handle) = self.active_window().terminal_manager.get(terminal_id) {
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
            .active_window()
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
            self.active_window_mut().terminal_manager.close(terminal_id);
            tracing::info!("Plugin closed terminal {:?} (no buffer found)", terminal_id);
        }
    }

    /// Fan `signal` out to every process group the window
    /// identified by `id` is tracking. The window's authority-
    /// configured signaller (see `app/window/process_group.rs`)
    /// decides how the signal is delivered. Failures from
    /// individual groups land in the tracing log so a partial
    /// failure surfaces without aborting the rest of the
    /// stop flow.
    fn handle_signal_window(&mut self, id: fresh_core::WindowId, signal: &str) {
        let Some(window) = self.windows.get_mut(&id) else {
            tracing::warn!("Plugin SignalWindow targeted unknown window {:?}", id);
            return;
        };
        let results = window.process_groups.signal_all(signal);
        for (entry, result) in results {
            match result {
                Ok(true) => tracing::info!(
                    "SignalWindow {:?}: {} → pid {} ({})",
                    id,
                    signal,
                    entry.leader_pid,
                    entry.label
                ),
                Ok(false) => tracing::debug!(
                    "SignalWindow {:?}: pid {} ({}) already exited",
                    id,
                    entry.leader_pid,
                    entry.label
                ),
                Err(e) => tracing::warn!(
                    "SignalWindow {:?}: pid {} ({}): {}",
                    id,
                    entry.leader_pid,
                    entry.label,
                    e
                ),
            }
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

impl Window {
    /// Populate the per-window fields of the plugin state snapshot.
    ///
    /// Called by `Editor::update_plugin_state_snapshot` while it holds
    /// the snapshot write lock. Covers everything that a single Window
    /// owns: active buffer/split ids, all this window's buffers (with
    /// per-buffer view-mode, compose state, preview flag, split
    /// membership), per-buffer cursor positions and text properties,
    /// the active buffer's cursors / viewport / selected text, the
    /// per-split snapshot list, this window's active-session plugin
    /// state, this window's authority label, diagnostics, folding
    /// ranges, editor mode, and the per-window plugin view states.
    /// Editor-wide fields (clipboard, windows list, config cache,
    /// user_config_raw, plugin_global_state) are populated by the
    /// Editor coda after this returns.
    #[cfg(feature = "plugins")]
    pub(crate) fn populate_plugin_state_snapshot(
        &mut self,
        snapshot: &mut fresh_core::api::EditorStateSnapshot,
    ) {
        use fresh_core::api::{BufferInfo, CursorInfo, ViewportInfo};

        // Rebuild only on registry mutation. Compares the registry's
        // monotonic catalog_gen against the last-seen value on the
        // snapshot — a single integer check, no allocation, no
        // count-mismatch ambiguity between the syntect set and the
        // unified catalog.
        let current_gen = self.resources.grammar_registry.catalog_gen();
        if snapshot.last_grammar_gen != current_gen {
            snapshot.available_grammars = self
                .resources
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
            snapshot.last_grammar_gen = current_gen;
        }

        snapshot.active_buffer_id = self.active_buffer();

        let (mgr_ref, vs_ref) = self
            .buffers
            .splits()
            .expect("active window must have a populated split layout");
        let active_split = mgr_ref.active_split();
        snapshot.active_split_id = active_split.0 .0;

        // Clear and update buffer info
        snapshot.buffers.clear();
        snapshot.buffer_saved_diffs.clear();
        snapshot.buffer_cursor_positions.clear();
        snapshot.buffer_text_properties.clear();

        let active_vs_opt = vs_ref.get(&active_split);
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
            let view_mode = active_vs_opt
                .and_then(|vs| vs.buffer_state(*buffer_id))
                .map(|bs| match bs.view_mode {
                    crate::state::ViewMode::Source => "source",
                    crate::state::ViewMode::PageView => "compose",
                })
                .unwrap_or("source");
            let compose_width = active_vs_opt
                .and_then(|vs| vs.buffer_state(*buffer_id))
                .and_then(|bs| bs.compose_width);
            let is_composing_in_any_split = vs_ref.values().any(|vs| {
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
            let splits: Vec<fresh_core::SplitId> = mgr_ref
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
            let source_split = vs_ref.iter().find(|(split_id, vs)| {
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

        // Update cursor information for active buffer.
        //
        // Use `effective_active_pair()` for the split id rather than
        // the split manager's outer `active_split()`. When the active
        // split holds a buffer-group tab, the user's keystrokes (and
        // therefore the meaningful cursor) live in the focused inner
        // panel's leaf — `focused_group_leaf` — not the outer leaf.
        // Reading the outer's cursor here would publish (0, 0) into
        // the snapshot while the user is editing the inner panel,
        // which is what `editor.getCursorPosition()` then sees.
        let active_buf_id = snapshot.active_buffer_id;
        let active_split_id = self.effective_active_pair().0;
        self.buffers
            .with_all_mut(|buffers_mut, mgr, vs_map| {
                let _ = mgr; // active_split_id was computed above
                if let Some(active_vs) = vs_map.get(&active_split_id) {
                    // Primary cursor (from SplitViewState)
                    let active_cursors = &active_vs.cursors;
                    let primary = active_cursors.primary();
                    let primary_position = primary.position;
                    let primary_selection = primary.selection_range();

                    snapshot.primary_cursor = Some(CursorInfo {
                        position: primary_position,
                        selection: primary_selection.clone(),
                    });

                    snapshot.all_cursors = active_cursors
                        .iter()
                        .map(|(_, cursor)| CursorInfo {
                            position: cursor.position,
                            selection: cursor.selection_range(),
                        })
                        .collect();

                    // Selected text from primary cursor (for clipboard plugin)
                    if let Some(range) = primary_selection {
                        if let Some(active_state) = buffers_mut.get_mut(&active_buf_id) {
                            snapshot.selected_text =
                                Some(active_state.get_text_range(range.start, range.end));
                        }
                    }

                    // Viewport — get from SplitViewState (the authoritative source)
                    let top_line = buffers_mut.get(&active_buf_id).and_then(|state| {
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

                // Per-split snapshot
                snapshot.splits.clear();
                for (leaf_id, vs) in vs_map.iter() {
                    let buf_id = vs.active_buffer;
                    let top_line = buffers_mut.get(&buf_id).and_then(|state| {
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
            })
            .expect("active window must have a populated split layout");

        // Mirror the active session's plugin_state into the snapshot
        // so getWindowState reads cheaply. Cloning is fine here: the
        // per-session state is small; plugins that store megabyte-
        // scale blobs in setWindowState will see proportional snapshot-
        // update cost, which is the desired feedback signal.
        snapshot.active_session_plugin_states = self.plugin_state.clone();
        // `authority_label` is populated by the Editor coda — see the
        // comment there for why it can't come from `self.resources`.

        // Update LSP diagnostics / folding ranges: Arc refcount bumps.
        snapshot.diagnostics = Arc::clone(&self.stored_diagnostics);
        snapshot.folding_ranges = Arc::clone(&self.stored_folding_ranges);

        // Update editor mode (for vi mode and other modal editing)
        snapshot.editor_mode = self.editor_mode.clone();

        // Update plugin view states from active split's BufferViewState.plugin_state.
        // If the active split changed, fully repopulate. Otherwise, merge
        // using or_insert to preserve JS-side write-through entries that
        // haven't round-tripped through the command channel yet.
        let active_split_id_u64 = active_split_id.0 .0;
        let split_changed = snapshot.plugin_view_states_split != active_split_id_u64;
        if split_changed {
            snapshot.plugin_view_states.clear();
            snapshot.plugin_view_states_split = active_split_id_u64;
        }

        // Clean up entries for buffers that are no longer open
        {
            let open_bids: Vec<_> = snapshot.buffers.keys().copied().collect();
            snapshot
                .plugin_view_states
                .retain(|bid, _| open_bids.contains(bid));
        }

        // Merge from Rust-side plugin_state (source of truth for persisted state)
        if let Some(vs_map) = self.buffers.split_view_states() {
            if let Some(active_vs) = vs_map.get(&active_split_id) {
                for (buffer_id, buf_state) in &active_vs.keyed_states {
                    if !buf_state.plugin_state.is_empty() {
                        let entry = snapshot.plugin_view_states.entry(*buffer_id).or_default();
                        for (key, value) in &buf_state.plugin_state {
                            entry.entry(key.clone()).or_insert_with(|| value.clone());
                        }
                    }
                }
            }
        }
    }
}
