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
use super::{Editor, FloatingWidgetState};

/// Normalize a session path for the plugin API. Sessions reach `WindowInfo`
/// from two sources — the canonicalized launch session and `create_window_at`'s
/// raw `PathBuf` — so any byte-level path field (lex sort, equality, …) in a
/// plugin needs them encoded the same way. On Windows that means resolving
/// 8.3 short names (`RUNNER~1` → `runneradmin`) and stripping the `\\?\`
/// verbatim prefix `canonicalize` adds. No-op on non-Windows.
///
/// `canonicalize` only works on paths that exist on disk. Worktree session
/// roots created by the orchestrator often point at directories that haven't
/// been materialized yet (`<repo>/wt-<name>`), so a naïve canonicalize would
/// leave them in their original 8.3 short-name form while the launch session
/// — whose root exists — gets the long-name form, and lex compare inverts
/// on case (`R` 0x52 < `r` 0x72). Walk up to the deepest existing ancestor,
/// canonicalize that, then re-attach the missing tail so siblings share the
/// same prefix encoding regardless of which exist.
fn normalize_plugin_path(path: std::path::PathBuf) -> std::path::PathBuf {
    #[cfg(windows)]
    {
        let canonical = canonicalize_deepest_existing(&path);
        let s = canonical.to_string_lossy();
        if let Some(stripped) = s.strip_prefix(r"\\?\") {
            return std::path::PathBuf::from(stripped);
        }
        return canonical;
    }
    #[cfg(not(windows))]
    path
}

#[cfg(windows)]
fn canonicalize_deepest_existing(path: &std::path::Path) -> std::path::PathBuf {
    if let Ok(c) = path.canonicalize() {
        return c;
    }
    // Walk up to the deepest ancestor that does canonicalize, then re-attach
    // the components we walked past. Falls back to the raw path if no
    // ancestor canonicalizes (drive root missing, etc).
    let mut tail: Vec<&std::ffi::OsStr> = Vec::new();
    let mut ancestor = path;
    loop {
        let Some(parent) = ancestor.parent() else {
            return path.to_path_buf();
        };
        if let Some(name) = ancestor.file_name() {
            tail.push(name);
        }
        if let Ok(c) = parent.canonicalize() {
            let mut out = c;
            for name in tail.iter().rev() {
                out.push(name);
            }
            return out;
        }
        ancestor = parent;
    }
}

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
    /// Shift plugin interval markers for one buffer to track an edit at `pos`
    /// that removed `removed` bytes and inserted `inserted` bytes. Called from
    /// the edit-apply path *before* the post-edit snapshot refresh + hook, so a
    /// plugin querying markers in after_insert/after_delete sees current
    /// coordinates. Insert gives `start` right-gravity / `end` left-gravity at
    /// the boundary; delete clamps into the deletion. Markers whose interior is
    /// touched keep shifting here (the plugin deletes + re-discovers them).
    ///
    /// Also a `#[doc(hidden)]` test hook: integration tests call this to
    /// deterministically reproduce the cross-thread marker/event desync (shift a
    /// plugin marker without editing the buffer) that the async `lines_changed`
    /// pipeline otherwise only produces as a timing race.
    #[doc(hidden)]
    pub fn shift_plugin_markers_for_edit(
        &self,
        buffer_id: BufferId,
        pos: usize,
        removed: usize,
        inserted: usize,
    ) {
        let Some(handle) = self.plugin_manager.read().unwrap().state_snapshot_handle() else {
            return;
        };
        let Ok(mut snapshot) = handle.write() else {
            return;
        };
        let Some(markers) = snapshot.plugin_markers.get_mut(&buffer_id) else {
            return;
        };
        // Reduce a same-position replacement (both lengths non-zero, as the bulk
        // path passes for type-over-selection / paste-over / query-replace / LSP
        // code actions) to its NET delta, mirroring `marker_list` and
        // `coord_map::record_replace`. The old code applied only the deletion
        // clamp and dropped `inserted`, so every plugin marker past such an edit
        // landed off by `inserted` bytes (the wrong direction when ins > del).
        let (eff_removed, eff_inserted) = if removed > 0 && inserted > 0 {
            if inserted >= removed {
                (0, inserted - removed)
            } else {
                (removed - inserted, 0)
            }
        } else {
            (removed, inserted)
        };
        for m in markers.values_mut() {
            if eff_removed == 0 {
                if m.start >= pos {
                    m.start += eff_inserted;
                }
                if m.end > pos {
                    m.end += eff_inserted;
                }
            } else {
                let d0 = pos;
                let d1 = pos + eff_removed;
                let clamp = |x: usize| {
                    if x <= d0 {
                        x
                    } else if x >= d1 {
                        x - eff_removed
                    } else {
                        d0
                    }
                };
                m.start = clamp(m.start);
                m.end = clamp(m.end);
            }
        }
    }

    pub fn update_plugin_state_snapshot(&mut self) {
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
        snapshot.working_dir = self.working_dir().to_path_buf();

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
        snapshot.authority_label = self.authority().display_label.clone();

        // Surface the active project's Workspace Trust level so plugins that
        // run repo-controlled work can gate on it.
        snapshot.workspace_trust_level = self
            .authority()
            .workspace_trust
            .level()
            .as_str()
            .to_string();
        snapshot.env_active = self.authority().env_provider.is_active();

        // Core is the *only* place that detects which environment a workspace
        // has. The env-manager plugin reads this resolved result via
        // `editor.detectedEnv()` rather than probing the filesystem itself.
        // Empty string ⇒ no env detected.
        snapshot.detected_env = crate::services::workspace_trust::detect_env(
            self.working_dir(),
            &self.config.env.detectors,
        )
        .and_then(|d| serde_json::to_string(&d).ok())
        .unwrap_or_default();

        // Publish the session list so plugins (Orchestrator, etc.)
        // see updates from createWindow/closeWindow without
        // a separate notification path. Sorted by id for
        // deterministic order — `next_window_id` is monotonic
        // so this is "creation order".
        // Dormant remote sessions (SSH / kube discovered at boot, not yet
        // connected) have no `Window` — they live in `dormant_remote` as
        // authority-less descriptors. They must still appear in the dock, so
        // fold them into the snapshot alongside the live windows. A dive
        // promotes one to a real window (removing it from `dormant_remote`), so
        // the two collections are disjoint and never double-list a session.
        let dormant_infos = self.dormant_remote.values().map(|d| {
            let slot = d.plugin_state.get("orchestrator");
            let project_path = slot
                .and_then(|m| m.get("project_path"))
                .and_then(|v| v.as_str())
                .filter(|p| !p.is_empty())
                .map(std::path::PathBuf::from)
                .or_else(|| d.project_path.clone())
                .unwrap_or_else(|| d.root.clone());
            fresh_core::api::WindowInfo {
                id: fresh_core::WindowId(d.id),
                label: d.label.clone(),
                root: normalize_plugin_path(d.root.clone()),
                project_path: normalize_plugin_path(project_path),
                shared_worktree: d.shared_worktree,
            }
        });
        let mut session_infos: Vec<fresh_core::api::WindowInfo> = self
            .windows
            .values()
            .map(|s| {
                let slot = s.plugin_state.get("orchestrator");
                // Normalise project_path at the API boundary: explicit
                // non-empty value if the orchestrator recorded one,
                // otherwise the session's root. Filtering empty strings
                // is the same guard the plugin used to apply via
                // `?? root` / `|| root` — now centralised so plugins
                // can treat `project_path` as an always-set `string`.
                let project_path = slot
                    .and_then(|m| m.get("project_path"))
                    .and_then(|v| v.as_str())
                    .filter(|p| !p.is_empty())
                    .map(std::path::PathBuf::from)
                    .unwrap_or_else(|| s.root.clone());
                let shared_worktree = slot
                    .and_then(|m| m.get("shared_worktree"))
                    .and_then(|v| v.as_bool())
                    .unwrap_or(false);
                fresh_core::api::WindowInfo {
                    id: s.id,
                    label: s.label.clone(),
                    root: normalize_plugin_path(s.root.clone()),
                    project_path: normalize_plugin_path(project_path),
                    shared_worktree,
                }
            })
            .collect();
        session_infos.extend(dormant_infos);
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
            PluginCommand::ClearOverlaysInRangeForNamespace {
                buffer_id,
                namespace,
                start,
                end,
            } => {
                self.handle_clear_overlays_in_range_for_namespace(buffer_id, namespace, start, end);
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
                epoch,
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
                    epoch,
                );
            }
            PluginCommand::ClearVirtualTextNamespace {
                buffer_id,
                namespace,
            } => {
                self.handle_clear_virtual_text_namespace(buffer_id, namespace);
            }
            PluginCommand::ClearVirtualLinesInRange {
                buffer_id,
                namespace,
                start,
                end,
                epoch,
            } => {
                self.handle_clear_virtual_lines_in_range(buffer_id, namespace, start, end, epoch);
            }

            // ==================== Conceal Commands ====================
            PluginCommand::AddConceal {
                buffer_id,
                namespace,
                start,
                end,
                replacement,
                epoch,
            } => {
                self.handle_add_conceal(buffer_id, namespace, start, end, replacement, epoch);
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
                epoch,
            } => {
                self.handle_clear_conceals_in_range(buffer_id, start, end, epoch);
            }
            PluginCommand::ClearConcealsInRangeForNamespace {
                buffer_id,
                namespace,
                start,
                end,
                epoch,
            } => {
                self.handle_clear_conceals_in_range_for_namespace(
                    buffer_id, namespace, start, end, epoch,
                );
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
                epoch,
            } => {
                self.handle_add_soft_break(buffer_id, namespace, position, indent, epoch);
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
                epoch,
            } => {
                self.handle_clear_soft_breaks_in_range(buffer_id, start, end, epoch);
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
                self.handle_set_split_label(split_id, label);
            }
            PluginCommand::ClearSplitLabel { split_id } => {
                self.handle_clear_split_label(split_id);
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
            PluginCommand::SetIndentationGuide { buffer_id, enabled } => {
                self.handle_set_indentation_guide(buffer_id, enabled);
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
            PluginCommand::SetFileExplorerSlots { namespace, slots } => {
                self.active_window_mut()
                    .handle_set_file_explorer_slots(namespace, slots);
            }
            PluginCommand::ClearFileExplorerSlots { namespace } => {
                self.active_window_mut()
                    .handle_clear_file_explorer_slots(&namespace);
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
            PluginCommand::AddPluginConfigField {
                plugin_name,
                field_name,
                field_schema,
            } => {
                self.handle_add_plugin_config_field(plugin_name, field_name, field_schema);
            }
            PluginCommand::ReloadThemes { apply_theme } => {
                self.handle_reload_themes(apply_theme);
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
            PluginCommand::CancelPrompt => {
                self.cancel_prompt();
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
                self.handle_set_key_capture_active(active);
            }
            PluginCommand::SetPromptSuggestions {
                suggestions,
                selected_index,
            } => {
                self.handle_set_prompt_suggestions(suggestions, selected_index);
            }
            PluginCommand::SetPromptInputSync { sync } => {
                self.handle_set_prompt_input_sync(sync);
            }
            PluginCommand::SetPromptTitle { title } => {
                self.handle_set_prompt_title(title);
            }
            PluginCommand::SetPromptFooter { footer } => {
                self.handle_set_prompt_footer(footer);
            }
            PluginCommand::SetPromptToolbar { spec } => {
                self.handle_set_prompt_toolbar(spec);
            }
            PluginCommand::ToggleOverlayToolbarWidget { key } => {
                self.toggle_overlay_toolbar_widget(&key);
            }
            PluginCommand::SetPromptStatus { status } => {
                self.handle_set_prompt_status(status);
            }
            PluginCommand::SetPromptSelectedIndex { index } => {
                self.handle_set_prompt_selected_index(index);
            }

            // ==================== Session lifecycle ====================
            // See docs/internal/orchestrator-sessions-design.md.
            PluginCommand::CreateWindow { root, label } => {
                self.handle_create_window(root, label);
            }
            PluginCommand::CreateWindowWithTerminal {
                root,
                label,
                cwd,
                command,
                title,
                resume,
                request_id,
            } => {
                self.handle_create_window_with_terminal(
                    root, label, cwd, command, title, resume, request_id,
                );
            }
            PluginCommand::SetActiveWindow { id } => {
                // Diving into a dormant remote session connects its backend and
                // promotes it to a real window (born with that authority) rather
                // than activating a placeholder — see `bring_dormant_remote_online`.
                if self.dormant_remote.contains_key(&id) {
                    self.bring_dormant_remote_online(id);
                } else {
                    self.set_active_window(id);
                }
            }
            PluginCommand::SetActiveWindowAnimated { id, from_edge } => {
                if self.dormant_remote.contains_key(&id) {
                    self.bring_dormant_remote_online(id);
                } else {
                    self.set_active_window_animated(id, &from_edge);
                }
            }
            PluginCommand::SetWindowCycleOrder { ids } => {
                self.window_cycle_order = if ids.is_empty() { None } else { Some(ids) };
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
                self.handle_watch_path(path, recursive, request_id);
            }
            PluginCommand::UnwatchPath { handle } => {
                self.file_watcher_manager.unwatch(handle);
            }

            PluginCommand::PreviewWindowInRect { id } => {
                self.handle_preview_window_in_rect(id);
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
                self.handle_register_status_bar_element(plugin_name, token_name, title);
            }
            PluginCommand::SetStatusBarValue {
                buffer_id,
                key,
                value,
            } => {
                self.handle_set_status_bar_value(buffer_id, key, value);
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
                self.handle_open_file_in_background_routed(path, window_id);
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
                self.handle_cancel_animation(id);
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

            PluginCommand::AttachRemoteAgent {
                payload,
                request_id,
            } => {
                self.handle_attach_remote_agent(payload, request_id);
            }

            PluginCommand::CancelRemoteAttach => {
                self.cancel_remote_attaches();
            }

            PluginCommand::ClearAuthority => {
                self.handle_clear_authority();
            }

            PluginCommand::SetEnv { snippet, dir } => {
                self.handle_set_env(snippet, dir);
            }

            PluginCommand::ClearEnv => {
                self.handle_clear_env();
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

            PluginCommand::HttpFetch {
                url,
                target_path,
                callback_id,
            } => {
                self.handle_http_fetch(url, target_path, callback_id);
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
                initial_cursor_line,
                indentation_guide,
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
                    initial_cursor_line,
                    indentation_guide,
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
                scrollable,
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
                    scrollable,
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
                initial_cursor_line,
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
                    initial_cursor_line,
                    request_id,
                );
            }

            // ==================== Context Commands ====================
            PluginCommand::SetContext { name, active } => {
                self.handle_set_context(name, active);
            }

            // ==================== Review Diff Commands ====================
            PluginCommand::SetReviewDiffHunks { hunks } => {
                self.handle_set_review_diff_hunks(hunks);
            }

            // ==================== Vi Mode Commands ====================
            PluginCommand::ExecuteAction { action_name } => {
                self.handle_execute_action(action_name);
            }
            PluginCommand::ExecuteActions { actions } => {
                self.handle_execute_actions(actions);
            }
            PluginCommand::DefineMacro { register, steps } => {
                self.handle_define_macro(register, steps);
            }
            PluginCommand::PlayMacroByRegister { register } => {
                if let Some(key) = register.chars().next() {
                    self.play_macro(key);
                }
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
            PluginCommand::GetCompositeCursorInfo { request_id } => {
                self.handle_get_composite_cursor_info(request_id);
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
                buffer_id,
            } => {
                self.handle_show_action_popup(popup_id, title, message, actions, buffer_id);
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

            PluginCommand::RegisterLspUriScheme { scheme } => {
                tracing::debug!("Plugin registered LSP URI scheme: {}", scheme);
                self.lsp_uri_schemes.insert(scheme);
            }

            PluginCommand::MarkBufferReadOnly { path } => {
                self.handle_mark_buffer_read_only(path);
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
                    crate::app::composite_buffer_actions::CreateCompositeBufferArgs {
                        name,
                        mode,
                        layout_config: layout,
                        source_configs: sources,
                        hunks,
                        initial_focus_hunk,
                        request_id,
                    },
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
                self.handle_composite_next_hunk(buffer_id);
            }
            PluginCommand::CompositePrevHunk { buffer_id } => {
                self.handle_composite_prev_hunk(buffer_id);
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
                source_buffer_id,
                handle_id,
            } => {
                self.handle_begin_search(crate::app::plugin_commands::BeginSearchArgs {
                    pattern,
                    fixed_string,
                    case_sensitive,
                    max_results,
                    whole_words,
                    source_buffer_id,
                    handle_id,
                });
            }

            PluginCommand::ReplaceInBuffer {
                file_path,
                buffer_id,
                matches,
                replacement,
                callback_id,
            } => {
                self.handle_replace_in_buffer(
                    file_path,
                    buffer_id,
                    matches,
                    replacement,
                    callback_id,
                );
            }

            PluginCommand::MountWidgetPanel {
                plugin,
                panel_id,
                buffer_id,
                spec,
            } => {
                let key = crate::widgets::PanelKey::new(plugin, panel_id);
                self.handle_mount_widget_panel(key, buffer_id, spec);
            }

            PluginCommand::UpdateWidgetPanel {
                plugin,
                panel_id,
                spec,
            } => {
                let key = crate::widgets::PanelKey::new(plugin, panel_id);
                self.handle_update_widget_panel(&key, spec);
            }

            PluginCommand::UnmountWidgetPanel { plugin, panel_id } => {
                let key = crate::widgets::PanelKey::new(plugin, panel_id);
                self.handle_unmount_widget_panel(&key);
            }

            PluginCommand::WidgetCommand {
                plugin,
                panel_id,
                action,
            } => {
                let key = crate::widgets::PanelKey::new(plugin, panel_id);
                self.handle_widget_command(&key, action);
            }

            PluginCommand::WidgetMutate {
                plugin,
                panel_id,
                mutation,
            } => {
                let key = crate::widgets::PanelKey::new(plugin, panel_id);
                self.handle_widget_mutate(&key, mutation);
            }

            PluginCommand::MountFloatingWidget {
                plugin,
                panel_id,
                spec,
                width_pct,
                height_pct,
                as_dock,
                focus_marker,
            } => {
                let key = crate::widgets::PanelKey::new(plugin, panel_id);
                self.handle_mount_floating_widget(
                    key,
                    spec,
                    width_pct,
                    height_pct,
                    as_dock,
                    focus_marker,
                );
            }

            PluginCommand::UpdateFloatingWidget {
                plugin,
                panel_id,
                spec,
            } => {
                let key = crate::widgets::PanelKey::new(plugin, panel_id);
                self.handle_update_floating_widget(&key, spec);
            }

            PluginCommand::UnmountFloatingWidget { plugin, panel_id } => {
                let key = crate::widgets::PanelKey::new(plugin, panel_id);
                self.handle_unmount_floating_widget(&key);
            }

            PluginCommand::FloatingPanelControl {
                plugin,
                panel_id,
                op,
                arg,
            } => {
                let key = crate::widgets::PanelKey::new(plugin, panel_id);
                self.handle_floating_panel_control(&key, &op, arg);
            }
        }
        Ok(())
    }

    // ── Delegated handlers extracted from the dispatch match ─────────────

    fn handle_watch_path(&mut self, path: std::path::PathBuf, recursive: bool, request_id: u64) {
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

    fn handle_set_env(&mut self, snippet: String, dir: Option<String>) {
        // Activation runs repo-controlled code, so it's only honored in
        // a Trusted workspace — defense in depth even though the plugin
        // already gates on `workspaceTrustLevel()`.
        use crate::services::workspace_trust::TrustLevel;
        if self.authority().workspace_trust.level() == TrustLevel::Trusted {
            self.authority()
                .env_provider
                .set(snippet, dir.map(std::path::PathBuf::from));
            // Re-evaluate already-running tooling under the new env — scoped to
            // THIS window only.
            self.refresh_active_window_lsp_for_env();
        } else {
            self.active_window_mut().status_message =
                Some("Workspace not trusted — cannot activate environment".to_string());
        }
    }

    fn handle_clear_env(&mut self) {
        let was_active = self.authority().env_provider.is_active();
        self.authority().env_provider.clear();
        if was_active {
            self.refresh_active_window_lsp_for_env();
        }
    }

    /// Re-launch the *active window's* already-running language servers so they
    /// pick up a just-changed environment (`editor.setEnv` / `clearEnv`),
    /// WITHOUT a process-wide editor rebuild.
    ///
    /// The window's `EnvProvider` is updated in place (see
    /// `services::env_provider` — "the plugin sets the recipe in place […] and
    /// there is no authority rebuild"), so every *new* spawn — new terminals,
    /// servers started later — already inherits the change. Only servers that
    /// were spawned under the old env need a re-launch, and only this window's:
    /// each `Window` owns its own LSP, terminals, and authority, so a single
    /// workspace's env activation must not touch the others. The previous
    /// `request_restart` here rebuilt the whole editor — tearing down every
    /// other orchestrator session's terminals and closing the dock — which is
    /// the "Trust restarted everything" bug. Already-open terminals keep
    /// running and only adopt the new env when reopened (matching the env
    /// subsystem's lazy, capture-at-spawn model).
    fn refresh_active_window_lsp_for_env(&mut self) {
        let active_id = self.active_window;
        let running: Vec<String> = self
            .windows
            .get(&active_id)
            .map(|w| w.lsp.running_servers())
            .unwrap_or_default();
        if running.is_empty() {
            return;
        }
        let file_path = self
            .active_window()
            .buffer_metadata
            .get(&self.active_buffer())
            .and_then(|meta| meta.file_path().cloned());
        for language in &running {
            if let Some(w) = self.windows.get_mut(&active_id) {
                let _ = w.lsp.manual_restart(language, file_path.as_deref());
            }
            // Re-send didOpen for this language's buffers so the fresh server
            // (now under the new env) sees the open documents.
            self.reopen_buffers_for_language(language);
        }
    }

    fn handle_open_file_in_background_routed(
        &mut self,
        path: std::path::PathBuf,
        window_id: Option<fresh_core::WindowId>,
    ) {
        let route_to_inactive =
            window_id.filter(|&id| id != self.active_window && self.windows.contains_key(&id));
        if let Some(target) = route_to_inactive {
            self.handle_open_file_in_inactive_session(target, path);
        } else {
            self.handle_open_file_in_background(path);
        }
    }

    // ── Handlers extracted from the dispatch match ───────────────────────

    fn handle_set_split_label(&mut self, split_id: SplitId, label: String) {
        self.windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .set_label(LeafId(split_id), label);
    }

    fn handle_clear_split_label(&mut self, split_id: SplitId) {
        self.windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .clear_label(split_id);
    }

    fn handle_reload_themes(&mut self, apply_theme: Option<String>) {
        self.reload_themes();
        if let Some(theme_name) = apply_theme {
            self.apply_theme(&theme_name);
        }
    }

    fn handle_set_key_capture_active(&mut self, active: bool) {
        self.active_window_mut().key_capture_active = active;
        if !active {
            // Capture window closed; any leftover queued keys were intended
            // for the plugin and should not leak into normal dispatch.
            self.active_window_mut().pending_key_capture_buffer.clear();
        }
    }

    fn handle_set_prompt_input_sync(&mut self, sync: bool) {
        if let Some(prompt) = &mut self.active_window_mut().prompt {
            prompt.sync_input_on_navigate = sync;
        }
    }

    fn handle_set_prompt_title(&mut self, title: Vec<fresh_core::api::StyledText>) {
        if let Some(prompt) = &mut self.active_window_mut().prompt {
            prompt.title = title;
        }
    }

    fn handle_set_prompt_footer(&mut self, footer: Vec<fresh_core::api::StyledText>) {
        if let Some(prompt) = &mut self.active_window_mut().prompt {
            prompt.footer = footer;
        }
    }

    fn handle_set_prompt_toolbar(&mut self, spec: Option<fresh_core::api::WidgetSpec>) {
        if let Some(prompt) = &mut self.active_window_mut().prompt {
            prompt.toolbar_widget = spec;
        }
    }

    fn handle_set_prompt_status(&mut self, status: String) {
        if let Some(prompt) = &mut self.active_window_mut().prompt {
            prompt.status = status;
        }
    }

    fn handle_set_prompt_selected_index(&mut self, index: u32) {
        if let Some(prompt) = &mut self.active_window_mut().prompt {
            let len = prompt.suggestions.len();
            if len > 0 {
                prompt.selected_suggestion = Some((index as usize).min(len - 1));
            }
        }
    }

    fn handle_create_window(&mut self, root: std::path::PathBuf, label: String) {
        if !root.is_absolute() {
            tracing::warn!(
                "CreateWindow rejected: root must be absolute, got {:?}",
                root
            );
        } else {
            let _ = self.create_window_at(root, label);
        }
    }

    fn handle_preview_window_in_rect(&mut self, id: Option<fresh_core::WindowId>) {
        // Only honour if the session exists and is not the active one
        // (no point previewing the session whose UI is already on screen).
        self.preview_window_id = match id {
            Some(sid) if sid != self.active_window && self.windows.contains_key(&sid) => Some(sid),
            _ => None,
        };
    }

    fn handle_register_status_bar_element(
        &mut self,
        plugin_name: String,
        token_name: String,
        title: String,
    ) {
        if let Err(e) = self.register_status_bar_element(&plugin_name, &token_name, &title) {
            tracing::warn!("Failed to register statusbar element: {}", e);
        }
    }

    fn handle_set_status_bar_value(&mut self, buffer_id: u64, key: String, value: String) {
        if let Err(e) =
            self.set_status_bar_value(fresh_core::BufferId(buffer_id as usize), &key, value)
        {
            // Plugins compute asynchronously off a lagging state snapshot, so
            // the target buffer may have closed — an expected, benign race.
            tracing::debug!("Skipped statusbar value for stale buffer: {}", e);
        }
    }

    fn handle_cancel_animation(&mut self, id: u64) {
        self.active_window_mut()
            .animations
            .cancel(crate::view::animation::AnimationId::from_raw(id));
    }

    fn handle_clear_authority(&mut self) {
        tracing::info!("Plugin cleared authority; restoring local");
        self.clear_authority();
    }

    fn handle_set_review_diff_hunks(&mut self, hunks: Vec<fresh_core::api::ReviewHunk>) {
        self.active_window_mut().review_hunks = hunks;
        tracing::debug!(
            "Set {} review hunks",
            self.active_window_mut().review_hunks.len()
        );
    }

    fn handle_composite_next_hunk(&mut self, buffer_id: fresh_core::BufferId) {
        // Inner group leaf: the Review Diff composite lives in the focused
        // group leaf, not the outer active split (see `handle_composite_action`).
        let split_id = self.active_window().effective_active_pair().0;
        self.active_window_mut()
            .composite_next_hunk(split_id, buffer_id);
    }

    fn handle_composite_prev_hunk(&mut self, buffer_id: fresh_core::BufferId) {
        let split_id = self.active_window().effective_active_pair().0;
        self.active_window_mut()
            .composite_prev_hunk(split_id, buffer_id);
    }

    // ── Virtual-buffer display configuration ────────────────────────────

    /// Apply the three display flags (line numbers, cursor visibility,
    /// editing lock) that every `create_virtual_buffer_*` command sets
    /// on the newly-created buffer's state.
    fn configure_vbuf_display(
        &mut self,
        buffer_id: crate::model::event::BufferId,
        show_line_numbers: bool,
        show_cursors: bool,
        editing_disabled: bool,
        indentation_guide: Option<bool>,
        scrollable: bool,
    ) {
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
            // `None` leaves the default (virtual buffers get no guides); a
            // plugin that shows real source in a virtual buffer (e.g. git log's
            // file-at-commit view) passes `Some(true)` to keep them.
            if let Some(enabled) = indentation_guide {
                state.indentation_guide_override = Some(enabled);
            }
            // Self-managing widget panels pass `scrollable=false` so no
            // buffer scrollbar is drawn and the viewport can't be dragged
            // off the panel's own content (issue #2434 follow-up).
            state.scrollable = scrollable;
        }
    }

    // ── Virtual-buffer-in-split sub-paths ───────────────────────────────

    /// Utility-dock fast path: a leaf with `dock_leaf`'s role already exists,
    /// so attach the new virtual buffer there instead of spawning a new split.
    #[allow(clippy::too_many_arguments)]
    fn route_vbuf_to_existing_dock(
        &mut self,
        dock_leaf: crate::model::event::LeafId,
        name: String,
        mode: String,
        read_only: bool,
        entries: Vec<fresh_core::text_property::TextPropertyEntry>,
        panel_id: Option<&str>,
        show_line_numbers: bool,
        show_cursors: bool,
        editing_disabled: bool,
        scrollable: bool,
        request_id: Option<u64>,
    ) {
        // Capture the source split *before* create_virtual_buffer tabs the
        // new buffer into it; we drop that phantom tab after the dock attach.
        let source_split_before_create = self.split_manager().active_split();
        let buffer_id =
            self.active_window_mut()
                .create_virtual_buffer(name.clone(), mode, read_only);
        self.configure_vbuf_display(
            buffer_id,
            show_line_numbers,
            show_cursors,
            editing_disabled,
            None,
            scrollable,
        );
        if let Some(pid) = panel_id {
            self.panel_ids_mut().insert(pid.to_string(), buffer_id);
        }
        if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries) {
            tracing::error!("Failed to set virtual buffer content (dock route): {}", e);
            return;
        }
        // Swap the dock leaf's active buffer to the new one and add it as a tab.
        self.split_manager_mut().set_active_split(dock_leaf);
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
    }

    /// Idempotent panel update: `panel_name` already maps to a live buffer,
    /// so just refresh its content and focus the split it lives in.
    fn update_existing_vbuf_panel(
        &mut self,
        existing_buffer_id: crate::model::event::BufferId,
        entries: Vec<fresh_core::text_property::TextPropertyEntry>,
        request_id: Option<u64>,
        panel_name: &str,
    ) {
        match self.set_virtual_buffer_content(existing_buffer_id, entries) {
            Ok(()) => tracing::info!("Updated existing panel '{}' content", panel_name),
            Err(e) => tracing::error!("Failed to update panel content: {}", e),
        }
        let splits = self.split_manager().splits_for_buffer(existing_buffer_id);
        if let Some(&split_id) = splits.first() {
            self.split_manager_mut().set_active_split(split_id);
            // Route through set_pane_buffer so tree + SVS stay consistent.
            self.active_window_mut()
                .set_pane_buffer(split_id, existing_buffer_id);
            tracing::debug!("Focused split {:?} containing panel buffer", split_id);
        }
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
    }

    // ── Line-position shared implementation ─────────────────────────────

    /// Shared implementation for `handle_get_line_start_position` and
    /// `handle_get_line_end_position`. When `want_end` is false the byte
    /// offset of the line's first character is returned; when true, the
    /// byte offset of its terminating newline (or `buffer_len` for the
    /// last line without a trailing newline).
    fn handle_get_line_position(
        &mut self,
        buffer_id: crate::model::event::BufferId,
        line: u32,
        request_id: u64,
        want_end: bool,
    ) {
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
                buffer_line_byte_offset(&content, len, line as usize, want_end)
            });
        self.resolve_json_callback(request_id, result);
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
        let load_result = self.plugin_manager.read().unwrap().load_plugin(&path);
        match load_result {
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
                if let Ok(mut schemas) = self.plugin_schemas.write() {
                    schemas.remove(&name);
                }
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
        // Capture the plugin's path before reloading so we can refresh its
        // schema sidecar too. `list_plugins` is cheap (one channel
        // round-trip).
        let path = self
            .plugin_manager
            .read()
            .unwrap()
            .list_plugins()
            .into_iter()
            .find(|p| p.name == name)
            .map(|p| p.path);
        let _ = path; // schema is now re-registered by plugin code on reload
        let reload_result = self.plugin_manager.read().unwrap().reload_plugin(&name);
        match reload_result {
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

        // Plugins may *request* the trust prompt (`workspace_trust_prompt`,
        // which asks the user) but must never *set* the trust level
        // themselves. Granting/lowering trust is a user+core decision — the
        // same boundary VS Code, JetBrains, and Zed enforce: an extension can
        // open the prompt, the user decides. Silently drop any attempt to
        // dispatch the level-setting actions through this generic channel.
        const PLUGIN_FORBIDDEN_ACTIONS: &[&str] = &[
            "workspace_trust_trust",
            "workspace_trust_restrict",
            "workspace_trust_block",
        ];

        for action_spec in actions {
            if PLUGIN_FORBIDDEN_ACTIONS.contains(&action_spec.action.as_str()) {
                tracing::warn!(
                    "plugin attempted to set workspace trust via '{}' — denied; \
                     plugins may request the prompt (workspace_trust_prompt), not set the level",
                    action_spec.action
                );
                continue;
            }
            if let Some(action) = Action::from_str(&action_spec.action, &action_spec.args) {
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

    /// Define (or replace) an in-memory macro under `register` from a step
    /// list supplied by a plugin (`editor.defineMacro`). Each step is parsed
    /// through `Action::from_str` (with its args), so payload actions like
    /// `insert_char` reconstruct faithfully. Unknown action names are skipped
    /// with a warning rather than aborting, so one typo in a hand-edited
    /// `init.ts` macro doesn't discard the whole register.
    fn handle_define_macro(&mut self, register: String, steps: Vec<fresh_core::api::ActionSpec>) {
        use crate::input::keybindings::Action;

        let Some(key) = register.chars().next() else {
            tracing::warn!("defineMacro: empty register key, ignoring");
            return;
        };

        let mut actions = Vec::with_capacity(steps.len());
        for spec in &steps {
            match Action::from_str(&spec.action, &spec.args) {
                Some(action) => {
                    for _ in 0..spec.count.max(1) {
                        actions.push(action.clone());
                    }
                }
                None => {
                    tracing::warn!(
                        "defineMacro['{}']: unknown action '{}' skipped",
                        key,
                        spec.action
                    );
                }
            }
        }

        let count = actions.len();
        self.active_window_mut().macros.define(key, actions);
        tracing::debug!("defineMacro['{}']: stored {} action(s)", key, count);
    }

    /// Get text from a buffer range (for vi mode yank operations).
    ///
    /// See [`clamp_buffer_text_range`] for why the requested range is
    /// clamped rather than rejected.
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
            // Plugins derive `end` from a snapshot length (see
            // `get_buffer_length`) that lags the live buffer, so when the
            // buffer shrinks between the length read and this fetch — e.g.
            // concurrent edits from the editor and an external process
            // rewriting the file on disk — the requested end can briefly
            // exceed the live length. Clamp to the current bounds and return
            // what's there; the plugin recomputes on the next change event.
            let (start, end) = clamp_buffer_text_range(start, end, state.buffer.len());
            Ok(state.get_text_range(start, end))
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

    /// Get the byte offset of the start of a line in the active buffer.
    fn handle_get_line_start_position(&mut self, buffer_id: BufferId, line: u32, request_id: u64) {
        self.handle_get_line_position(buffer_id, line, request_id, false);
    }

    /// Get the byte offset of the end of a line (position of its terminating newline,
    /// or `buffer_len` for the last line without a trailing newline).
    fn handle_get_line_end_position(&mut self, buffer_id: BufferId, line: u32, request_id: u64) {
        self.handle_get_line_position(buffer_id, line, request_id, true);
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
            let newlines = content.bytes().filter(|&b| b == b'\n').count();
            Some(if content.is_empty() {
                1
            } else {
                newlines + usize::from(!content.ends_with('\n'))
            })
        } else {
            None
        };

        self.resolve_json_callback(request_id, result);
    }

    /// Resolve cursor info for the active composite (side-by-side diff)
    /// buffer. Returns `null` to the plugin when the active buffer isn't a
    /// composite buffer; otherwise an object with the focused pane index,
    /// pane count, and the 0-indexed source line shown in each pane on the
    /// cursor's aligned row (`null` per-pane where that side is blank).
    fn handle_get_composite_cursor_info(&mut self, request_id: u64) {
        let info = self.active_window().active_composite_cursor_info();
        let value = info.map(|(focused_pane, pane_count, lines)| {
            serde_json::json!({
                "focusedPane": focused_pane,
                "paneCount": pane_count,
                "lines": lines,
            })
        });
        self.resolve_json_callback(request_id, value);
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
        if !self.authority().filesystem.exists(&path) {
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

        let new_size = match self.authority().filesystem.metadata(&path) {
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

            // Workspace Trust gates host spawns too. `spawnHostProcess`
            // deliberately bypasses the authority spawner, so the choke-point
            // guard never sees it — enforce the level here directly. Blocked
            // fails every host spawn; Restricted refuses repo-local
            // executables. Without this, Blocked wouldn't actually block
            // everything.
            if let crate::services::workspace_trust::SpawnDecision::Deny(reason) = self
                .authority()
                .workspace_trust
                .decide(&command, effective_cwd.as_deref())
            {
                #[allow(clippy::let_underscore_must_use)]
                let _ = sender.send(AsyncMessage::PluginProcessOutput {
                    process_id,
                    stdout: String::new(),
                    stderr: reason,
                    exit_code: -1,
                });
                return;
            }

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

    #[allow(clippy::too_many_arguments)]
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
        initial_cursor_line: Option<u32>,
        indentation_guide: Option<bool>,
        request_id: Option<u64>,
    ) {
        // Hidden-from-tabs buffers (e.g. composite source panes) must NOT be
        // attached to the active split or made the active buffer: doing so
        // pollutes the main tab bar and, when they're later closed, leaves
        // auto-created "[No Name]" tabs behind. Create them detached.
        let buffer_id = if hidden_from_tabs {
            self.active_window_mut().create_virtual_buffer_detached(
                name.clone(),
                mode.clone(),
                read_only,
            )
        } else {
            self.active_window_mut()
                .create_virtual_buffer(name.clone(), mode.clone(), read_only)
        };
        tracing::info!(
            "Created virtual buffer '{}' with mode '{}' (id={:?}, detached={})",
            name,
            mode,
            buffer_id,
            hidden_from_tabs
        );

        // TODO: show_line_numbers is duplicated between EditorState.margins and
        // BufferViewState. The renderer reads BufferViewState and overwrites
        // margins each frame via configure_for_line_numbers(), making the margin
        // setting here effectively write-only. Consider removing the margin call
        // and only setting BufferViewState.show_line_numbers.
        self.configure_vbuf_display(
            buffer_id,
            show_line_numbers,
            show_cursors,
            editing_disabled,
            indentation_guide,
            true,
        );
        if !hidden_from_tabs {
            let active_split = self.split_manager().active_split();
            if let Some(view_state) = self
                .windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_view_states_mut())
                .expect("active window must have a populated split layout")
                .get_mut(&active_split)
            {
                view_state.ensure_buffer_state(buffer_id).show_line_numbers = show_line_numbers;
            }
        } else if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(&buffer_id) {
            meta.hidden_from_tabs = true;
        }

        // Now set the content
        match self.set_virtual_buffer_content(buffer_id, entries) {
            Ok(()) => {
                tracing::debug!("Set virtual buffer content for {:?}", buffer_id);
                // Switch to the new buffer to display it — but only when it's
                // attached (a detached hidden buffer must not steal the view).
                if !hidden_from_tabs {
                    self.set_active_buffer(buffer_id);
                    tracing::debug!("Switched to virtual buffer {:?}", buffer_id);
                }

                // Apply an initial cursor position. We do this in the same
                // command-processing tick as creation, so the cursor is in
                // place by the time the editor next processes user input —
                // a follow-up SetBufferCursor from the plugin would race
                // against the user. The plugin passes a line index; we
                // resolve it to a byte here using the buffer's own content
                // so UTF-8-byte math never touches JS-side string-length
                // semantics. Done AFTER `set_active_buffer` so the new
                // buffer is actually mounted in a split when
                // `set_buffer_cursor_in_splits` walks the split tree.
                if let Some(line) = initial_cursor_line {
                    let target_line = line as usize;
                    let byte = self
                        .windows
                        .get_mut(&self.active_window)
                        .and_then(|w| w.buffers.get_mut(&buffer_id))
                        .map(|s| {
                            let total = s.buffer.len();
                            let mut iter = s.buffer.line_iterator(0, 80);
                            let mut target_byte = 0;
                            for current_line in 0..=target_line {
                                if let Some((line_start, _)) = iter.next_line() {
                                    if current_line == target_line {
                                        target_byte = line_start;
                                        break;
                                    }
                                } else {
                                    target_byte = total;
                                    break;
                                }
                            }
                            target_byte
                        })
                        .unwrap_or(0);
                    let splits: Vec<super::LeafId> = self
                        .windows
                        .get(&self.active_window)
                        .and_then(|w| w.buffers.splits())
                        .map(|(mgr, _)| mgr)
                        .expect("active window must have a populated split layout")
                        .splits_for_buffer(buffer_id);
                    self.active_window_mut()
                        .set_buffer_cursor_in_splits(buffer_id, byte, &splits);
                }

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

    #[allow(clippy::too_many_arguments)]
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
        scrollable: Option<bool>,
        request_id: Option<u64>,
    ) {
        // Buffers are user-scrollable unless the plugin opts out (widget
        // panels that own their own scroll window).
        let scrollable = scrollable.unwrap_or(true);
        // Resolve the role string. Unknown roles are silently dropped
        // (forward-compat for plugins targeting newer cores).
        let split_role: Option<crate::view::split::SplitRole> = match role.as_deref() {
            Some("utility_dock") => Some(crate::view::split::SplitRole::UtilityDock),
            _ => None,
        };

        // Path 1 — Utility-dock fast path (issue #1796 / Section 2 of the design):
        // if a leaf with this role already exists, attach the new buffer there
        // instead of spawning a fresh split.
        if let Some(dock_leaf) = split_role.and_then(|r| self.split_manager().find_leaf_by_role(r))
        {
            return self.route_vbuf_to_existing_dock(
                dock_leaf,
                name,
                mode,
                read_only,
                entries,
                panel_id.as_deref(),
                show_line_numbers,
                show_cursors,
                editing_disabled,
                scrollable,
                request_id,
            );
            // No dock yet — fall through to normal split creation,
            // then tag the new leaf with the requested role at the end.
        }

        // Path 2 — Idempotent panel update: if this panel_id already maps to a
        // live buffer, refresh its content and re-focus it.
        if let Some(pid) = panel_id.as_deref() {
            let maybe_existing = self.panel_ids().get(pid).copied();
            if let Some(existing_id) = maybe_existing {
                let buffer_alive = self
                    .windows
                    .get(&self.active_window)
                    .map(|w| w.buffers.contains_key(&existing_id))
                    .unwrap_or(false);
                if buffer_alive {
                    return self.update_existing_vbuf_panel(existing_id, entries, request_id, pid);
                }
                // Buffer no longer exists — remove the stale entry and fall through.
                tracing::warn!(
                    "Removing stale panel_id '{}' pointing to non-existent buffer {:?}",
                    pid,
                    existing_id
                );
                self.panel_ids_mut().remove(pid);
            }
        }

        // Path 3 — Fresh split creation.
        //
        // Capture the source split before creating the buffer —
        // `create_virtual_buffer` unconditionally adds the new buffer as a tab
        // to the currently active split, which is wrong for a panel that lives
        // in its own dedicated split (it would appear in BOTH splits — bug #3).
        let source_split_before_create = self.split_manager().active_split();

        let buffer_id =
            self.active_window_mut()
                .create_virtual_buffer(name.clone(), mode.clone(), read_only);
        tracing::info!(
            "Created virtual buffer '{}' with mode '{}' in split (id={:?})",
            name,
            mode,
            buffer_id
        );

        self.configure_vbuf_display(
            buffer_id,
            show_line_numbers,
            show_cursors,
            editing_disabled,
            None,
            scrollable,
        );

        if let Some(pid) = panel_id {
            self.panel_ids_mut().insert(pid, buffer_id);
        }

        if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries) {
            tracing::error!("Failed to set virtual buffer content: {}", e);
            return;
        }

        let split_dir = match direction.as_deref() {
            Some("vertical") => crate::model::event::SplitDirection::Vertical,
            _ => crate::model::event::SplitDirection::Horizontal,
        };

        // When the caller requested `role = "utility_dock"` but no dock leaf
        // existed yet (we fell through the fast path above), split at the
        // *root* so the dock spans the full width — splitting the active leaf
        // would nest it under whichever pane was focused.
        let split_result = if split_role == Some(crate::view::split::SplitRole::UtilityDock) {
            self.split_manager_mut()
                .split_root_positioned(split_dir, buffer_id, ratio, before)
        } else {
            self.split_manager_mut()
                .split_active_positioned(split_dir, buffer_id, ratio, before)
        };

        let created_split_id = match split_result {
            Ok(new_split_id) => {
                // The buffer now lives in its own split — drop its phantom tab
                // from the source split (bug #3). Only when the splits differ;
                // otherwise we'd leave the buffer with no display.
                if new_split_id != source_split_before_create {
                    if let Some(src_vs) = self
                        .windows
                        .get_mut(&self.active_window)
                        .and_then(|w| w.split_view_states_mut())
                        .expect("active window must have a populated split layout")
                        .get_mut(&source_split_before_create)
                    {
                        src_vs.remove_buffer(buffer_id);
                    }
                }

                let mut view_state = SplitViewState::with_buffer(
                    self.terminal_width,
                    self.terminal_height,
                    buffer_id,
                );
                view_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
                    line_numbers: self.config.editor.line_numbers,
                    highlight_current_line: self.config.editor.highlight_current_line,
                    line_wrap: line_wrap.unwrap_or_else(|| {
                        self.active_window().resolve_line_wrap_for_buffer(buffer_id)
                    }),
                    wrap_indent: self.config.editor.wrap_indent,
                    wrap_column: self
                        .active_window()
                        .resolve_wrap_column_for_buffer(buffer_id),
                    rulers: self.config.editor.rulers.clone(),
                    scroll_offset: self.config.editor.scroll_offset,
                });
                view_state.ensure_buffer_state(buffer_id).show_line_numbers = show_line_numbers;
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .insert(new_split_id, view_state);

                self.split_manager_mut().set_active_split(new_split_id);

                // Tag the new leaf with the requested role so the next
                // utility-dock open lands here. Clear any stale role first
                // to maintain the one-leaf-per-role invariant.
                if let Some(target_role) = split_role {
                    self.split_manager_mut().clear_role(target_role);
                    self.split_manager_mut()
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
                self.set_active_buffer(buffer_id);
                None
            }
        };

        if let Some(req_id) = request_id {
            tracing::trace!(
                "CreateVirtualBufferInSplit: resolving callback for request_id={}, \
                 buffer_id={:?}, split_id={:?}",
                req_id,
                buffer_id,
                created_split_id
            );
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

    #[allow(clippy::too_many_arguments)]
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
        initial_cursor_line: Option<u32>,
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

        self.configure_vbuf_display(
            buffer_id,
            show_line_numbers,
            show_cursors,
            editing_disabled,
            None,
            true,
        );

        if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries) {
            tracing::error!("Failed to set virtual buffer content: {}", e);
            return;
        }

        // Apply an initial cursor position before the buffer becomes the
        // active buffer in the split. Same rationale as the equivalent
        // path in `handle_create_virtual_buffer_with_content`: a follow-up
        // SetBufferCursor from the plugin would race against user input.
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

        // Apply an initial cursor position after the buffer is mounted in
        // the target split. Done in the same command-processing tick as
        // creation so the cursor is in place before the editor next
        // processes user input — a follow-up SetBufferCursor from the
        // plugin would race against the user. The plugin passes a line
        // index; we resolve it to a byte here using the buffer's content
        // so UTF-8-byte math never touches JS-side string-length
        // semantics.
        if let Some(line) = initial_cursor_line {
            let target_line = line as usize;
            let byte = self
                .windows
                .get_mut(&self.active_window)
                .and_then(|w| w.buffers.get_mut(&buffer_id))
                .map(|s| {
                    let total = s.buffer.len();
                    let mut iter = s.buffer.line_iterator(0, 80);
                    let mut target_byte = 0;
                    for current_line in 0..=target_line {
                        if let Some((line_start, _)) = iter.next_line() {
                            if current_line == target_line {
                                target_byte = line_start;
                                break;
                            }
                        } else {
                            target_byte = total;
                            break;
                        }
                    }
                    target_byte
                })
                .unwrap_or(0);
            // `splits_for_buffer` only walks the main split tree, so a
            // buffer mounted into an inner leaf of a grouped subtree
            // (buffer-group panel) wouldn't be found and the cursor move
            // would silently no-op. Mirror `handle_set_buffer_cursor` and
            // include any matching inner leaves so the cursor lands
            // regardless of where the buffer ended up.
            let mut splits: Vec<LeafId> = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(mgr, _)| mgr)
                .expect("active window must have a populated split layout")
                .splits_for_buffer(buffer_id);
            for node in self.active_window().grouped_subtrees.values() {
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
                            if vs.active_buffer == buffer_id && !splits.contains(&inner_leaf) {
                                splits.push(inner_leaf);
                            }
                        }
                    }
                }
            }
            self.active_window_mut()
                .set_buffer_cursor_in_splits(buffer_id, byte, &splits);
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
        buffer_id: Option<usize>,
    ) {
        tracing::info!(
            "Action popup requested: id={}, title={}, actions={}, buffer_id={:?}",
            popup_id,
            title,
            actions.len(),
            buffer_id,
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
        let (popup_bg, popup_border_fg) = {
            let theme = self.theme();
            (theme.popup_bg, theme.popup_border_fg)
        };
        let mut popup_obj =
            crate::state::convert_popup_data_to_popup(&popup_data, popup_bg, popup_border_fg);
        popup_obj.resolver = crate::view::popup::PopupResolver::PluginAction {
            popup_id: popup_id.clone(),
        };

        // Buffer-scoped popup: a plugin raised this for one specific buffer
        // (e.g. asm-lsp's `.asm-lsp.toml` offer on `after_file_open`), so it
        // belongs on that buffer's popup stack — it then renders only while
        // that buffer is active and is dropped when the buffer closes,
        // instead of floating over every buffer like a global notification.
        // Fall back to the global stack if the buffer has since closed, so
        // the popup is never silently lost.
        if let Some(bid) = buffer_id {
            let bid = BufferId(bid);
            let stack = self
                .windows
                .values_mut()
                .find_map(|w| w.buffers.get_mut(&bid))
                .map(|state| &mut state.popups);
            if let Some(stack) = stack {
                // Dedup by `popup_id` within this buffer's stack — repeated
                // file-open hooks shouldn't pile up duplicate offers.
                let existing_idx = stack.all().iter().position(|p| {
                    matches!(
                        &p.resolver,
                        crate::view::popup::PopupResolver::PluginAction { popup_id: id } if id == &popup_id,
                    )
                });
                match existing_idx.and_then(|idx| stack.get_mut(idx)) {
                    Some(slot) => *slot = popup_obj,
                    None => stack.show(popup_obj),
                }
                tracing::info!("Action popup shown on buffer {:?}: id={}", bid, popup_id,);
                return;
            }
            tracing::warn!(
                "Action popup id={} requested for missing buffer {:?}; showing globally",
                popup_id,
                bid,
            );
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

    #[allow(clippy::too_many_arguments)]
    fn handle_create_window_with_terminal(
        &mut self,
        root: std::path::PathBuf,
        label: String,
        cwd: Option<String>,
        command: Option<Vec<String>>,
        title: Option<String>,
        resume: Option<Vec<String>>,
        request_id: u64,
    ) {
        let callback_id = JsCallbackId::from(request_id);
        if !root.is_absolute() {
            let msg = format!(
                "createWindowWithTerminal: root must be absolute, got {:?}",
                root
            );
            tracing::warn!("{}", msg);
            self.plugin_manager
                .read()
                .unwrap()
                .reject_callback(callback_id, msg);
            return;
        }
        let cwd_buf = cwd.map(std::path::PathBuf::from);
        // The Orchestrator's "New Session (Local)" flow births its own local
        // backend rather than inheriting the active window's (which may be a
        // container/SSH/k8s session). Remote sessions take the
        // `attachRemoteAgent` → `create_remote_session_window` path, which
        // passes its connected authority. `resume` is the agent-resume argv
        // carried through to the new session's terminal. The new local
        // session gets its own per-session trust scoped to its root.
        let new_authority = self.local_session_authority(&root);
        match self.create_window_with_terminal(
            root,
            label,
            cwd_buf,
            command,
            title,
            new_authority,
            resume,
        ) {
            Ok((window_id, terminal_id, buffer_id)) => {
                let api_result = fresh_core::api::SessionWithTerminalResult {
                    window_id: window_id.0,
                    terminal_id: terminal_id.0 as u64,
                    buffer_id: buffer_id.0 as u64,
                };
                self.plugin_manager.read().unwrap().resolve_callback(
                    callback_id,
                    serde_json::to_string(&api_result).unwrap_or_default(),
                );
            }
            Err(e) => {
                tracing::error!("createWindowWithTerminal failed: {e}");
                self.plugin_manager
                    .read()
                    .unwrap()
                    .reject_callback(callback_id, format!("createWindowWithTerminal: {e}"));
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
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
            target.create_plugin_terminal(crate::app::terminal::PluginTerminalSpec {
                cwd: cwd_buf,
                direction: split_direction,
                ratio,
                focus: focus.unwrap_or(true),
                persistent,
                command,
                title: title.filter(|t| !t.is_empty()),
            })
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
            // The plugin now owns this buffer's cursor visibility; stop
            // the widget runtime from overriding it on every repaint.
            state.cursor_visibility_locked = true;
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
            let spawner = self.authority().process_spawner.clone();

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
                // The new authority shares the editor's live trust + env
                // handles, so its spawners are gated and env'd identically.
                let trust = std::sync::Arc::clone(&self.authority().workspace_trust);
                let env = std::sync::Arc::clone(&self.authority().env_provider);
                // Record the spec on the active session *before* the restart
                // so it persists (save-on-restart) and the rebuilt editor
                // restores this session under the same backend instead of
                // degrading it to local. The payload is cloned because
                // `from_plugin_payload` consumes it.
                let spec = crate::services::authority::SessionAuthoritySpec::Plugin(parsed.clone());
                match crate::services::authority::Authority::from_plugin_payload(parsed, trust, env)
                {
                    Ok(auth) => {
                        tracing::info!("Plugin installed new authority");
                        self.active_window_mut().authority_spec = spec;
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

    /// If the just-activated window is a **dormant remote** session — restored
    /// from disk so its backend spec is remote but its live authority is still
    /// the local placeholder (no live keepalive) — start reconnecting it. SSH /
    /// Kubernetes reconnect from core via [`Self::start_remote_connect`]; a
    /// container (`Plugin`) session needs its owning plugin to re-attach (only
    /// the devcontainer plugin can run `devcontainer up`), left to a follow-up.
    /// Idempotent: a reconnect already in flight for this window is a no-op.
    pub(crate) fn reconnect_dormant_session_if_needed(&mut self, window_id: fresh_core::WindowId) {
        // The *activate* path (diving into a session). A live session already
        // holds its connection (keepalive), so leave it alone here — switching
        // to an already-connected window must not tear down and rebuild it. A
        // local session has nothing to reconnect.
        if self.session_keepalives.contains_key(&window_id) {
            return;
        }
        self.start_remote_reconnect(window_id);
    }

    /// Reconnect a session's remote backend on *explicit user request* — the
    /// remote status indicator's Reconnect / Retry action.
    ///
    /// Unlike [`Self::reconnect_dormant_session_if_needed`] (the activate path,
    /// which no-ops while a keepalive is parked) this forces the connect even
    /// for a *live* remote `Window` that lost its carrier: such a window keeps
    /// its now-stale keepalive and `Window`, so the activate-path guard would
    /// wrongly skip it. On success the `RemoteAttachReady::Reconnect` handler
    /// re-points the window's authority, replaces the stale keepalive, and
    /// respawns its dead embedded terminals (see `async_dispatch.rs`).
    pub(crate) fn force_reconnect_remote_session(&mut self, window_id: fresh_core::WindowId) {
        self.start_remote_reconnect(window_id);
    }

    /// Shared body of the two reconnect entry points: read the window's backend
    /// spec and, for a remote-agent session, kick off the async connect tagged
    /// with this window so its result re-points *this* window.
    fn start_remote_reconnect(&mut self, window_id: fresh_core::WindowId) {
        let Some(spec) = self
            .windows
            .get(&window_id)
            .map(|w| w.authority_spec.clone())
        else {
            return;
        };
        match spec {
            crate::services::authority::SessionAuthoritySpec::Local => {}
            crate::services::authority::SessionAuthoritySpec::RemoteAgent(agent_spec) => {
                // Synthetic, window-derived request id (well clear of the
                // low-numbered JS callback ids) so the in-flight/cancel
                // tracking works and a repeated switch doesn't double-connect.
                let request_id = u64::MAX - window_id.0;
                if self.remote_attach_inflight.contains(&request_id) {
                    return;
                }
                // Clear any prior FailedAttach so the indicator shows
                // "Connecting" (not a stale error) while this retry runs.
                if let Some(w) = self.windows.get_mut(&window_id) {
                    w.remote_reconnect_error = None;
                }
                self.start_remote_connect(agent_spec, Some(window_id), request_id);
            }
            crate::services::authority::SessionAuthoritySpec::Plugin(_) => {
                // Container: only the owning plugin can rebuild the backend
                // (`devcontainer up`). TODO(per-session): fire a
                // `session_reattach_requested` hook so it can.
                tracing::debug!("remote session {window_id}: reattach is plugin-driven (TODO)");
            }
        }
    }

    fn handle_attach_remote_agent(&mut self, payload: serde_json::Value, request_id: u64) {
        // Opaque at the fresh-core boundary; the concrete schema lives in
        // services::authority so core stays backend-agnostic.
        let spec =
            match serde_json::from_value::<crate::services::authority::RemoteAgentSpec>(payload) {
                Ok(spec) => spec,
                Err(e) => {
                    tracing::warn!("attachRemoteAgent: invalid payload: {}", e);
                    self.reject_remote_attach(request_id, format!("invalid attach spec: {e}"));
                    return;
                }
            };
        // A plugin attach: spawn a born-attached window or restart (per
        // `spec.window`), not a reconnect of an existing one.
        self.start_remote_connect(spec, None, request_id);
    }

    /// Spawn the async remote connect (carrier + agent bootstrap) for `spec`
    /// and report the result back via the bridge. Shared by the plugin
    /// `attachRemoteAgent` op and the reconnect-on-activate path:
    /// `reconnect_window = Some(id)` re-points *that dormant window's*
    /// authority on success (no new window / no restart); `None` follows
    /// `spec.window` (born-attached window vs. global restart).
    pub(crate) fn start_remote_connect(
        &mut self,
        spec: crate::services::authority::RemoteAgentSpec,
        reconnect_window: Option<fresh_core::WindowId>,
        request_id: u64,
    ) {
        // Take owned handles up front so the immutable borrows of `self`
        // end before the mutable `set_status_message` / spawn below.
        let runtime = self.tokio_runtime.clone();
        let sender = self.async_bridge.as_ref().map(|b| b.sender());
        let (Some(runtime), Some(sender)) = (runtime, sender) else {
            self.reject_remote_attach(request_id, "async runtime not available".to_string());
            return;
        };

        // Track this connect as in-flight so a plugin can cancel it (the
        // New-Session dialog's Cancel) before it resolves. The cancel sender is
        // handed to the connect via its `select!`; signalling it tears down the
        // in-flight carrier child.
        self.remote_attach_inflight.insert(request_id);
        let (cancel_tx, cancel_rx) = tokio::sync::oneshot::channel::<()>();
        self.remote_attach_cancels.insert(request_id, cancel_tx);

        // Window-mode opts captured before `spec` is consumed — when `window`
        // is set the main loop spawns a born-attached new window instead of
        // restarting the whole editor.
        let window_mode = spec.window;
        let window_label = spec.label.clone();
        let window_command = spec.command.clone();
        // A remote session gets its **own** fresh trust + env handles — never
        // a clone of the launching session's `Arc`s — so a born-attached
        // remote window can't share (and leak) trust/env with the window it
        // was launched from. The trust *level* is copied by value from the
        // launching context (independent handle thereafter); env starts
        // inactive (the remote's env rides the spawner's captured probe).
        let trust = std::sync::Arc::new(crate::services::workspace_trust::WorkspaceTrust::new(
            None,
            self.authority().workspace_trust.level(),
        ));
        let env = std::sync::Arc::new(crate::services::env_provider::EnvProvider::inactive());

        // The connect (spawn the carrier, bootstrap the agent, await `ready`)
        // is async and can take seconds, so run it on the runtime and report
        // back via the bridge instead of blocking the event loop. On success
        // the main loop installs the authority + keepalive (restart or new
        // window); on failure it surfaces the error. Both transports converge
        // on the same `RemoteAttachReady`; only the connect future differs.
        use crate::services::authority::RemoteTransportSpec;
        let base_env = spec.base_env.clone();
        // The reconnect spec persisted on the session (so a restart can bring
        // this remote backend back). Cloned before `spec` is consumed below.
        let session_spec =
            crate::services::authority::SessionAuthoritySpec::RemoteAgent(spec.clone());
        let mode_for = |label: &str| {
            if let Some(window_id) = reconnect_window {
                crate::services::async_bridge::RemoteAttachMode::Reconnect { window_id }
            } else if window_mode {
                crate::services::async_bridge::RemoteAttachMode::Window {
                    label: window_label.clone().unwrap_or_else(|| label.to_string()),
                    command: window_command.clone(),
                }
            } else {
                crate::services::async_bridge::RemoteAttachMode::Restart
            }
        };

        match spec.transport {
            RemoteTransportSpec::KubectlExec { .. } => {
                let (target, base_env) = spec.into_kube_target();
                let label = target.display();
                // Pod-side workspace to re-root at (e.g. `/workspace`).
                let workspace = target.workspace.clone().map(std::path::PathBuf::from);
                let mode = mode_for(&label);
                self.set_status_message(format!("Connecting to {label}…"));
                runtime.spawn(async move {
                    let outcome = crate::services::authority::connect_kube_authority(
                        target,
                        base_env,
                        trust,
                        env,
                        Some(cancel_rx),
                    )
                    .await;
                    let msg = match outcome {
                        Ok((authority, keepalive)) => AsyncMessage::RemoteAttachReady(
                            crate::services::async_bridge::RemoteAttachReady {
                                authority,
                                keepalive: Box::new(keepalive),
                                working_dir: workspace,
                                mode,
                                spec: session_spec,
                                request_id,
                            },
                        ),
                        Err(e) => AsyncMessage::RemoteAttachFailed {
                            error: e.to_string(),
                            request_id,
                            reconnect_window,
                        },
                    };
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = sender.send(msg);
                });
            }
            RemoteTransportSpec::Ssh {
                user,
                host,
                port,
                identity_file,
                remote_path,
                extra_args,
            } => {
                let _ = base_env; // SSH probes its own env on the remote host.
                let params = crate::services::remote::ConnectionParams {
                    user: user.clone().filter(|u| !u.is_empty()),
                    host: host.clone(),
                    port,
                    identity_file: identity_file.map(std::path::PathBuf::from),
                    extra_args,
                };
                // Label: `user@host` when a user was given, else bare `host`.
                let target = params.ssh_target();
                let label = match port {
                    Some(p) => format!("ssh:{target}:{p}"),
                    None => format!("ssh:{target}"),
                };
                let workspace = remote_path.clone().map(std::path::PathBuf::from);
                let mode = mode_for(&label);
                self.set_status_message(format!("Connecting to {label}…"));
                runtime.spawn(async move {
                    let outcome = crate::services::authority::connect_ssh_authority(
                        params,
                        remote_path,
                        trust,
                        env,
                        Some(cancel_rx),
                    )
                    .await;
                    let msg = match outcome {
                        Ok((authority, keepalive)) => AsyncMessage::RemoteAttachReady(
                            crate::services::async_bridge::RemoteAttachReady {
                                authority,
                                keepalive: Box::new(keepalive),
                                working_dir: workspace,
                                mode,
                                spec: session_spec,
                                request_id,
                            },
                        ),
                        Err(e) => AsyncMessage::RemoteAttachFailed {
                            error: e.to_string(),
                            request_id,
                            reconnect_window,
                        },
                    };
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = sender.send(msg);
                });
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

    fn handle_http_fetch(
        &mut self,
        url: String,
        target_path: std::path::PathBuf,
        callback_id: fresh_core::api::JsCallbackId,
    ) {
        if let (Some(runtime), Some(bridge)) = (&self.tokio_runtime, &self.async_bridge) {
            let sender = bridge.sender();
            let process_id = callback_id.as_u64();

            runtime.spawn(async move {
                let fetch = tokio::task::spawn_blocking(move || {
                    crate::services::http::download_to_file(&url, &target_path)
                })
                .await;

                let (stdout, stderr, exit_code) = match fetch {
                    Ok(Ok(status)) => {
                        if (200..300).contains(&status) {
                            (String::new(), String::new(), 0)
                        } else {
                            (String::new(), format!("HTTP {}", status), i32::from(status))
                        }
                    }
                    Ok(Err(e)) => (String::new(), e, -1),
                    Err(e) => (String::new(), format!("fetch task failed: {}", e), -1),
                };

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
        panel_key: crate::widgets::PanelKey,
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
            panel_key.clone(),
            buffer_id,
            spec,
            out.hits,
            out.instance_states,
            out.focus_key,
            out.tabbable,
        );
        // Mark the buffer as hosting an interactive widget panel so the
        // focus/click paths keep routing focus to it even when it opts out
        // of buffer scrolling (a non-scrollable widget panel is still an
        // interactive target, unlike a fixed buffer-group toolbar).
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.buffers.get_mut(&buffer_id))
        {
            state.interactive_widget_panel = true;
        }
        let entries = out.entries;
        if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries.clone()) {
            tracing::error!(
                "Failed to render mounted widget panel {} into {:?}: {}",
                panel_key,
                buffer_id,
                e
            );
        } else {
            tracing::debug!(
                "Mounted widget panel {} into buffer {:?}",
                panel_key,
                buffer_id
            );
        }
        self.apply_widget_focus_cursor(buffer_id, &entries, focus_cursor);
    }

    fn handle_update_widget_panel(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        spec: fresh_core::api::WidgetSpec,
    ) {
        let prev = match self.widget_registry.instance_states(panel_key) {
            Some(s) => s.clone(),
            None => {
                tracing::debug!(
                    "UpdateWidgetPanel for unknown panel {} ignored (not mounted)",
                    panel_key
                );
                return;
            }
        };
        let prev_focus = self
            .widget_registry
            .focus_key(panel_key)
            .map(|s| s.to_string())
            .unwrap_or_default();
        let buffer_id_for_width = self
            .widget_registry
            .buffer_and_spec(panel_key)
            .map(|(b, _)| b)
            .unwrap_or(BufferId(0));
        let panel_width = self.widget_panel_width(buffer_id_for_width);
        let out = crate::widgets::render_spec(&spec, &prev, &prev_focus, panel_width);
        let focus_cursor = out.focus_cursor;
        let entries = out.entries;
        match self.widget_registry.update(
            panel_key,
            spec,
            out.hits,
            out.instance_states,
            out.focus_key,
            out.tabbable,
        ) {
            Ok(buffer_id) => {
                if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries.clone()) {
                    tracing::error!("Failed to render updated widget panel {}: {}", panel_key, e);
                }
                self.apply_widget_focus_cursor(buffer_id, &entries, focus_cursor);
            }
            Err(()) => {
                tracing::debug!(
                    "UpdateWidgetPanel for unknown panel {} ignored (not mounted)",
                    panel_key
                );
            }
        }
    }

    /// Apply a `WidgetMutation` in place, then re-render the panel.
    /// This is the IPC fast path: the plugin doesn't re-transmit
    /// the full spec; it sends one targeted change. The host
    /// mutates the registry's spec / instance state and re-renders
    /// against the just-mutated state.
    fn handle_widget_mutate(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        mutation: fresh_core::api::WidgetMutation,
    ) {
        use fresh_core::api::WidgetMutation;

        // Look up the panel; bail if unknown.
        if self.widget_registry.get(panel_key).is_none() {
            tracing::debug!(
                "WidgetMutate for unknown panel {} ignored (not mounted)",
                panel_key
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
                if let Some(panel) = self.widget_registry.get_mut(panel_key) {
                    // Preserve `scroll` + `multiline` so plugin-
                    // driven SetValue doesn't snap the viewport,
                    // and preserve `completions` /
                    // `completion_selected_index` so the popup
                    // (if open) doesn't disappear on a value
                    // mutation that happens to land while the
                    // user is mid-keystroke.
                    let (scroll, multiline, completions, sel_idx, scroll_off, navigated) =
                        match panel.instance_states.get(&widget_key) {
                            Some(crate::widgets::WidgetInstanceState::Text {
                                editor,
                                scroll,
                                completions,
                                completion_selected_index,
                                completion_scroll_offset,
                                completion_navigated,
                            }) => (
                                *scroll,
                                editor.multiline,
                                completions.clone(),
                                *completion_selected_index,
                                *completion_scroll_offset,
                                *completion_navigated,
                            ),
                            _ => (0u32, true, Vec::new(), 0usize, 0u32, false),
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
                        crate::widgets::WidgetInstanceState::Text {
                            editor,
                            scroll,
                            completions,
                            completion_selected_index: sel_idx,
                            completion_scroll_offset: scroll_off,
                            completion_navigated: navigated,
                        },
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
                if let Some(panel) = self.widget_registry.get_mut(panel_key) {
                    crate::widgets::set_toggle_checked_in_spec(
                        &mut panel.spec,
                        &widget_key,
                        checked,
                    );
                }
            }
            WidgetMutation::SetSelectedIndex { widget_key, index } => {
                // Selected index lives in instance state for both List
                // and Tree widgets — dispatch on the existing variant so
                // a plugin-driven selection move on a Tree (e.g. Search &
                // Replace "next match") actually updates the Tree instead
                // of clobbering it with a List state (which drops the
                // expanded-keys set and never moves the highlight).
                if let Some(panel) = self.widget_registry.get_mut(panel_key) {
                    Self::set_widget_selected_index_state(panel, &widget_key, index);
                }
            }
            WidgetMutation::SetNumber { widget_key, value } => {
                // Number value is host-owned instance state; clamp to
                // the widget's bounds and write it. The trailing
                // rerender repaints. No `change` event — a plugin-driven
                // set is not a user edit (matches SetValue).
                if let Some(panel) = self.widget_registry.get_mut(panel_key) {
                    let (min, max) = match crate::widgets::find_widget_by_key(
                        &panel.spec,
                        &widget_key,
                    ) {
                        Some(fresh_core::api::WidgetSpec::Number { min, max, .. }) => (*min, *max),
                        _ => (None, None),
                    };
                    let clamped = crate::widgets::clamp_number(value, min, max);
                    panel.instance_states.insert(
                        widget_key.clone(),
                        crate::widgets::WidgetInstanceState::Number { value: clamped },
                    );
                }
            }
            WidgetMutation::SetCompletions { widget_key, items } => {
                // Update completion popup state on a Text widget.
                // Non-empty `items` opens the popup and resets the
                // host-managed selection to the top candidate;
                // empty closes it. The instance state has to
                // exist first (a SetCompletions arriving before
                // any render is dropped on the floor — Text
                // instance state is seeded on first render of
                // the spec).
                if let Some(panel) = self.widget_registry.get_mut(panel_key) {
                    if let Some(crate::widgets::WidgetInstanceState::Text {
                        completions,
                        completion_selected_index,
                        completion_scroll_offset,
                        completion_navigated,
                        ..
                    }) = panel.instance_states.get_mut(&widget_key)
                    {
                        *completions = items;
                        *completion_selected_index = 0;
                        *completion_scroll_offset = 0;
                        // A (re)opened popup is not yet "entered": Tab /
                        // Enter act on the form until the user steps in
                        // with ↑/↓. (Closing — empty `items` — also
                        // resets it, harmlessly.)
                        *completion_navigated = false;
                    }
                }
            }
            WidgetMutation::SetItems {
                widget_key,
                items,
                item_keys,
            } => {
                // List items live in the spec.
                if let Some(panel) = self.widget_registry.get_mut(panel_key) {
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
                if let Some(panel) = self.widget_registry.get_mut(panel_key) {
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
                if let Some(panel) = self.widget_registry.get_mut(panel_key) {
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
                if let Some(panel) = self.widget_registry.get_mut(panel_key) {
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
                if let Some(panel) = self.widget_registry.get_mut(panel_key) {
                    crate::widgets::set_raw_entries_in_spec(&mut panel.spec, &widget_key, entries);
                }
            }
            WidgetMutation::SetFocusKey { widget_key } => {
                // Panel-level focus lives in the registry, not the
                // spec. The renderer reads it on the next paint and
                // re-clamps to the first tabbable if the key isn't a
                // current tabbable, so an unknown key is a safe no-op.
                self.widget_registry.set_focus_key(panel_key, widget_key);
            }
        }

        // Re-render with the mutated state. `rerender_widget_panel`
        // reads the registry's current spec + instance state and
        // pushes the result through the buffer.
        self.rerender_widget_panel(panel_key);
    }

    fn handle_unmount_widget_panel(&mut self, panel_key: &crate::widgets::PanelKey) {
        match self.widget_registry.unmount(panel_key) {
            Some(buffer_id) => {
                tracing::debug!(
                    "Unmounted widget panel {} (was rendering into {:?})",
                    panel_key,
                    buffer_id
                );
                // Buffer lifetime is owned by the plugin (it created the
                // virtual buffer before mounting). The plugin is
                // responsible for closing/clearing it; we only forget our
                // panel state.
            }
            None => {
                tracing::debug!("UnmountWidgetPanel for unknown panel {} ignored", panel_key);
            }
        }
    }

    fn handle_mount_floating_widget(
        &mut self,
        panel_key: crate::widgets::PanelKey,
        spec: fresh_core::api::WidgetSpec,
        width_pct: u8,
        height_pct: u8,
        as_dock: bool,
        focus_marker: bool,
    ) {
        let width_pct = width_pct.clamp(1, 100);
        let height_pct = height_pct.clamp(1, 100);
        // The dock mounts into its own slot so it coexists with a
        // centered modal; everything else is a centered overlay.
        let slot = if as_dock {
            super::PanelSlot::Dock
        } else {
            super::PanelSlot::Floating
        };
        let buffer_id = slot.buffer_id();
        // A centered modal owns the keyboard: blur a focused dock so the
        // two slots never both claim input. Without this, a dock key
        // handler (e.g. its Esc→blur) would greedily consume keys the
        // modal deferred to its own mode bindings, stranding the modal
        // open. Fires the dock's `blur` widget_event so the owning plugin
        // can mirror the state. Does nothing when the dock isn't focused.
        if !as_dock && self.dock.as_ref().is_some_and(|f| f.focused) {
            self.blur_floating_panel(super::PanelSlot::Dock);
        }
        let placement = if as_dock {
            let width = self
                .dock_width
                .unwrap_or(32)
                .clamp(10, self.terminal_width.max(20).saturating_sub(20).max(10));
            super::PanelPlacement::LeftDock { width_cols: width }
        } else {
            super::PanelPlacement::Centered
        };
        if let Some(existing) = self.panel_opt_mut(slot).take() {
            if existing.panel_key != panel_key {
                let _ = self.widget_registry.unmount(&existing.panel_key);
            }
        }
        *self.panel_opt_mut(slot) = Some(FloatingWidgetState {
            panel_key: panel_key.clone(),
            width_pct,
            height_pct,
            placement,
            focused: true,
            entries: Vec::new(),
            focus_cursor: None,
            embeds: Vec::new(),
            overlays: Vec::new(),
            scroll_regions: Vec::new(),
            scrollbar_tracks: Vec::new(),
            scrollbar_mouse: Default::default(),
            scrollbar_drag_key: None,
            last_inner_rect: None,
            scrollbar_hover_zones: Vec::new(),
            scrollbar_zone_hovered: false,
            fullscreen: false,
            focus_marker,
        });
        let prev = std::collections::HashMap::new();
        let prev_focus = String::new();
        let panel_width = self.floating_panel_inner_width(slot);
        let out = super::widget_runtime::render_floating_spec(
            focus_marker,
            &spec,
            &prev,
            &prev_focus,
            panel_width,
        );
        let focus_cursor = out.focus_cursor;
        let entries = out.entries;
        let embeds = out.embeds;
        let overlays = out.overlays;
        let scroll_regions = out.scroll_regions;
        self.widget_registry.mount(
            panel_key.clone(),
            buffer_id,
            spec,
            out.hits,
            out.instance_states,
            out.focus_key,
            out.tabbable,
        );
        if let Some(fwp) = self.panel_mut(slot) {
            fwp.entries = entries;
            fwp.focus_cursor = focus_cursor;
            fwp.embeds = embeds;
            fwp.overlays = overlays;
            fwp.scroll_regions = scroll_regions;
        }
        tracing::debug!(
            "Mounted floating widget panel {} ({}%x{}%)",
            panel_key,
            width_pct,
            height_pct
        );

        // Mounting a panel as the left dock carves a full-height column out
        // of the chrome. Run the single layout funnel so terminals and
        // viewports reflow to the post-dock width right away (a centered
        // panel leaves `dock_cols` at 0, so this is a cheap no-op there).
        if as_dock {
            self.relayout();
        }
    }

    fn handle_update_floating_widget(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        spec: fresh_core::api::WidgetSpec,
    ) {
        let Some(slot) = self.slot_of_panel(panel_key) else {
            tracing::debug!(
                "UpdateFloatingWidget for unknown / mismatched panel {} ignored",
                panel_key
            );
            return;
        };
        let prev = self
            .widget_registry
            .instance_states(panel_key)
            .cloned()
            .unwrap_or_default();
        let prev_focus = self
            .widget_registry
            .focus_key(panel_key)
            .map(|s| s.to_string())
            .unwrap_or_default();
        let panel_width = self.floating_panel_inner_width(slot);
        let focus_marker = self.panel(slot).map(|f| f.focus_marker).unwrap_or(false);
        let out = super::widget_runtime::render_floating_spec(
            focus_marker,
            &spec,
            &prev,
            &prev_focus,
            panel_width,
        );
        let focus_cursor = out.focus_cursor;
        let entries = out.entries;
        let embeds = out.embeds;
        let overlays = out.overlays;
        let scroll_regions = out.scroll_regions;
        if self
            .widget_registry
            .update(
                panel_key,
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
                panel_key
            );
            return;
        }
        if let Some(fwp) = self.panel_mut(slot) {
            fwp.entries = entries;
            fwp.focus_cursor = focus_cursor;
            fwp.embeds = embeds;
            fwp.overlays = overlays;
            fwp.scroll_regions = scroll_regions;
        }
    }

    fn handle_unmount_floating_widget(&mut self, panel_key: &crate::widgets::PanelKey) {
        let Some(slot) = self.slot_of_panel(panel_key) else {
            tracing::debug!(
                "UnmountFloatingWidget for unknown / mismatched panel {} ignored",
                panel_key
            );
            return;
        };
        *self.panel_opt_mut(slot) = None;
        let _ = self.widget_registry.unmount(panel_key);
        // Hiding the left dock frees its full-height column. The next
        // frame's `compute_dock_split` already lays the chrome back out
        // full-width (and the early command drain in `render` makes that
        // happen in the *same* frame as the unmount), so the layout is
        // correct — but the freed strip can still show stale glyphs from
        // the old dock until something repaints those cells. Force a full
        // clear+redraw so the reclaim is unconditional on every terminal,
        // mirroring how a resize relayout clears. Gated to the dock slot:
        // a centered modal overlays the full-width chrome without carving
        // it, so clearing on its close would only cause a visible flicker.
        if slot == super::PanelSlot::Dock {
            self.request_full_redraw();
        }
        // Restore the active window's visible terminal PTYs to their
        // dive-view split rects. The orchestrator picker's preview
        // pane shrinks PTYs to the embed size on every frame while
        // it's up (see `render_session_preview_into_rect`); when the
        // picker closes onto the *same* session the user was
        // previewing, `set_active_window` short-circuits because the
        // active pointer didn't move, and the shrink-down never gets
        // undone — top / vim / etc. keep drawing at the embed's ~15
        // rows. Resizing here on every panel unmount restores the
        // full dive-view dimensions; for panels that didn't preview
        // anything (the new-session form, plugin overlays) this is a
        // cheap no-op because the PTY sizes already match. Unmounting the
        // dock also frees its column, so route through the single layout
        // funnel: it re-derives `dock_cols` (now 0 for a dock unmount) and
        // reflows every window's terminals + viewports to the reclaimed width.
        self.relayout();
        tracing::debug!("Unmounted floating widget panel {}", panel_key);
    }

    /// Apply a `FloatingPanelControl` op. No-op if the panel id
    /// doesn't match the mounted floating panel.
    fn handle_floating_panel_control(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        op: &str,
        arg: f64,
    ) {
        let Some(slot) = self.slot_of_panel(panel_key) else {
            tracing::warn!("FloatingPanelControl for unknown/mismatched panel {panel_key} ignored");
            return;
        };
        // `blur` fires a widget_event, so handle it before borrowing the
        // panel — it reborrows `self` via the shared helper.
        if op == "blur" {
            self.blur_floating_panel(slot);
            return;
        }
        // Clamp the dock width relative to the terminal so it can never
        // swallow the whole chrome. Read before the &mut borrow below.
        // A user-dragged width (`dock_width`) overrides the plugin's
        // default so the resize survives toggling the dock off/on.
        let max_cols = self.terminal_width.max(20).saturating_sub(20).max(10);
        let persisted = self.dock_width;
        let Some(fwp) = self.panel_mut(slot) else {
            return;
        };
        // Whether this op changed the chrome geometry (dock width/placement),
        // so we know to re-derive the layout once the `fwp` borrow ends.
        let geometry_changed = match op {
            "dock" => {
                let requested = persisted.unwrap_or(arg as u16);
                let width_cols = requested.clamp(10, max_cols);
                fwp.placement = super::PanelPlacement::LeftDock { width_cols };
                fwp.focused = true;
                true
            }
            // Update the dock's width WITHOUT touching focus — used by the
            // plugin to make the dock responsive (re-issued on terminal
            // resize). Unlike "dock" this never steals keyboard focus back
            // from the editor, and it's a no-op unless the panel is already
            // docked. A user-dragged width still wins (persisted override).
            "dock_width" => {
                if let super::PanelPlacement::LeftDock { .. } = fwp.placement {
                    let requested = persisted.unwrap_or(arg as u16);
                    let width_cols = requested.clamp(10, max_cols);
                    fwp.placement = super::PanelPlacement::LeftDock { width_cols };
                    true
                } else {
                    false
                }
            }
            "center" => {
                fwp.placement = super::PanelPlacement::Centered;
                fwp.focused = true;
                true
            }
            // Place the panel as an unobtrusive content-sized popup anchored
            // at a screen cell (a right-click context menu). The (x, y) cell
            // is packed into the single `f64` arg as `y << 16 | x` — both fit
            // a u16 and the sum is exact in `f64`. No chrome-geometry change
            // (the dock/editor layout is untouched), so no relayout.
            "anchor" => {
                let packed = arg.max(0.0) as u64;
                let x = (packed & 0xFFFF) as u16;
                let y = ((packed >> 16) & 0xFFFF) as u16;
                fwp.placement = super::PanelPlacement::Anchored { x, y };
                fwp.focused = true;
                fwp.fullscreen = false;
                false
            }
            "focus" => {
                fwp.focused = true;
                false
            }
            // Render a centered panel over the whole frame (covering the
            // dimmed dock) instead of beside the dock in `chrome_area`.
            // `arg != 0` enables it. No chrome-geometry change (the dock
            // and editor layout are untouched — only where the modal
            // paints), so no relayout; the next frame reads the flag.
            "fullscreen" => {
                fwp.fullscreen = arg != 0.0;
                false
            }
            other => {
                tracing::warn!("FloatingPanelControl: unknown op {other:?}");
                false
            }
        };
        // The `fwp` mutable borrow ends above; now that the dock's
        // placement/width is settled, run the single layout funnel so
        // terminals, viewports and panels all reflow to the new chrome.
        if geometry_changed {
            self.relayout();
        }
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
        if let Some(lsp) = self.windows.get_mut(&__active_id).map(|w| &mut w.lsp) {
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
        let success = if let Some(lsp) = self.windows.get_mut(&__active_id).map(|w| &mut w.lsp) {
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

    /// Mark the buffer backing `path` read-only (plugin `markFileReadOnly`).
    /// Resolved by path so it composes race-free with a preceding `openFile`
    /// command: both are processed in order, so the buffer exists here.
    fn handle_mark_buffer_read_only(&mut self, path: std::path::PathBuf) {
        let buffer_id = self
            .active_window()
            .buffer_metadata
            .iter()
            .find(|(_, m)| m.file_path().map(|p| p == &path).unwrap_or(false))
            .map(|(id, _)| *id);
        match buffer_id {
            Some(id) => {
                self.active_window_mut().mark_buffer_read_only(id, true);
                tracing::debug!("Marked buffer {:?} read-only ({})", id, path.display());
            }
            None => {
                tracing::warn!(
                    "markFileReadOnly: no open buffer for path {}",
                    path.display()
                );
            }
        }
    }

    fn handle_set_lsp_root_uri(&mut self, language: String, uri: String) {
        tracing::info!("Plugin setting LSP root URI for {}: {}", language, uri);
        match uri.parse::<lsp_types::Uri>() {
            Ok(parsed_uri) => {
                let __active_id = self.active_window;
                if let Some(lsp) = self.windows.get_mut(&__active_id).map(|w| &mut w.lsp) {
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
            .find(|(_, tb)| tb.terminal_id == terminal_id)
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

/// Clamp a plugin-requested `[start, end)` text range to a buffer's live
/// length.
///
/// `getBufferText` callers size `end` from `getBufferLength`, which reads a
/// state snapshot that lags the authoritative buffer. When the buffer shrinks
/// in between (concurrent editor + external-process edits), the requested end
/// briefly exceeds the live length. Returning the available text is the right
/// behaviour — the plugin recomputes on the next change event — so clamp
/// instead of rejecting. `start` is pinned to `end` so an over-large start
/// yields an empty range rather than `start > end`.
fn clamp_buffer_text_range(start: usize, end: usize, len: usize) -> (usize, usize) {
    let end = end.min(len);
    let start = start.min(end);
    (start, end)
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

    use super::clamp_buffer_text_range;

    #[test]
    fn clamp_text_range_passes_through_in_bounds() {
        assert_eq!(clamp_buffer_text_range(0, 165, 165), (0, 165));
        assert_eq!(clamp_buffer_text_range(10, 50, 165), (10, 50));
    }

    /// The reported regression: `getBufferLength` returned a snapshot
    /// length one byte ahead of the live buffer (the file was shrinking
    /// under concurrent editor + external edits), so `getBufferText`
    /// requested `0..len+1`. Pre-fix this produced "Invalid range
    /// 0..165003 for buffer of length 165002"; now the end clamps down.
    #[test]
    fn clamp_text_range_clamps_stale_end_past_buffer() {
        assert_eq!(clamp_buffer_text_range(0, 165_003, 165_002), (0, 165_002));
    }

    #[test]
    fn clamp_text_range_pins_overlarge_start_to_empty() {
        // start beyond the live length must not yield start > end.
        assert_eq!(clamp_buffer_text_range(200, 250, 165), (165, 165));
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

        // Mirror the active session's recorded macros into the snapshot so
        // plugins can read them synchronously via `editor.listMacros()` /
        // `editor.getMacro()`. Computed before the split-layout borrow below
        // to avoid overlapping immutable borrows of `self`. Macros are few and
        // small, so rebuilding each tick is negligible.
        snapshot.macros = {
            let macros = &self.macros;
            macros
                .keys_sorted()
                .into_iter()
                .filter_map(|key| {
                    macros
                        .get(key)
                        .map(|actions| fresh_core::api::MacroSnapshot {
                            register: key.to_string(),
                            steps: actions.iter().map(|a| a.to_action_spec()).collect(),
                        })
                })
                .collect()
        };

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
            let is_preview = self.is_buffer_preview(*buffer_id);
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
                editing_disabled: state.editing_disabled,
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

                    // Resolve a byte offset to its 0-indexed line, but only when the
                    // active buffer has a line index. Huge files load without line
                    // metadata (`line_count() == None`); reporting `0` there would be
                    // a lie, so we surface `None` instead — the same guard the
                    // viewport's `top_line` uses below.
                    let line_of = |offset: usize| -> Option<usize> {
                        buffers_mut.get(&active_buf_id).and_then(|state| {
                            if state.buffer.line_count().is_some() {
                                Some(state.buffer.get_line_number(offset))
                            } else {
                                None
                            }
                        })
                    };

                    snapshot.primary_cursor = Some(CursorInfo {
                        position: primary_position,
                        selection: primary_selection.clone(),
                        line: line_of(primary_position),
                    });

                    // Mirror the editor's cached primary cursor line number so
                    // `getCursorLine()` returns a meaningful value without the
                    // plugin runtime having to scan the buffer. Falls back to
                    // 0 if the active buffer state isn't available.
                    snapshot.primary_cursor_line = Some(
                        buffers_mut
                            .get(&active_buf_id)
                            .map(|s| s.primary_cursor_line_number.value() as u32)
                            .unwrap_or(0),
                    );

                    snapshot.all_cursors = active_cursors
                        .iter()
                        .map(|(_, cursor)| CursorInfo {
                            position: cursor.position,
                            selection: cursor.selection_range(),
                            line: line_of(cursor.position),
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
                    snapshot.primary_cursor_line = None;
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
            // Plugin markers live only in the snapshot; drop closed buffers'.
            snapshot
                .plugin_markers
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

        // Update active search state so plugins can query it via hasActiveSearch()
        snapshot.has_active_search = self.search_state.is_some();
    }
}

// `editor.httpFetch` downloads stream through `services::http::download_to_file`,
// which keeps all ureq/TLS usage in one place (gated by the `http` feature).
