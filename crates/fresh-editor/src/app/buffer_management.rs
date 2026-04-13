//! Buffer management operations for the Editor.
//!
//! This module contains all methods related to buffer lifecycle and navigation:
//! - Opening files (with and without focus)
//! - Creating new buffers (regular and virtual)
//! - Closing buffers and tabs
//! - Switching between buffers
//! - Navigate back/forward in position history
//! - Buffer state persistence

use anyhow::Result as AnyhowResult;
use rust_i18n::t;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::app::warning_domains::WarningDomain;
use crate::model::event::{BufferId, Event, LeafId};
use crate::state::EditorState;
use crate::view::prompt::PromptType;
use crate::view::split::SplitViewState;

use super::help;
use super::Editor;

impl Editor {
    /// Resolve the effective line_wrap setting for a buffer, considering language overrides.
    ///
    /// Returns the language-specific `line_wrap` if set, otherwise the global `editor.line_wrap`.
    pub(super) fn resolve_line_wrap_for_buffer(&self, buffer_id: BufferId) -> bool {
        if let Some(state) = self.buffers.get(&buffer_id) {
            if let Some(lang_config) = self.config.languages.get(&state.language) {
                if let Some(line_wrap) = lang_config.line_wrap {
                    return line_wrap;
                }
            }
        }
        self.config.editor.line_wrap
    }

    /// Resolve page view settings for a buffer from its language config.
    ///
    /// Returns `Some((page_width))` if page_view is enabled for this buffer's language,
    /// `None` otherwise.
    pub(super) fn resolve_page_view_for_buffer(
        &self,
        buffer_id: BufferId,
    ) -> Option<Option<usize>> {
        let state = self.buffers.get(&buffer_id)?;
        let lang_config = self.config.languages.get(&state.language)?;
        if lang_config.page_view == Some(true) {
            Some(lang_config.page_width.or(self.config.editor.page_width))
        } else {
            None
        }
    }

    /// Resolve the effective wrap_column for a buffer, considering language overrides.
    ///
    /// Returns the language-specific `wrap_column` if set, otherwise the global `editor.wrap_column`.
    pub(super) fn resolve_wrap_column_for_buffer(&self, buffer_id: BufferId) -> Option<usize> {
        if let Some(state) = self.buffers.get(&buffer_id) {
            if let Some(lang_config) = self.config.languages.get(&state.language) {
                if lang_config.wrap_column.is_some() {
                    return lang_config.wrap_column;
                }
            }
        }
        self.config.editor.wrap_column
    }

    /// Get the preferred split for opening a file.
    /// If the active split has no label, use it (normal case).
    /// Otherwise find an unlabeled leaf so files don't open in labeled splits (e.g., sidebars).
    fn preferred_split_for_file(&self) -> LeafId {
        let active = self.split_manager.active_split();
        if self.split_manager.get_label(active.into()).is_none() {
            return active;
        }
        self.split_manager.find_unlabeled_leaf().unwrap_or(active)
    }

    /// Open a file in "preview" (ephemeral) mode and return its buffer ID.
    ///
    /// Used for exploratory single-click opens from the file explorer. If the
    /// `file_explorer.preview_tabs` setting is disabled, this is equivalent to
    /// `open_file`.
    ///
    /// Semantics (see `Editor::preview` for the full invariants):
    /// - Preview is anchored to a specific split. At most one preview exists
    ///   editor-wide.
    /// - If the file is already open (deduped by canonical path, including
    ///   symlinks and relative paths, by delegating to `open_file_no_focus`),
    ///   just switch to it. No preview-state changes in either direction.
    /// - Otherwise, if there's an existing preview in the **same** target
    ///   split, close it and replace it. If it's in a **different** split,
    ///   promote it (walking away is commitment) and start a fresh preview
    ///   in the target split.
    /// - Skips writing to position history, so a string of exploratory
    ///   clicks doesn't flood back/forward navigation with stale entries.
    ///
    /// TODO(perf): Each preview swap today triggers LSP didClose + didOpen.
    /// For heavy language servers (rust-analyzer, tsserver) that's wasteful
    /// on rapid browsing. A future optimization is to keep the LSP session
    /// for the outgoing buffer until the user commits to the new one.
    pub fn open_file_preview(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        // Feature gate — fall back to normal open when preview tabs are off.
        if !self.config.file_explorer.preview_tabs {
            return self.open_file(path);
        }

        // Decide target split up-front. `open_file_no_focus` will target
        // the same one (it calls `preferred_split_for_file` internally),
        // so this mirrors its logic. If that invariant ever drifts we'd
        // open the preview in one split and track it in another.
        let target_split = self.preferred_split_for_file();

        // Snapshot the buffer IDs that already back a real file, so we can
        // tell "opened a previously-unknown file" from "switched to one
        // that was already open". We delegate the symlink/relative-path
        // dedup to `open_file_no_focus` (which canonicalizes) — any buffer
        // with a non-empty file path is a candidate match. Note: the
        // initial empty buffer has a `BufferKind::File` with an empty
        // `PathBuf`, and we deliberately exclude it here because
        // `open_file_no_focus` may *repurpose* that buffer (same ID, new
        // content) for the newly-opened file.
        let previously_file_backed: HashSet<BufferId> = self
            .buffers
            .iter()
            .filter_map(|(id, state)| {
                state.buffer.file_path().and_then(|p| {
                    if p.as_os_str().is_empty() {
                        None
                    } else {
                        Some(*id)
                    }
                })
            })
            .collect();

        // Route through `open_file` with position-history suppression.
        // Using the regular `open_file` path keeps all cross-cutting concerns
        // (LSP, language detection, split targeting, status message, plugin
        // hooks) consistent with a normal open.
        self.suppress_position_history_once = true;
        let open_result = self.open_file(path);
        self.suppress_position_history_once = false;
        let buffer_id = open_result?;
        let is_new = !previously_file_backed.contains(&buffer_id);

        // Already-open buffer: leave preview state untouched. A previously-
        // committed tab must not be demoted back to preview, and the existing
        // preview (if any, in whichever split) is still valid.
        if !is_new {
            return Ok(buffer_id);
        }

        // New buffer. Resolve the existing preview (if any) relative to the
        // target split.
        match self.preview.take() {
            Some((prev_split, old_id)) if prev_split == target_split => {
                // Same split: close the old preview so the new one takes its
                // place. If close fails (modified buffer — shouldn't happen
                // because edits promote, but defend in depth), demote the
                // orphan to a permanent tab rather than leaving behind an
                // italic "(preview)" tab that will never be replaced.
                if let Err(e) = self.close_buffer(old_id) {
                    tracing::warn!(
                        "preview: could not replace stale preview buffer {:?}, demoting to permanent: {}",
                        old_id,
                        e
                    );
                    if let Some(m) = self.buffer_metadata.get_mut(&old_id) {
                        m.is_preview = false;
                    }
                }
            }
            Some((_other_split, old_id)) => {
                // Different split: user walked away from the old preview
                // before this click. Promote it to permanent — their focus
                // moving to another split was the commitment signal.
                if let Some(m) = self.buffer_metadata.get_mut(&old_id) {
                    m.is_preview = false;
                }
            }
            None => {}
        }

        // Mark the new buffer as the preview, anchored to its split.
        if let Some(meta) = self.buffer_metadata.get_mut(&buffer_id) {
            meta.is_preview = true;
        }
        self.preview = Some((target_split, buffer_id));

        Ok(buffer_id)
    }

    /// Promote a specific buffer from preview to permanent, if it was in
    /// preview mode. No-op if the buffer is not currently a preview.
    pub(crate) fn promote_buffer_from_preview(&mut self, buffer_id: BufferId) {
        if let Some(m) = self.buffer_metadata.get_mut(&buffer_id) {
            m.is_preview = false;
        }
        if let Some((_, id)) = self.preview {
            if id == buffer_id {
                self.preview = None;
            }
        }
    }

    /// Promote the active buffer from preview to permanent, if applicable.
    /// Called on any buffer mutation so that touching a preview buffer
    /// commits it to a permanent tab.
    pub(crate) fn promote_active_buffer_from_preview(&mut self) {
        let id = self.active_buffer();
        self.promote_buffer_from_preview(id);
    }

    /// Promote the current preview, regardless of which buffer it points at.
    /// Used before layout changes (split, close-split, move-tab) where the
    /// preview invariant ("anchored to a specific split") would otherwise
    /// be broken by the operation itself.
    pub(crate) fn promote_current_preview(&mut self) {
        if let Some((_, id)) = self.preview.take() {
            if let Some(m) = self.buffer_metadata.get_mut(&id) {
                m.is_preview = false;
            }
        }
    }

    /// Promote the current preview if it belongs to a split other than
    /// `new_split`. Called from split-focus-change paths so that moving
    /// focus away from the preview's pane commits it.
    pub(crate) fn promote_preview_if_not_in_split(&mut self, new_split: LeafId) {
        if let Some((preview_split, _)) = self.preview {
            if preview_split != new_split {
                self.promote_current_preview();
            }
        }
    }

    /// Whether the given buffer is currently in preview (ephemeral) mode.
    /// Primarily for tests; production code should use `self.preview`.
    pub fn is_buffer_preview(&self, buffer_id: BufferId) -> bool {
        self.buffer_metadata
            .get(&buffer_id)
            .map(|m| m.is_preview)
            .unwrap_or(false)
    }

    /// Number of open buffers (including hidden/virtual buffers).
    /// Intended for tests that verify preview tabs don't accumulate.
    pub fn open_buffer_count(&self) -> usize {
        self.buffers.len()
    }

    /// The (split, buffer) tuple of the current preview tab, if any.
    /// Intended for tests that verify preview anchoring semantics.
    pub fn current_preview(&self) -> Option<(LeafId, BufferId)> {
        self.preview
    }

    /// Open a file and return its buffer ID
    ///
    /// If the file doesn't exist, creates an unsaved buffer with that filename.
    /// Saving the buffer will create the file.
    pub fn open_file(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        // Check whether the active buffer had a file path before loading.
        // If it didn't, open_file_no_focus may replace the empty initial buffer
        // in-place (same buffer ID, new content), and we need to notify plugins.
        let active_had_path = self
            .buffers
            .get(&self.active_buffer())
            .and_then(|s| s.buffer.file_path())
            .is_some();

        let buffer_id = self.open_file_no_focus(path)?;

        // Check if this was an already-open buffer or a new one
        // For already-open buffers, just switch to them
        // For new buffers, record position history before switching
        let is_new_buffer = self.active_buffer() != buffer_id;

        if is_new_buffer && !self.suppress_position_history_once {
            // Save current position before switching to new buffer
            self.position_history.commit_pending_movement();

            // Explicitly record current position before switching
            let cursors = self.active_cursors();
            let position = cursors.primary().position;
            let anchor = cursors.primary().anchor;
            self.position_history
                .record_movement(self.active_buffer(), position, anchor);
            self.position_history.commit_pending_movement();
        }

        self.set_active_buffer(buffer_id);

        // If the initial empty buffer was replaced in-place with file content,
        // set_active_buffer is a no-op (same buffer ID). Fire buffer_activated
        // explicitly so plugins see the newly loaded file.
        // Skip this when re-opening an already-active file (active_had_path),
        // as nothing changed and the extra hook would cause spurious refreshes
        // in plugins like the diagnostics panel.
        if !is_new_buffer && !active_had_path {
            #[cfg(feature = "plugins")]
            self.update_plugin_state_snapshot();

            self.plugin_manager.run_hook(
                "buffer_activated",
                crate::services::plugins::hooks::HookArgs::BufferActivated { buffer_id },
            );
        }

        // Use display_name from metadata for relative path display
        let display_name = self
            .buffer_metadata
            .get(&buffer_id)
            .map(|m| m.display_name.clone())
            .unwrap_or_else(|| path.display().to_string());

        // Check if buffer is binary for status message
        let is_binary = self
            .buffers
            .get(&buffer_id)
            .map(|s| s.buffer.is_binary())
            .unwrap_or(false);

        // Show appropriate status message for binary vs regular files
        if is_binary {
            self.status_message = Some(t!("buffer.opened_binary", name = display_name).to_string());
        } else {
            self.status_message = Some(t!("buffer.opened", name = display_name).to_string());
        }

        Ok(buffer_id)
    }

    /// Open a file without switching focus to it
    ///
    /// Creates a new buffer for the file (or returns existing buffer ID if already open)
    /// but does not change the active buffer. Useful for opening files in background tabs.
    ///
    /// If the file doesn't exist, creates an unsaved buffer with that filename.
    pub fn open_file_no_focus(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        // Fail fast if the remote connection is down — don't attempt I/O that
        // would either timeout or return confusing errors.
        if !self.filesystem.is_remote_connected() {
            anyhow::bail!(
                "Cannot open file: remote connection lost ({})",
                self.filesystem
                    .remote_connection_info()
                    .unwrap_or("unknown host")
            );
        }

        // Resolve relative paths against appropriate base directory
        // For remote mode, use the remote home directory; for local, use working_dir
        let base_dir = if self.filesystem.remote_connection_info().is_some() {
            self.filesystem
                .home_dir()
                .unwrap_or_else(|_| self.working_dir.clone())
        } else {
            self.working_dir.clone()
        };

        let resolved_path = if path.is_relative() {
            base_dir.join(path)
        } else {
            path.to_path_buf()
        };

        // Determine if we're opening a non-existent file (for creating new files)
        // Use filesystem trait method to support remote files
        let file_exists = self.filesystem.exists(&resolved_path);

        // Save the user-visible (non-canonicalized) path for language detection.
        // Glob patterns in language config should match the path as the user sees it,
        // not the canonical path (e.g., on macOS /var -> /private/var symlinks).
        let display_path = resolved_path.clone();

        // Canonicalize the path to resolve symlinks and normalize path components
        // This ensures consistent path representation throughout the editor
        // For non-existent files, we need to canonicalize the parent directory and append the filename
        let canonical_path = if file_exists {
            self.filesystem
                .canonicalize(&resolved_path)
                .unwrap_or_else(|_| resolved_path.clone())
        } else {
            // For non-existent files, canonicalize parent dir and append filename
            if let Some(parent) = resolved_path.parent() {
                let canonical_parent = if parent.as_os_str().is_empty() {
                    // No parent means just a filename, use base dir
                    base_dir.clone()
                } else {
                    self.filesystem
                        .canonicalize(parent)
                        .unwrap_or_else(|_| parent.to_path_buf())
                };
                if let Some(filename) = resolved_path.file_name() {
                    canonical_parent.join(filename)
                } else {
                    resolved_path
                }
            } else {
                resolved_path
            }
        };
        let path = canonical_path.as_path();

        // Check if the path is a directory (after following symlinks via canonicalize)
        // Directories cannot be opened as files in the editor
        // Use filesystem trait method to support remote files
        if self.filesystem.is_dir(path).unwrap_or(false) {
            anyhow::bail!(t!("buffer.cannot_open_directory"));
        }

        // Check if file is already open - return existing buffer without switching
        let already_open = self
            .buffers
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(path))
            .map(|(id, _)| *id);

        if let Some(id) = already_open {
            return Ok(id);
        }

        // If the current buffer is empty and unmodified, replace it instead of creating a new one
        // Note: Don't replace composite buffers (they appear empty but are special views)
        let replace_current = {
            let current_state = self.buffers.get(&self.active_buffer()).unwrap();
            !current_state.is_composite_buffer
                && current_state.buffer.is_empty()
                && !current_state.buffer.is_modified()
                && current_state.buffer.file_path().is_none()
        };

        let buffer_id = if replace_current {
            // Reuse the current empty buffer
            self.active_buffer()
        } else {
            // Create new buffer for this file
            let id = BufferId(self.next_buffer_id);
            self.next_buffer_id += 1;
            id
        };

        // Create the editor state - either load from file or create empty buffer
        tracing::info!(
            "[SYNTAX DEBUG] open_file_no_focus: path={:?}, extension={:?}, registry_syntaxes={}, user_extensions={:?}",
            path,
            path.extension(),
            self.grammar_registry.available_syntaxes().len(),
            self.grammar_registry.user_extensions_debug()
        );
        let mut state = if file_exists {
            // Load from canonical path (for I/O and dedup), detect language from
            // display path (for glob pattern matching against user-visible names).
            let buffer = crate::model::buffer::Buffer::load_from_file(
                &canonical_path,
                self.config.editor.large_file_threshold_bytes as usize,
                Arc::clone(&self.filesystem),
            )?;
            let detected =
                crate::primitives::detected_language::DetectedLanguage::from_path_with_fallback(
                    &display_path,
                    &self.grammar_registry,
                    &self.config.languages,
                    self.config.default_language.as_deref(),
                );
            EditorState::from_buffer_with_language(buffer, detected)
        } else {
            // File doesn't exist - create empty buffer with the file path set
            EditorState::new_with_path(
                self.config.editor.large_file_threshold_bytes as usize,
                Arc::clone(&self.filesystem),
                path.to_path_buf(),
            )
        };
        // Note: line_wrap_enabled is set on SplitViewState.viewport when the split is created

        // Check if the buffer contains binary content
        let is_binary = state.buffer.is_binary();
        if is_binary {
            // Make binary buffers read-only
            state.editing_disabled = true;
            tracing::info!("Detected binary file: {}", path.display());
        }

        // Set whitespace visibility, use_tabs, and tab_size based on language config
        // with fallback to global editor config for tab_size
        // Use the buffer's stored language (already set by from_file_with_languages)
        let mut whitespace =
            crate::config::WhitespaceVisibility::from_editor_config(&self.config.editor);
        state.buffer_settings.auto_close = self.config.editor.auto_close;
        state.buffer_settings.auto_surround = self.config.editor.auto_surround;
        if let Some(lang_config) = self.config.languages.get(&state.language) {
            whitespace = whitespace.with_language_tab_override(lang_config.show_whitespace_tabs);
            state.buffer_settings.use_tabs =
                lang_config.use_tabs.unwrap_or(self.config.editor.use_tabs);
            // Use language-specific tab_size if set, otherwise fall back to global
            state.buffer_settings.tab_size =
                lang_config.tab_size.unwrap_or(self.config.editor.tab_size);
            // Auto close: language override (only if globally enabled)
            if state.buffer_settings.auto_close {
                if let Some(lang_auto_close) = lang_config.auto_close {
                    state.buffer_settings.auto_close = lang_auto_close;
                }
            }
            // Auto surround: language override (only if globally enabled)
            if state.buffer_settings.auto_surround {
                if let Some(lang_auto_surround) = lang_config.auto_surround {
                    state.buffer_settings.auto_surround = lang_auto_surround;
                }
            }
        } else {
            state.buffer_settings.tab_size = self.config.editor.tab_size;
            state.buffer_settings.use_tabs = self.config.editor.use_tabs;
        }
        state.buffer_settings.whitespace = whitespace;

        // Apply line_numbers default from config
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Create metadata for this buffer
        let mut metadata = super::types::BufferMetadata::with_file(
            path.to_path_buf(),
            &display_path,
            &self.working_dir,
        );

        // Mark binary files in metadata and disable LSP
        if is_binary {
            metadata.binary = true;
            metadata.read_only = true;
            metadata.disable_lsp(t!("buffer.binary_file").to_string());
        }

        // Check if the file is read-only on disk (filesystem permissions)
        if file_exists && !metadata.read_only && !self.filesystem.is_writable(path) {
            metadata.read_only = true;
        }

        // Mark read-only files (library, binary, or filesystem-readonly) as editing-disabled
        if metadata.read_only {
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                state.editing_disabled = true;
            }
        }

        // Notify LSP about the newly opened file (skip for binary files)
        if !is_binary {
            self.notify_lsp_file_opened(path, buffer_id, &mut metadata);
        }

        // Store metadata for this buffer
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add buffer to the preferred split's tabs (but don't switch to it)
        // Uses preferred_split_for_file() to avoid opening in labeled splits (e.g., sidebars)
        let target_split = self.preferred_split_for_file();
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        let page_view = self.resolve_page_view_for_buffer(buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            view_state.add_buffer(buffer_id);
            // Initialize per-buffer view state for the new buffer with config defaults
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                line_wrap,
                self.config.editor.wrap_indent,
                wrap_column,
                self.config.editor.rulers.clone(),
            );
            // Auto-activate page view if configured for this language
            if let Some(page_width) = page_view {
                buf_state.activate_page_view(page_width);
            }
        }

        // Restore global file state (scroll/cursor position) if available
        // This persists file positions across projects and editor instances
        self.restore_global_file_state(buffer_id, path, target_split);

        // Emit control event
        self.emit_event(
            crate::model::control_event::events::FILE_OPENED.name,
            serde_json::json!({
                "path": path.display().to_string(),
                "buffer_id": buffer_id.0
            }),
        );

        // Track file for auto-revert and conflict detection
        self.watch_file(path);

        // Fire AfterFileOpen hook for plugins
        self.plugin_manager.run_hook(
            "after_file_open",
            crate::services::plugins::hooks::HookArgs::AfterFileOpen {
                buffer_id,
                path: path.to_path_buf(),
            },
        );

        Ok(buffer_id)
    }

    /// Open a local file (always uses local filesystem, not remote)
    ///
    /// This is used for opening local files like log files when in remote mode.
    /// Unlike `open_file`, this always uses the local filesystem even when
    /// the editor is connected to a remote server.
    pub fn open_local_file(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        // Resolve relative paths against working_dir
        let resolved_path = if path.is_relative() {
            self.working_dir.join(path)
        } else {
            path.to_path_buf()
        };

        // Save user-visible path for language detection before canonicalizing
        let display_path = resolved_path.clone();

        // Canonicalize the path
        let canonical_path = resolved_path
            .canonicalize()
            .unwrap_or_else(|_| resolved_path.clone());
        let path = canonical_path.as_path();

        // Check if already open
        let already_open = self
            .buffers
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(path))
            .map(|(id, _)| *id);

        if let Some(id) = already_open {
            self.set_active_buffer(id);
            return Ok(id);
        }

        // Create new buffer
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        // Load from canonical path (for I/O and dedup), detect language from
        // display path (for glob pattern matching against user-visible names).
        let buffer = crate::model::buffer::Buffer::load_from_file(
            &canonical_path,
            self.config.editor.large_file_threshold_bytes as usize,
            Arc::clone(&self.local_filesystem),
        )?;
        let detected =
            crate::primitives::detected_language::DetectedLanguage::from_path_with_fallback(
                &display_path,
                &self.grammar_registry,
                &self.config.languages,
                self.config.default_language.as_deref(),
            );
        let state = EditorState::from_buffer_with_language(buffer, detected);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Create metadata
        let metadata = super::types::BufferMetadata::with_file(
            path.to_path_buf(),
            &display_path,
            &self.working_dir,
        );
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add to preferred split's tabs (avoids labeled splits like sidebars)
        let target_split = self.preferred_split_for_file();
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                line_wrap,
                self.config.editor.wrap_indent,
                wrap_column,
                self.config.editor.rulers.clone(),
            );
        }

        self.set_active_buffer(buffer_id);

        let display_name = path.display().to_string();
        self.status_message = Some(t!("buffer.opened", name = display_name).to_string());

        Ok(buffer_id)
    }

    /// Open a file with a specific encoding (no auto-detection).
    ///
    /// Used when the user disables auto-detection in the file browser
    /// and selects a specific encoding to use.
    pub fn open_file_with_encoding(
        &mut self,
        path: &Path,
        encoding: crate::model::buffer::Encoding,
    ) -> anyhow::Result<BufferId> {
        // Use the same base directory logic as open_file
        let base_dir = self.working_dir.clone();

        let resolved_path = if path.is_relative() {
            base_dir.join(path)
        } else {
            path.to_path_buf()
        };

        // Save user-visible path for language detection before canonicalizing
        let display_path = resolved_path.clone();

        // Canonicalize the path
        let canonical_path = self
            .filesystem
            .canonicalize(&resolved_path)
            .unwrap_or_else(|_| resolved_path.clone());
        let path = canonical_path.as_path();

        // Check if already open
        let already_open = self
            .buffers
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(path))
            .map(|(id, _)| *id);

        if let Some(id) = already_open {
            // File is already open - update its encoding and reload
            if let Some(state) = self.buffers.get_mut(&id) {
                state.buffer.set_encoding(encoding);
            }
            self.set_active_buffer(id);
            return Ok(id);
        }

        // Create new buffer with specified encoding
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        // Load buffer with the specified encoding (use canonical path for I/O)
        let buffer = crate::model::buffer::Buffer::load_from_file_with_encoding(
            path,
            encoding,
            Arc::clone(&self.filesystem),
            crate::model::buffer::BufferConfig {
                estimated_line_length: self.config.editor.estimated_line_length,
            },
        )?;
        // Create editor state with the buffer
        // Use display_path for language detection (glob patterns match user-visible paths)
        let detected =
            crate::primitives::detected_language::DetectedLanguage::from_path_with_fallback(
                &display_path,
                &self.grammar_registry,
                &self.config.languages,
                self.config.default_language.as_deref(),
            );

        let mut state = EditorState::from_buffer_with_language(buffer, detected);

        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        let metadata = super::types::BufferMetadata::with_file(
            path.to_path_buf(),
            &display_path,
            &self.working_dir,
        );
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add to preferred split's tabs (avoids labeled splits like sidebars)
        let target_split = self.preferred_split_for_file();
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                line_wrap,
                self.config.editor.wrap_indent,
                wrap_column,
                self.config.editor.rulers.clone(),
            );
        }

        self.set_active_buffer(buffer_id);

        Ok(buffer_id)
    }

    /// Reload the current file with a specific encoding.
    ///
    /// Requires the buffer to have no unsaved modifications.
    pub fn reload_with_encoding(
        &mut self,
        encoding: crate::model::buffer::Encoding,
    ) -> anyhow::Result<()> {
        let buffer_id = self.active_buffer();

        // Get the file path
        let path = self
            .buffers
            .get(&buffer_id)
            .and_then(|s| s.buffer.file_path().map(|p| p.to_path_buf()))
            .ok_or_else(|| anyhow::anyhow!("Buffer has no file path"))?;

        // Check for unsaved modifications
        if let Some(state) = self.buffers.get(&buffer_id) {
            if state.buffer.is_modified() {
                anyhow::bail!("Cannot reload: buffer has unsaved modifications");
            }
        }

        // Reload the buffer with the new encoding
        let new_buffer = crate::model::buffer::Buffer::load_from_file_with_encoding(
            &path,
            encoding,
            Arc::clone(&self.filesystem),
            crate::model::buffer::BufferConfig {
                estimated_line_length: self.config.editor.estimated_line_length,
            },
        )?;

        // Update the buffer in the editor state
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer = new_buffer;
            // Invalidate highlighting
            state.highlighter.invalidate_all();
        }

        // Reset cursor to start in the split view state
        let split_id = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
            if let Some(buf_state) = view_state.keyed_states.get_mut(&buffer_id) {
                buf_state.cursors = crate::model::cursor::Cursors::new();
            }
        }

        Ok(())
    }

    /// Open a large file with confirmed full loading for non-resynchronizable encoding.
    ///
    /// Called after user confirms they want to load a large file with an encoding like
    /// GB18030, GBK, Shift-JIS, or EUC-KR that requires loading the entire file into memory.
    pub fn open_file_large_encoding_confirmed(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        // Use the same base directory logic as open_file
        let base_dir = self.working_dir.clone();

        let resolved_path = if path.is_relative() {
            base_dir.join(path)
        } else {
            path.to_path_buf()
        };

        // Save user-visible path for language detection before canonicalizing
        let display_path = resolved_path.clone();

        // Canonicalize the path
        let canonical_path = self
            .filesystem
            .canonicalize(&resolved_path)
            .unwrap_or_else(|_| resolved_path.clone());
        let path = canonical_path.as_path();

        // Check if already open
        let already_open = self
            .buffers
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(path))
            .map(|(id, _)| *id);

        if let Some(id) = already_open {
            self.set_active_buffer(id);
            return Ok(id);
        }

        // Create new buffer with forced full loading
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        // Load buffer with forced full loading (bypasses the large file encoding check)
        let buffer = crate::model::buffer::Buffer::load_large_file_confirmed(
            path,
            Arc::clone(&self.filesystem),
        )?;
        // Create editor state with the buffer
        // Use display_path for language detection (glob patterns match user-visible paths)
        let detected =
            crate::primitives::detected_language::DetectedLanguage::from_path_with_fallback(
                &display_path,
                &self.grammar_registry,
                &self.config.languages,
                self.config.default_language.as_deref(),
            );

        let mut state = EditorState::from_buffer_with_language(buffer, detected);

        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        let metadata = super::types::BufferMetadata::with_file(
            path.to_path_buf(),
            &display_path,
            &self.working_dir,
        );
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add to preferred split's tabs (avoids labeled splits like sidebars)
        let target_split = self.preferred_split_for_file();
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                line_wrap,
                self.config.editor.wrap_indent,
                wrap_column,
                self.config.editor.rulers.clone(),
            );
        }

        self.set_active_buffer(buffer_id);

        // Use display_name from metadata for relative path display
        let display_name = self
            .buffer_metadata
            .get(&buffer_id)
            .map(|m| m.display_name.clone())
            .unwrap_or_else(|| path.display().to_string());

        self.status_message = Some(t!("buffer.opened", name = display_name).to_string());

        Ok(buffer_id)
    }

    /// Restore global file state (cursor and scroll position) for a newly opened file
    ///
    /// This looks up the file's saved state from the global file states store
    /// and applies it to both the EditorState (cursor) and SplitViewState (viewport).
    fn restore_global_file_state(&mut self, buffer_id: BufferId, path: &Path, split_id: LeafId) {
        use crate::workspace::PersistedFileWorkspace;

        // Load the per-file state for this path (lazy load from disk)
        let file_state = match PersistedFileWorkspace::load(path) {
            Some(state) => state,
            None => return, // No saved state for this file
        };

        // Get the buffer to validate positions
        let max_pos = match self.buffers.get(&buffer_id) {
            Some(buffer) => buffer.buffer.len(),
            None => return,
        };

        // Apply cursor position and viewport (scroll) state to SplitViewState
        if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
            if let Some(buf_state) = view_state.keyed_states.get_mut(&buffer_id) {
                let cursor_pos = file_state.cursor.position.min(max_pos);
                buf_state.cursors.primary_mut().position = cursor_pos;
                buf_state.cursors.primary_mut().anchor =
                    file_state.cursor.anchor.map(|a| a.min(max_pos));
            }
            view_state.viewport.top_byte = file_state.scroll.top_byte;
            view_state.viewport.left_column = file_state.scroll.left_column;
        }
    }

    /// Save file state when a buffer is closed (for per-file session persistence)
    fn save_file_state_on_close(&self, buffer_id: BufferId) {
        use crate::workspace::{
            PersistedFileWorkspace, SerializedCursor, SerializedFileState, SerializedScroll,
        };

        // Get the file path for this buffer
        let abs_path = match self.buffer_metadata.get(&buffer_id) {
            Some(metadata) => match metadata.file_path() {
                Some(path) => path.to_path_buf(),
                None => return, // Not a file buffer
            },
            None => return,
        };

        // Find a split that has this buffer open to get the view state
        let view_state = self
            .split_view_states
            .values()
            .find(|vs| vs.has_buffer(buffer_id));

        let view_state = match view_state {
            Some(vs) => vs,
            None => return, // No split has this buffer
        };

        // Get the per-buffer view state (not necessarily the active buffer in this split)
        let buf_state = match view_state.keyed_states.get(&buffer_id) {
            Some(bs) => bs,
            None => return,
        };

        // Capture the current state
        let primary_cursor = buf_state.cursors.primary();
        let file_state = SerializedFileState {
            cursor: SerializedCursor {
                position: primary_cursor.position,
                anchor: primary_cursor.anchor,
                sticky_column: primary_cursor.sticky_column,
            },
            additional_cursors: buf_state
                .cursors
                .iter()
                .skip(1)
                .map(|(_, cursor)| SerializedCursor {
                    position: cursor.position,
                    anchor: cursor.anchor,
                    sticky_column: cursor.sticky_column,
                })
                .collect(),
            scroll: SerializedScroll {
                top_byte: buf_state.viewport.top_byte,
                top_view_line_offset: buf_state.viewport.top_view_line_offset,
                left_column: buf_state.viewport.left_column,
            },
            view_mode: Default::default(),
            compose_width: None,
            plugin_state: std::collections::HashMap::new(),
            folds: Vec::new(),
        };

        // Save to disk
        PersistedFileWorkspace::save(&abs_path, file_state);
        tracing::debug!("Saved file state on close for {:?}", abs_path);
    }

    /// Navigate to a specific line and column in the active buffer.
    ///
    /// Line and column are 1-indexed (matching typical editor conventions).
    /// If the line is out of bounds, navigates to the last line.
    /// If the column is out of bounds, navigates to the end of the line.
    pub fn goto_line_col(&mut self, line: usize, column: Option<usize>) {
        if line == 0 {
            return; // Line numbers are 1-indexed
        }

        let buffer_id = self.active_buffer();

        // Read cursor state from split view state
        let cursors = self.active_cursors();
        let cursor_id = cursors.primary_id();
        let old_position = cursors.primary().position;
        let old_anchor = cursors.primary().anchor;
        let old_sticky_column = cursors.primary().sticky_column;

        if let Some(state) = self.buffers.get(&buffer_id) {
            let has_line_index = state.buffer.line_count().is_some();
            let has_line_scan = state.buffer.has_line_feed_scan();
            let buffer_len = state.buffer.len();

            // Convert 1-indexed line to 0-indexed
            let target_line = line.saturating_sub(1);
            // Column is also 1-indexed, convert to 0-indexed
            let target_col = column.map(|c| c.saturating_sub(1)).unwrap_or(0);

            // Track the known exact line number for scanned large files,
            // since offset_to_position may not be able to reverse-resolve it accurately.
            let mut known_line: Option<usize> = None;

            let position = if has_line_scan && has_line_index {
                // Scanned large file: use tree metadata to find exact line offset
                let max_line = state.buffer.line_count().unwrap_or(1).saturating_sub(1);
                let actual_line = target_line.min(max_line);
                known_line = Some(actual_line);
                // Need mutable access to potentially read chunk data from disk
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    state
                        .buffer
                        .resolve_line_byte_offset(actual_line)
                        .map(|offset| (offset + target_col).min(buffer_len))
                        .unwrap_or(0)
                } else {
                    0
                }
            } else {
                // Small file with full line starts or no line index:
                // use exact line position
                let max_line = state.buffer.line_count().unwrap_or(1).saturating_sub(1);
                let actual_line = target_line.min(max_line);
                state.buffer.line_col_to_position(actual_line, target_col)
            };

            let event = Event::MoveCursor {
                cursor_id,
                old_position,
                new_position: position,
                old_anchor,
                new_anchor: None,
                old_sticky_column,
                new_sticky_column: target_col,
            };

            let split_id = self.split_manager.active_split();
            let state = self.buffers.get_mut(&buffer_id).unwrap();
            let view_state = self.split_view_states.get_mut(&split_id).unwrap();
            state.apply(&mut view_state.cursors, &event);

            // For scanned large files, override the line number with the known exact value
            // since offset_to_position may fall back to proportional estimation.
            if let Some(line) = known_line {
                state.primary_cursor_line_number = crate::model::buffer::LineNumber::Absolute(line);
            }
        }
    }

    /// Select a range in the active buffer. Lines/columns are 1-indexed.
    /// The cursor moves to the end of the range and the anchor is set to the
    /// start, producing a visual selection.
    pub fn select_range(
        &mut self,
        start_line: usize,
        start_col: Option<usize>,
        end_line: usize,
        end_col: Option<usize>,
    ) {
        if start_line == 0 || end_line == 0 {
            return;
        }

        let buffer_id = self.active_buffer();

        let cursors = self.active_cursors();
        let cursor_id = cursors.primary_id();
        let old_position = cursors.primary().position;
        let old_anchor = cursors.primary().anchor;
        let old_sticky_column = cursors.primary().sticky_column;

        if let Some(state) = self.buffers.get(&buffer_id) {
            let buffer_len = state.buffer.len();

            // Convert 1-indexed to 0-indexed
            let start_line_0 = start_line.saturating_sub(1);
            let start_col_0 = start_col.map(|c| c.saturating_sub(1)).unwrap_or(0);
            let end_line_0 = end_line.saturating_sub(1);
            let end_col_0 = end_col.map(|c| c.saturating_sub(1)).unwrap_or(0);

            let max_line = state.buffer.line_count().unwrap_or(1).saturating_sub(1);

            let start_pos = state
                .buffer
                .line_col_to_position(start_line_0.min(max_line), start_col_0)
                .min(buffer_len);
            let end_pos = state
                .buffer
                .line_col_to_position(end_line_0.min(max_line), end_col_0)
                .min(buffer_len);

            let event = Event::MoveCursor {
                cursor_id,
                old_position,
                new_position: end_pos,
                old_anchor,
                new_anchor: Some(start_pos),
                old_sticky_column,
                new_sticky_column: end_col_0,
            };

            let split_id = self.split_manager.active_split();
            let state = self.buffers.get_mut(&buffer_id).unwrap();
            let view_state = self.split_view_states.get_mut(&split_id).unwrap();
            state.apply(&mut view_state.cursors, &event);
        }
    }

    /// Go to an exact byte offset in the buffer (used in byte-offset mode for large files)
    pub fn goto_byte_offset(&mut self, offset: usize) {
        let buffer_id = self.active_buffer();

        let cursors = self.active_cursors();
        let cursor_id = cursors.primary_id();
        let old_position = cursors.primary().position;
        let old_anchor = cursors.primary().anchor;
        let old_sticky_column = cursors.primary().sticky_column;

        if let Some(state) = self.buffers.get(&buffer_id) {
            let buffer_len = state.buffer.len();
            let position = offset.min(buffer_len);

            let event = Event::MoveCursor {
                cursor_id,
                old_position,
                new_position: position,
                old_anchor,
                new_anchor: None,
                old_sticky_column,
                new_sticky_column: 0,
            };

            let split_id = self.split_manager.active_split();
            let state = self.buffers.get_mut(&buffer_id).unwrap();
            let view_state = self.split_view_states.get_mut(&split_id).unwrap();
            state.apply(&mut view_state.cursors, &event);
        }
    }

    /// Create a new empty buffer
    pub fn new_buffer(&mut self) -> BufferId {
        // Save current position before switching to new buffer
        self.position_history.commit_pending_movement();

        // Explicitly record current position before switching
        let cursors = self.active_cursors();
        let position = cursors.primary().position;
        let anchor = cursors.primary().anchor;
        self.position_history
            .record_movement(self.active_buffer(), position, anchor);
        self.position_history.commit_pending_movement();

        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            Arc::clone(&self.filesystem),
        );
        // Note: line_wrap_enabled is set on SplitViewState.viewport when the split is created
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);
        // Set default line ending for new buffers from config
        state
            .buffer
            .set_default_line_ending(self.config.editor.default_line_ending.to_line_ending());
        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());
        self.buffer_metadata
            .insert(buffer_id, crate::app::types::BufferMetadata::new());

        self.set_active_buffer(buffer_id);

        // Initialize per-buffer view state with config defaults.
        // Must happen AFTER set_active_buffer, because switch_buffer creates
        // the new BufferViewState with defaults (show_line_numbers=true).
        let active_split = self.split_manager.active_split();
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                line_wrap,
                self.config.editor.wrap_indent,
                wrap_column,
                self.config.editor.rulers.clone(),
            );
        }

        self.status_message = Some(t!("buffer.new").to_string());

        buffer_id
    }

    /// Create a new buffer from stdin content stored in a temp file
    ///
    /// Uses lazy chunk loading for efficient handling of large stdin inputs.
    /// The buffer is unnamed (no file path for save) - saving will prompt for a filename.
    /// The temp file path is preserved internally for lazy loading to work.
    ///
    /// # Arguments
    /// * `temp_path` - Path to temp file where stdin content is being written
    /// * `thread_handle` - Optional handle to background thread streaming stdin to temp file
    pub fn open_stdin_buffer(
        &mut self,
        temp_path: &Path,
        thread_handle: Option<std::thread::JoinHandle<anyhow::Result<()>>>,
    ) -> AnyhowResult<BufferId> {
        // Save current position before switching to new buffer
        self.position_history.commit_pending_movement();

        // Explicitly record current position before switching
        let cursors = self.active_cursors();
        let position = cursors.primary().position;
        let anchor = cursors.primary().anchor;
        self.position_history
            .record_movement(self.active_buffer(), position, anchor);
        self.position_history.commit_pending_movement();

        // If the current buffer is empty and unmodified, replace it instead of creating a new one
        // Note: Don't replace composite buffers (they appear empty but are special views)
        let replace_current = {
            let current_state = self.buffers.get(&self.active_buffer()).unwrap();
            !current_state.is_composite_buffer
                && current_state.buffer.is_empty()
                && !current_state.buffer.is_modified()
                && current_state.buffer.file_path().is_none()
        };

        let buffer_id = if replace_current {
            // Reuse the current empty buffer
            self.active_buffer()
        } else {
            // Create new buffer ID
            let id = BufferId(self.next_buffer_id);
            self.next_buffer_id += 1;
            id
        };

        // Get file size for status message before loading
        let file_size = self.filesystem.metadata(temp_path)?.size as usize;

        // Load from temp file using EditorState::from_file_with_languages
        // This enables lazy chunk loading for large inputs (>100MB by default)
        let mut state = EditorState::from_file_with_languages(
            temp_path,
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            &self.grammar_registry,
            &self.config.languages,
            Arc::clone(&self.filesystem),
        )?;

        // Clear the file path so the buffer is "unnamed" for save purposes
        // The Unloaded chunks still reference the temp file for lazy loading
        state.buffer.clear_file_path();
        // Clear modified flag - content is "fresh" from stdin (vim behavior)
        state.buffer.clear_modified();

        // Set tab size, auto_close, and auto_surround from config
        state.buffer_settings.tab_size = self.config.editor.tab_size;
        state.buffer_settings.auto_close = self.config.editor.auto_close;
        state.buffer_settings.auto_surround = self.config.editor.auto_surround;

        // Apply line_numbers default from config
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Create metadata for this buffer (no file path)
        let metadata =
            super::types::BufferMetadata::new_unnamed(t!("stdin.display_name").to_string());
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add buffer to the active split's tabs
        let active_split = self.split_manager.active_split();
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                line_wrap,
                self.config.editor.wrap_indent,
                wrap_column,
                self.config.editor.rulers.clone(),
            );
        }

        self.set_active_buffer(buffer_id);

        // Set up stdin streaming state for polling
        // If no thread handle, it means data is already complete (testing scenario)
        let complete = thread_handle.is_none();
        self.stdin_streaming = Some(super::StdinStreamingState {
            temp_path: temp_path.to_path_buf(),
            buffer_id,
            last_known_size: file_size,
            complete,
            thread_handle,
        });

        // Status will be updated by poll_stdin_streaming
        self.status_message = Some(t!("stdin.streaming").to_string());

        Ok(buffer_id)
    }

    /// Poll stdin streaming state and extend buffer if file grew.
    /// Returns true if the status changed (needs render).
    pub fn poll_stdin_streaming(&mut self) -> bool {
        let Some(ref mut stream_state) = self.stdin_streaming else {
            return false;
        };

        if stream_state.complete {
            return false;
        }

        let mut changed = false;

        // Check current file size
        let current_size = self
            .filesystem
            .metadata(&stream_state.temp_path)
            .map(|m| m.size as usize)
            .unwrap_or(stream_state.last_known_size);

        // If file grew, extend the buffer
        if current_size > stream_state.last_known_size {
            if let Some(editor_state) = self.buffers.get_mut(&stream_state.buffer_id) {
                editor_state
                    .buffer
                    .extend_streaming(&stream_state.temp_path, current_size);
            }
            stream_state.last_known_size = current_size;

            // Update status message with current progress
            self.status_message =
                Some(t!("stdin.streaming_bytes", bytes = current_size).to_string());
            changed = true;
        }

        // Check if background thread has finished
        let thread_finished = stream_state
            .thread_handle
            .as_ref()
            .map(|h| h.is_finished())
            .unwrap_or(true);

        if thread_finished {
            // Take ownership of handle to join it
            if let Some(handle) = stream_state.thread_handle.take() {
                match handle.join() {
                    Ok(Ok(())) => {
                        tracing::info!("Stdin streaming completed successfully");
                    }
                    Ok(Err(e)) => {
                        tracing::warn!("Stdin streaming error: {}", e);
                        self.status_message =
                            Some(t!("stdin.read_error", error = e.to_string()).to_string());
                    }
                    Err(_) => {
                        tracing::warn!("Stdin streaming thread panicked");
                        self.status_message = Some(t!("stdin.read_error_panic").to_string());
                    }
                }
            }
            self.complete_stdin_streaming();
            changed = true;
        }

        changed
    }

    /// Mark stdin streaming as complete.
    /// Called when the background thread finishes.
    pub fn complete_stdin_streaming(&mut self) {
        if let Some(ref mut stream_state) = self.stdin_streaming {
            stream_state.complete = true;

            // Final poll to get any remaining data
            let final_size = self
                .filesystem
                .metadata(&stream_state.temp_path)
                .map(|m| m.size as usize)
                .unwrap_or(stream_state.last_known_size);

            if final_size > stream_state.last_known_size {
                if let Some(editor_state) = self.buffers.get_mut(&stream_state.buffer_id) {
                    editor_state
                        .buffer
                        .extend_streaming(&stream_state.temp_path, final_size);
                }
                stream_state.last_known_size = final_size;
            }

            self.status_message =
                Some(t!("stdin.read_complete", bytes = stream_state.last_known_size).to_string());
        }
    }

    /// Check if stdin streaming is active (not complete).
    pub fn is_stdin_streaming(&self) -> bool {
        self.stdin_streaming
            .as_ref()
            .map(|s| !s.complete)
            .unwrap_or(false)
    }

    /// Create a new virtual buffer (not backed by a file)
    ///
    /// # Arguments
    /// * `name` - Display name (e.g., "*Diagnostics*")
    /// * `mode` - Buffer mode for keybindings (e.g., "diagnostics-list")
    /// * `read_only` - Whether the buffer should be read-only
    ///
    /// # Returns
    /// The BufferId of the created virtual buffer
    pub fn create_virtual_buffer(
        &mut self,
        name: String,
        mode: String,
        read_only: bool,
    ) -> BufferId {
        let buffer_id = BufferId(self.next_buffer_id);
        self.next_buffer_id += 1;

        let mut state = EditorState::new(
            self.terminal_width,
            self.terminal_height,
            self.config.editor.large_file_threshold_bytes as usize,
            Arc::clone(&self.filesystem),
        );
        // Note: line_wrap_enabled is set on SplitViewState.viewport when the split is created

        // Set syntax highlighting based on buffer name (e.g., "*OURS*.c" will get C highlighting)
        state.set_language_from_name(&name, &self.grammar_registry);

        // Apply line_numbers default from config
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Set virtual buffer metadata
        let metadata = super::types::BufferMetadata::virtual_buffer(name, mode, read_only);
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add buffer to the active split's open_buffers (tabs)
        let active_split = self.split_manager.active_split();
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                line_wrap,
                self.config.editor.wrap_indent,
                wrap_column,
                self.config.editor.rulers.clone(),
            );
        } else {
            // Create view state if it doesn't exist
            let mut view_state =
                SplitViewState::with_buffer(self.terminal_width, self.terminal_height, buffer_id);
            view_state.apply_config_defaults(
                self.config.editor.line_numbers,
                self.config.editor.highlight_current_line,
                line_wrap,
                self.config.editor.wrap_indent,
                wrap_column,
                self.config.editor.rulers.clone(),
            );
            self.split_view_states.insert(active_split, view_state);
        }

        buffer_id
    }

    /// Set the content of a virtual buffer with text properties
    ///
    /// # Arguments
    /// * `buffer_id` - The virtual buffer to update
    /// * `entries` - Text entries with embedded properties
    pub fn set_virtual_buffer_content(
        &mut self,
        buffer_id: BufferId,
        entries: Vec<crate::primitives::text_property::TextPropertyEntry>,
    ) -> Result<(), String> {
        // Save current cursor position from split view state to preserve it after content update
        let old_cursor_pos = self
            .split_view_states
            .values()
            .find(|vs| vs.has_buffer(buffer_id))
            .and_then(|vs| vs.keyed_states.get(&buffer_id))
            .map(|bs| bs.cursors.primary().position)
            .unwrap_or(0);

        let state = self
            .buffers
            .get_mut(&buffer_id)
            .ok_or_else(|| "Buffer not found".to_string())?;

        // Build text and properties from entries
        let (text, properties, collected_overlays) =
            crate::primitives::text_property::TextPropertyManager::from_entries(entries);

        // Replace buffer content
        // Note: we use buffer.delete_bytes/insert directly (not state.delete_range/insert_text_at)
        // which bypasses marker_list adjustment. Clear ALL overlays first so no stale markers
        // remain pointing at invalid positions in the new content.
        state.overlays.clear(&mut state.marker_list);

        let current_len = state.buffer.len();
        if current_len > 0 {
            state.buffer.delete_bytes(0, current_len);
        }
        state.buffer.insert(0, &text);

        // Clear modified flag since this is virtual buffer content setting, not user edits
        state.buffer.clear_modified();

        // Set text properties
        state.text_properties = properties;

        // Create inline overlays for the new content
        {
            use crate::view::overlay::{Overlay, OverlayFace};
            use fresh_core::overlay::OverlayNamespace;

            let inline_ns = OverlayNamespace::from_string("_inline".to_string());

            for co in collected_overlays {
                let face = OverlayFace::from_options(&co.options);
                let mut overlay = Overlay::with_namespace(
                    &mut state.marker_list,
                    co.range,
                    face,
                    inline_ns.clone(),
                );
                overlay.extend_to_line_end = co.options.extend_to_line_end;
                if let Some(url) = co.options.url {
                    overlay.url = Some(url);
                }
                state.overlays.add(overlay);
            }
        }

        // Preserve cursor position (clamped to new content length and snapped to char boundary)
        let new_len = state.buffer.len();
        let clamped_pos = old_cursor_pos.min(new_len);
        // Ensure cursor is at a valid UTF-8 character boundary (without moving if already valid)
        let new_cursor_pos = state.buffer.snap_to_char_boundary(clamped_pos);

        // Update cursor in the split view state that has this buffer
        for view_state in self.split_view_states.values_mut() {
            if let Some(buf_state) = view_state.keyed_states.get_mut(&buffer_id) {
                buf_state.cursors.primary_mut().position = new_cursor_pos;
                buf_state.cursors.primary_mut().anchor = None;
            }
        }

        Ok(())
    }

    /// Open the built-in help manual in a read-only buffer
    ///
    /// If a help manual buffer already exists, switch to it instead of creating a new one.
    pub fn open_help_manual(&mut self) {
        // Check if help buffer already exists
        let existing_buffer = self
            .buffer_metadata
            .iter()
            .find(|(_, m)| m.display_name == help::HELP_MANUAL_BUFFER_NAME)
            .map(|(id, _)| *id);

        if let Some(buffer_id) = existing_buffer {
            // Switch to existing help buffer
            self.set_active_buffer(buffer_id);
            return;
        }

        // Create new help buffer with "special" mode (has 'q' to close)
        let buffer_id = self.create_virtual_buffer(
            help::HELP_MANUAL_BUFFER_NAME.to_string(),
            "special".to_string(),
            true,
        );

        // Set the content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer.insert(0, help::HELP_MANUAL_CONTENT);
            state.buffer.clear_modified();
            state.editing_disabled = true;

            // Disable line numbers for cleaner display
            state.margins.configure_for_line_numbers(false);
        }

        self.set_active_buffer(buffer_id);
    }

    /// Open the keyboard shortcuts viewer in a read-only buffer
    ///
    /// If a keyboard shortcuts buffer already exists, switch to it instead of creating a new one.
    /// The shortcuts are dynamically generated from the current keybindings configuration.
    pub fn open_keyboard_shortcuts(&mut self) {
        // Check if keyboard shortcuts buffer already exists
        let existing_buffer = self
            .buffer_metadata
            .iter()
            .find(|(_, m)| m.display_name == help::KEYBOARD_SHORTCUTS_BUFFER_NAME)
            .map(|(id, _)| *id);

        if let Some(buffer_id) = existing_buffer {
            // Switch to existing buffer
            self.set_active_buffer(buffer_id);
            return;
        }

        // Get all keybindings
        let bindings = self.keybindings.read().unwrap().get_all_bindings();

        // Format the keybindings as readable text
        let mut content = String::from("Keyboard Shortcuts\n");
        content.push_str("==================\n\n");
        content.push_str("Press 'q' to close this buffer.\n\n");

        // Group bindings by context (Normal, Prompt, etc.)
        let mut current_context = String::new();
        for (key, action) in &bindings {
            // Check if action starts with a context prefix like "[Prompt] "
            let (context, action_name) = if let Some(bracket_end) = action.find("] ") {
                let ctx = &action[1..bracket_end];
                let name = &action[bracket_end + 2..];
                (ctx.to_string(), name.to_string())
            } else {
                ("Normal".to_string(), action.clone())
            };

            // Print context header when it changes
            if context != current_context {
                if !current_context.is_empty() {
                    content.push('\n');
                }
                content.push_str(&format!("── {} Mode ──\n\n", context));
                current_context = context;
            }

            // Format: "  Ctrl+S          Save"
            content.push_str(&format!("  {:20} {}\n", key, action_name));
        }

        // Create new keyboard shortcuts buffer with "special" mode (has 'q' to close)
        let buffer_id = self.create_virtual_buffer(
            help::KEYBOARD_SHORTCUTS_BUFFER_NAME.to_string(),
            "special".to_string(),
            true,
        );

        // Set the content
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer.insert(0, &content);
            state.buffer.clear_modified();
            state.editing_disabled = true;

            // Disable line numbers for cleaner display
            state.margins.configure_for_line_numbers(false);
        }

        self.set_active_buffer(buffer_id);
    }

    /// Show warnings by opening the warning log file directly
    ///
    /// If there are no warnings, shows a brief status message.
    /// Otherwise, opens the warning log file for the user to view.
    pub fn show_warnings_popup(&mut self) {
        if !self.warning_domains.has_any_warnings() {
            self.status_message = Some(t!("warnings.none").to_string());
            return;
        }

        // Open the warning log file directly
        self.open_warning_log();
    }

    /// Show LSP status popup with details about servers active for the current buffer.
    /// Lists each server with its status and provides actions: restart, stop, view log.
    pub fn show_lsp_status_popup(&mut self) {
        // Toggle behavior: if the LSP popup is already showing, close it
        // instead of rebuilding and re-showing it.  This lets clicking the
        // status-bar LSP indicator a second time dismiss the popup, matching
        // the common affordance for status-bar menus.
        if self.pending_lsp_status_popup.is_some() {
            self.hide_popup();
            self.pending_lsp_status_popup = None;
            return;
        }

        let has_error = self.warning_domains.lsp.level() == crate::app::WarningLevel::Error;
        let language = self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
            .unwrap_or_else(|| "unknown".to_string());

        // Compute the set of configured servers whose binaries are not
        // resolvable — plugins and the popup itself both need this to
        // decide between "offer to start" and "offer install help".
        let missing_servers: Vec<String> = self
            .config
            .lsp
            .get(&language)
            .map(|cfg| {
                cfg.as_slice()
                    .iter()
                    .filter(|c| c.enabled && !c.command.is_empty())
                    .filter(|c| !crate::services::lsp::command_exists(&c.command))
                    .map(|c| c.command.clone())
                    .collect()
            })
            .unwrap_or_default();
        let user_dismissed = self.is_lsp_language_user_dismissed(&language);

        // Fire the LspStatusClicked hook for plugins
        self.plugin_manager.run_hook(
            "lsp_status_clicked",
            crate::services::plugins::hooks::HookArgs::LspStatusClicked {
                language: language.clone(),
                has_error,
                missing_servers,
                user_dismissed,
            },
        );

        self.build_and_show_lsp_status_popup(&language);
    }

    /// Rebuild the LSP-status popup in place if it's currently open.
    ///
    /// Used when an async event (progress update, server state change) might
    /// change the popup's contents — notably while rust-analyzer is indexing
    /// and emits `$/progress` every few hundred ms.  Without this, the popup
    /// would freeze on the snapshot taken at open time while the status-bar
    /// spinner keeps moving, making them look disconnected.
    pub fn refresh_lsp_status_popup_if_open(&mut self) {
        if self.pending_lsp_status_popup.is_none() {
            return;
        }
        let language = self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
            .unwrap_or_else(|| "unknown".to_string());
        // Replace contents: hide then rebuild.  hide_popup() clears
        // pending_lsp_status_popup via handle_popup_cancel pathways, but
        // here we're calling it directly without routing through a cancel,
        // so stash and restore the marker so the rebuild sees "already
        // open" and doesn't fall through the toggle branch.
        let was_pending = self.pending_lsp_status_popup.take();
        self.hide_popup();
        drop(was_pending);
        self.build_and_show_lsp_status_popup(&language);
    }

    fn build_and_show_lsp_status_popup(&mut self, language: &str) {
        use crate::services::async_bridge::LspServerStatus;

        // Build a unified list of all configured servers for this language,
        // merged with their runtime status (if running).
        let running_statuses: std::collections::HashMap<String, LspServerStatus> = self
            .lsp_server_statuses
            .iter()
            .filter(|((lang, _), _)| lang == language)
            .map(|((_, name), status)| (name.clone(), *status))
            .collect();

        let configured_servers: Vec<String> = self
            .config
            .lsp
            .get(language)
            .map(|cfg| {
                cfg.as_slice()
                    .iter()
                    .filter(|c| !c.command.is_empty())
                    .map(|c| c.display_name())
                    .collect()
            })
            .unwrap_or_default();

        // Per-server binary availability map (display_name → bool).
        // `command_exists` is cached, so repeated popup opens or a
        // refresh-while-open are cheap.  We look up by display name
        // because `all_servers` below is built from display names;
        // LspServerConfig::display_name() falls back to the command
        // basename when no explicit `name` is set.
        let missing_by_server: std::collections::HashMap<String, bool> = self
            .config
            .lsp
            .get(language)
            .map(|cfg| {
                cfg.as_slice()
                    .iter()
                    .filter(|c| !c.command.is_empty())
                    .map(|c| {
                        (
                            c.display_name(),
                            !crate::services::lsp::command_exists(&c.command),
                        )
                    })
                    .collect()
            })
            .unwrap_or_default();
        let user_dismissed = self.is_lsp_language_user_dismissed(language);

        if configured_servers.is_empty() && running_statuses.is_empty() {
            self.status_message = Some(t!("lsp.no_server_active").to_string());
            return;
        }

        // Merge: start with configured servers, then add any running servers
        // not in the config (shouldn't happen, but be safe).
        let mut all_servers: Vec<String> = configured_servers;
        for name in running_statuses.keys() {
            if !all_servers.contains(name) {
                all_servers.push(name.clone());
            }
        }
        all_servers.sort();

        // Build the popup's items as view-level `PopupListItem`s directly.
        // We bypass the `PopupListItemData` event type here because we need
        // the `disabled` field (for "View Log" when no log exists), which
        // is a view-only concern and plumbing it through the event boundary
        // would require touching ~40 existing literals across the test
        // suite.
        let mut items: Vec<crate::view::popup::PopupListItem> = Vec::new();
        let mut action_keys: Vec<(String, String)> = Vec::new();

        /// Truncate `s` to at most `max_cells` display cells, appending an
        /// ellipsis if truncation happened (the ellipsis is included in the
        /// budget, so the result is ≤ `max_cells` wide regardless of input).
        fn truncate(s: &str, max_cells: usize) -> String {
            use unicode_width::UnicodeWidthChar;
            let w = unicode_width::UnicodeWidthStr::width(s);
            if w <= max_cells {
                return s.to_string();
            }
            let budget = max_cells.saturating_sub(1);
            let mut used = 0;
            let mut out = String::new();
            for ch in s.chars() {
                let cw = ch.width().unwrap_or(0);
                if used + cw > budget {
                    break;
                }
                used += cw;
                out.push(ch);
            }
            out.push('…');
            out
        }
        const PROGRESS_FIELD_MAX: usize = 14;
        const POPUP_WIDTH_MAX: u16 = 50;

        for name in &all_servers {
            let status = running_statuses.get(name).copied();
            let is_active = status
                .map(|s| !matches!(s, LspServerStatus::Shutdown))
                .unwrap_or(false);
            // A server is "missing" only when it's NOT currently running
            // (an absolute-path binary could have been removed mid-session,
            // but the live server is still talking to us).
            let binary_missing =
                !is_active && missing_by_server.get(name).copied().unwrap_or(false);

            // Header: server name + status (data = None → not clickable,
            // not underlined).  Swap the "not running" label for a more
            // actionable "binary not found" when we can see up-front that
            // a start attempt would fail — this is the user-visible half
            // of the pre-click probe.
            let (icon, label) = match status {
                Some(LspServerStatus::Running) => ("●", "ready"),
                Some(LspServerStatus::Error) => ("✗", "error"),
                Some(LspServerStatus::Starting) => ("◌", "starting"),
                Some(LspServerStatus::Initializing) => ("◌", "initializing"),
                Some(LspServerStatus::Shutdown) | None => {
                    if binary_missing {
                        ("○", "binary not in PATH")
                    } else {
                        ("○", "not running")
                    }
                }
            };
            items.push(crate::view::popup::PopupListItem::new(format!(
                "{} {} ({})",
                icon, name, label
            )));

            // Progress row immediately UNDER the server's name row, if
            // there's an active `$/progress` notification for this
            // language.  Indented to match the action rows below, and the
            // title + message fields are individually truncated so a
            // runaway progress path can't stretch the popup.  The popup
            // width is pinned in advance (see below) so the row's content
            // changing never reshapes the popup.
            if let Some(info) = self
                .lsp_progress
                .values()
                .find(|info| info.language == language)
            {
                let mut line = format!("    ⏳ {}", truncate(&info.title, PROGRESS_FIELD_MAX));
                if let Some(ref msg) = info.message {
                    line.push_str(&format!(" · {}", truncate(msg, PROGRESS_FIELD_MAX)));
                }
                if let Some(pct) = info.percentage {
                    line.push_str(&format!(" ({}%)", pct));
                }
                items.push(crate::view::popup::PopupListItem::new(line));
            }

            if is_active {
                // Restart
                let restart_key = format!("restart:{}/{}", language, name);
                items.push(
                    crate::view::popup::PopupListItem::new(format!("    Restart {}", name))
                        .with_data(restart_key.clone()),
                );
                action_keys.push((restart_key, format!("Restart {}", name)));

                // Stop
                let stop_key = format!("stop:{}/{}", language, name);
                items.push(
                    crate::view::popup::PopupListItem::new(format!("    Stop {}", name))
                        .with_data(stop_key.clone()),
                );
                action_keys.push((stop_key, format!("Stop {}", name)));
            } else if binary_missing {
                // Show a disabled advisory row instead of an actionable
                // "Start" — clicking Start here would spawn, fail, and
                // noise up the status area.  The per-language
                // Install/Dismiss actions are added once at the end of
                // the popup, below.
                items.push(
                    crate::view::popup::PopupListItem::new(format!(
                        "    Install {} to enable",
                        name
                    ))
                    .disabled(),
                );
            } else {
                // Start
                let start_key = format!("start:{}", language);
                if !action_keys.iter().any(|(k, _)| k == &start_key) {
                    items.push(
                        crate::view::popup::PopupListItem::new(format!("    Start {}", name))
                            .with_data(start_key.clone()),
                    );
                    action_keys.push((start_key, format!("Start {}", name)));
                }
            }
        }

        // Dismiss / Enable row — shown whenever the language has at
        // least one configured server.  Gives the user a surface to
        // mute the pill (dim style) and, later, to restore it.  We
        // reuse `all_servers.is_empty()` as the "nothing here" signal
        // since languages with zero configured-or-running servers
        // already bailed out above.
        if user_dismissed {
            let enable_key = format!("enable:{}", language);
            items.push(
                crate::view::popup::PopupListItem::new(format!(
                    "    Enable LSP pill for {}",
                    language
                ))
                .with_data(enable_key.clone()),
            );
            action_keys.push((enable_key, format!("Enable LSP for {}", language)));
        } else {
            let dismiss_key = format!("dismiss:{}", language);
            items.push(
                crate::view::popup::PopupListItem::new(format!(
                    "    Disable LSP pill for {}",
                    language
                ))
                .with_data(dismiss_key.clone()),
            );
            action_keys.push((dismiss_key, format!("Disable LSP for {}", language)));
        }

        // View log action (always, at the end) — grayed out and
        // non-actionable when no log file exists yet for this language
        // (e.g. the server was never started, or has been rotated away).
        let log_path = crate::services::log_dirs::lsp_log_path(language);
        let log_exists = log_path.exists();
        let log_key = format!("log:{}", language);
        let mut log_item = crate::view::popup::PopupListItem::new("    View Log".to_string());
        if log_exists {
            log_item = log_item.with_data(log_key.clone());
            action_keys.push((log_key, "View Log".to_string()));
        } else {
            log_item = log_item.disabled();
        }
        items.push(log_item);

        // Store action keys for handling confirmation
        self.pending_lsp_status_popup = Some(action_keys);

        // Pin the popup width up-front, using the *worst-case* widths for
        // any row that varies at runtime (the progress line).  This keeps
        // the popup from jittering when progress messages come and go or
        // change length — the whole point of the spinner + live-refresh
        // pair is that the UI should look stable while the LSP churns.
        //
        //   worst-case progress line =
        //     "    ⏳ " (4-space indent + ⏳ (2 cells) + space = 7 cells)
        //     + PROGRESS_FIELD_MAX   (title)
        //     + " · "                (3 cells)
        //     + PROGRESS_FIELD_MAX   (message)
        //     + " (100%)"            (7 cells)
        //   = 7 + 14 + 3 + 14 + 7 = 45 cells
        const PROGRESS_LINE_MAX: usize = 7 + PROGRESS_FIELD_MAX + 3 + PROGRESS_FIELD_MAX + 7;
        let max_static_item_width = items
            .iter()
            .map(|i| unicode_width::UnicodeWidthStr::width(i.text.as_str()))
            .max()
            .unwrap_or(20);
        let popup_width =
            (max_static_item_width.max(PROGRESS_LINE_MAX) as u16 + 4).clamp(30, POPUP_WIDTH_MAX);

        // Pre-select the first actionable item (skip header items with no
        // data and disabled items like a non-existent View Log).
        let first_actionable = items
            .iter()
            .position(|i| i.data.is_some() && !i.disabled)
            .unwrap_or(0);

        // Left-align the popup's column with the LSP indicator on the
        // status bar, if we know where it was drawn in the last frame.
        // Falls back to the previous BottomRight anchor when the LSP
        // segment isn't visible (e.g. first render).
        let position = self
            .cached_layout
            .status_bar_lsp_area
            .map(
                |(_, col_start, _)| crate::view::popup::PopupPosition::AboveStatusBarAt {
                    x: col_start,
                },
            )
            .unwrap_or(crate::view::popup::PopupPosition::BottomRight);

        use crate::view::popup::{Popup, PopupContent, PopupKind};
        use ratatui::style::Style;

        let popup = Popup {
            kind: PopupKind::List,
            title: Some(format!("LSP Servers ({})", language)),
            description: None,
            transient: false,
            content: PopupContent::List {
                items,
                selected: first_actionable,
            },
            position,
            width: popup_width,
            max_height: 15,
            bordered: true,
            border_style: Style::default().fg(self.theme.popup_border_fg),
            background_style: Style::default().bg(self.theme.popup_bg),
            scroll_offset: 0,
            text_selection: None,
            accept_key_hint: None,
        };

        let buffer_id = self.active_buffer();
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.popups.show(popup);
        }
    }

    /// Show a transient hover popup with the given message text, positioned below the cursor.
    /// Used for file-open messages (e.g. `file.txt:10@"Look at this"`).
    pub fn show_file_message_popup(&mut self, message: &str) {
        use crate::view::popup::{Popup, PopupPosition};
        use ratatui::style::Style;

        // Build markdown: message text + blank line + italic hint
        let md = format!("{}\n\n*esc to dismiss*", message);
        // Size popup width to content: longest line + border padding, clamped to reasonable bounds
        let content_width = message.lines().map(|l| l.len()).max().unwrap_or(0) as u16;
        let hint_width = 16u16; // "*esc to dismiss*"
        let popup_width = (content_width.max(hint_width) + 4).clamp(20, 60);

        let mut popup = Popup::markdown(&md, &self.theme, Some(&self.grammar_registry));
        popup.transient = false;
        popup.position = PopupPosition::BelowCursor;
        popup.width = popup_width;
        popup.max_height = 15;
        popup.border_style = Style::default().fg(self.theme.popup_border_fg);
        popup.background_style = Style::default().bg(self.theme.popup_bg);

        let buffer_id = self.active_buffer();
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.popups.show(popup);
        }
    }

    /// Get text properties at the cursor position in the active buffer
    pub fn get_text_properties_at_cursor(
        &self,
    ) -> Option<Vec<&crate::primitives::text_property::TextProperty>> {
        let state = self.buffers.get(&self.active_buffer())?;
        let cursor_pos = self.active_cursors().primary().position;
        Some(state.text_properties.get_at(cursor_pos))
    }

    /// Close the given buffer
    pub fn close_buffer(&mut self, id: BufferId) -> anyhow::Result<()> {
        // Check for unsaved changes
        if let Some(state) = self.buffers.get(&id) {
            if state.buffer.is_modified() {
                return Err(anyhow::anyhow!("Buffer has unsaved changes"));
            }
        }
        self.close_buffer_internal(id)
    }

    /// Force close the given buffer without checking for unsaved changes
    /// Use this when the user has already confirmed they want to discard changes
    pub fn force_close_buffer(&mut self, id: BufferId) -> anyhow::Result<()> {
        self.close_buffer_internal(id)
    }

    /// Internal helper to close a buffer (shared by close_buffer and force_close_buffer)
    fn close_buffer_internal(&mut self, id: BufferId) -> anyhow::Result<()> {
        // Clear preview tracking if we're closing the current preview buffer.
        // This keeps `preview` from pointing at a freed buffer id.
        if let Some((_, preview_id)) = self.preview {
            if preview_id == id {
                self.preview = None;
            }
        }

        // Complete any --wait tracking for this buffer
        if let Some((wait_id, _)) = self.wait_tracking.remove(&id) {
            self.completed_waits.push(wait_id);
        }

        // Save file state before closing (for per-file session persistence)
        self.save_file_state_on_close(id);

        // Delete recovery data for explicitly closed buffers (including unnamed)
        if let Err(e) = self.delete_buffer_recovery(id) {
            tracing::debug!("Failed to delete buffer recovery on close: {}", e);
        }

        // If closing a terminal buffer, clean up terminal-related data structures
        if let Some(terminal_id) = self.terminal_buffers.remove(&id) {
            // Close the terminal process
            self.terminal_manager.close(terminal_id);

            // Clean up backing/rendering file
            let backing_file = self.terminal_backing_files.remove(&terminal_id);
            if let Some(ref path) = backing_file {
                // Best-effort cleanup of temporary terminal files.
                #[allow(clippy::let_underscore_must_use)]
                let _ = self.filesystem.remove_file(path);
            }
            // Clean up raw log file
            if let Some(log_file) = self.terminal_log_files.remove(&terminal_id) {
                if backing_file.as_ref() != Some(&log_file) {
                    // Best-effort cleanup of temporary terminal files.
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = self.filesystem.remove_file(&log_file);
                }
            }

            // Remove from terminal_mode_resume to prevent stale entries
            self.terminal_mode_resume.remove(&id);

            // Exit terminal mode if we were in it
            if self.terminal_mode {
                self.terminal_mode = false;
                self.key_context = crate::input::keybindings::KeyContext::Normal;
            }
        }

        // Walk the focus-history LRU (most recent first) to find the tab
        // the user should land on. This naturally handles both buffer and
        // group tabs — whichever the user was looking at most recently wins.
        let active_split = self.split_manager.active_split();

        let replacement_target: Option<crate::view::split::TabTarget> =
            self.split_view_states.get(&active_split).and_then(|vs| {
                use crate::view::split::TabTarget;
                vs.focus_history.iter().rev().find_map(|t| match t {
                    TabTarget::Buffer(bid) if *bid == id => None, // skip the closing buffer
                    TabTarget::Buffer(bid) => {
                        // Skip hidden-from-tabs buffers (panel helpers etc.)
                        let hidden = self
                            .buffer_metadata
                            .get(bid)
                            .map(|m| m.hidden_from_tabs)
                            .unwrap_or(false);
                        if hidden || !self.buffers.contains_key(bid) {
                            None
                        } else {
                            Some(*t)
                        }
                    }
                    TabTarget::Group(leaf) => {
                        // Only if the group still exists
                        if self.grouped_subtrees.contains_key(leaf) {
                            Some(*t)
                        } else {
                            None
                        }
                    }
                })
            });

        // Fall back: any visible buffer in the split, then any visible buffer at all.
        let fallback_buffer: Option<BufferId> = if replacement_target.is_none() {
            self.buffers
                .keys()
                .find(|&&bid| {
                    bid != id
                        && !self
                            .buffer_metadata
                            .get(&bid)
                            .map(|m| m.hidden_from_tabs)
                            .unwrap_or(false)
                })
                .copied()
        } else {
            None
        };

        // Capture before the replacement computation — new_buffer() has the
        // side effect of calling set_active_buffer which changes active_buffer().
        let closing_active = self.active_buffer() == id;

        // Determine what to do: activate a group, switch to a buffer, or
        // create a new empty buffer as last resort.
        let return_to_group = match replacement_target {
            Some(crate::view::split::TabTarget::Group(leaf)) => Some(leaf),
            _ => None,
        };
        let replacement_buffer = match replacement_target {
            Some(crate::view::split::TabTarget::Buffer(bid)) => bid,
            Some(crate::view::split::TabTarget::Group(group_leaf)) => {
                // The group's inner panel buffer serves as the split leaf's
                // underlying buffer. The group takes over the active target
                // via activate_group_tab below, so this isn't user-visible.
                self.grouped_subtrees
                    .get(&group_leaf)
                    .and_then(|node| {
                        if let crate::view::split::SplitNode::Grouped {
                            active_inner_leaf, ..
                        } = node
                        {
                            self.split_view_states
                                .get(active_inner_leaf)
                                .map(|vs| vs.active_buffer)
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| fallback_buffer.unwrap_or_else(|| self.new_buffer()))
            }
            None => fallback_buffer.unwrap_or_else(|| self.new_buffer()),
        };

        let created_empty_buffer = replacement_target.is_none() && fallback_buffer.is_none();

        // Switch to replacement buffer BEFORE updating splits.
        // Only needed when the closing buffer is the one the user is
        // looking at — otherwise the current active buffer stays.
        if closing_active {
            self.set_active_buffer(replacement_buffer);

            // When the replacement is a group's hidden inner panel buffer,
            // undo the side effects of set_active_buffer adding it to the
            // host split's tab list and focus history.
            if return_to_group.is_some() {
                if let Some(vs) = self.split_view_states.get_mut(&active_split) {
                    use crate::view::split::TabTarget;
                    vs.open_buffers
                        .retain(|t| *t != TabTarget::Buffer(replacement_buffer));
                    vs.focus_history
                        .retain(|t| *t != TabTarget::Buffer(replacement_buffer));
                }
            }
        }

        // Update all splits that are showing this buffer to show the replacement
        let splits_to_update = self.split_manager.splits_for_buffer(id);
        for split_id in splits_to_update {
            self.split_manager
                .set_split_buffer(split_id, replacement_buffer);
        }

        self.buffers.remove(&id);
        self.event_logs.remove(&id);
        self.seen_byte_ranges.remove(&id);
        self.buffer_metadata.remove(&id);
        if let Some((request_id, _, _)) = self.semantic_tokens_in_flight.remove(&id) {
            self.pending_semantic_token_requests.remove(&request_id);
        }
        if let Some((request_id, _, _, _)) = self.semantic_tokens_range_in_flight.remove(&id) {
            self.pending_semantic_token_range_requests
                .remove(&request_id);
        }
        self.semantic_tokens_range_last_request.remove(&id);
        self.semantic_tokens_range_applied.remove(&id);
        self.semantic_tokens_full_debounce.remove(&id);

        // Remove buffer from panel_ids mapping if it was a panel buffer
        // This prevents stale entries when the same panel_id is reused later
        self.panel_ids.retain(|_, &mut buf_id| buf_id != id);

        // Remove buffer from all splits' open_buffers lists and focus history
        for view_state in self.split_view_states.values_mut() {
            view_state.remove_buffer(id);
            view_state.remove_from_history(id);
        }

        if closing_active {
            if created_empty_buffer {
                self.focus_file_explorer();
            }
            if let Some(group_leaf) = return_to_group {
                self.activate_group_tab(group_leaf);
            }
        }

        Ok(())
    }

    /// Switch to the given buffer
    pub fn switch_buffer(&mut self, id: BufferId) {
        if self.buffers.contains_key(&id) && id != self.active_buffer() {
            // Save current position before switching buffers
            self.position_history.commit_pending_movement();

            // Also explicitly record current position (in case there was no pending movement)
            let cursors = self.active_cursors();
            let position = cursors.primary().position;
            let anchor = cursors.primary().anchor;
            self.position_history
                .record_movement(self.active_buffer(), position, anchor);
            self.position_history.commit_pending_movement();

            self.set_active_buffer(id);
        }
    }

    /// Close the current tab in the current split view.
    /// If the tab is the last viewport of the underlying buffer, do the same as close_buffer
    /// (including triggering the save/discard prompt for modified buffers).
    ///
    /// When the active tab is a buffer group (its `active_group_tab` is set),
    /// this closes the entire group rather than the currently-focused inner
    /// panel buffer. Individual panels are internal details of the group —
    /// the user closes them all together by closing the group tab.
    pub fn close_tab(&mut self) {
        // If the active split has a group tab active, close the whole group
        // rather than just the focused panel buffer.
        let active_split = self.split_manager.active_split();
        if let Some(group_leaf_id) = self
            .split_view_states
            .get(&active_split)
            .and_then(|vs| vs.active_group_tab)
        {
            self.close_buffer_group_by_leaf(group_leaf_id);
            self.set_status_message(t!("buffer.tab_closed").to_string());
            return;
        }

        let buffer_id = self.active_buffer();

        // Count how many splits have this buffer in their open_buffers
        let buffer_in_other_splits = self
            .split_view_states
            .iter()
            .filter(|(&split_id, view_state)| {
                split_id != active_split && view_state.has_buffer(buffer_id)
            })
            .count();

        // Get current split's open buffers
        let current_split_tabs = self
            .split_view_states
            .get(&active_split)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default();

        // If this is the only tab in this split and there are no other splits with this buffer,
        // this is the last viewport - behave like close_buffer
        let is_last_viewport = buffer_in_other_splits == 0;

        if is_last_viewport {
            // If this is the only buffer in this split AND there are other splits,
            // close the split instead of the buffer (don't create an empty replacement)
            let has_other_splits = self.split_manager.root().count_leaves() > 1;
            if current_split_tabs.len() <= 1 && has_other_splits {
                // Check for unsaved changes first
                if self.active_state().buffer.is_modified() {
                    let name = self.get_buffer_display_name(buffer_id);
                    let save_key = t!("prompt.key.save").to_string();
                    let discard_key = t!("prompt.key.discard").to_string();
                    let cancel_key = t!("prompt.key.cancel").to_string();
                    self.start_prompt(
                        t!(
                            "prompt.buffer_modified",
                            name = name,
                            save_key = save_key,
                            discard_key = discard_key,
                            cancel_key = cancel_key
                        )
                        .to_string(),
                        PromptType::ConfirmCloseBuffer { buffer_id },
                    );
                    return;
                }
                // Close the buffer first, then the split
                if let Err(e) = self.close_buffer(buffer_id) {
                    tracing::warn!("Failed to close buffer: {}", e);
                }
                self.close_active_split();
                return;
            }

            // Last viewport of this buffer - close the buffer entirely
            if self.active_state().buffer.is_modified() {
                // Buffer has unsaved changes - prompt for confirmation
                let name = self.get_buffer_display_name(buffer_id);
                let save_key = t!("prompt.key.save").to_string();
                let discard_key = t!("prompt.key.discard").to_string();
                let cancel_key = t!("prompt.key.cancel").to_string();
                self.start_prompt(
                    t!(
                        "prompt.buffer_modified",
                        name = name,
                        save_key = save_key,
                        discard_key = discard_key,
                        cancel_key = cancel_key
                    )
                    .to_string(),
                    PromptType::ConfirmCloseBuffer { buffer_id },
                );
            } else if let Err(e) = self.close_buffer(buffer_id) {
                self.set_status_message(t!("file.cannot_close", error = e.to_string()).to_string());
            } else {
                self.set_status_message(t!("buffer.tab_closed").to_string());
            }
        } else {
            // There are other viewports of this buffer - just remove from current split's tabs
            if current_split_tabs.len() <= 1 {
                // This is the only tab in this split - close the split
                // If we're closing a terminal buffer while in terminal mode, exit terminal mode
                if self.terminal_mode && self.is_terminal_buffer(buffer_id) {
                    self.terminal_mode = false;
                    self.key_context = crate::input::keybindings::KeyContext::Normal;
                }
                self.close_active_split();
                return;
            }

            // Find replacement buffer for this split
            let current_idx = current_split_tabs
                .iter()
                .position(|&id| id == buffer_id)
                .unwrap_or(0);
            let replacement_idx = if current_idx > 0 { current_idx - 1 } else { 1 };
            let replacement_buffer = current_split_tabs[replacement_idx];

            // If we're closing a terminal buffer while in terminal mode, exit terminal mode
            if self.terminal_mode && self.is_terminal_buffer(buffer_id) {
                self.terminal_mode = false;
                self.key_context = crate::input::keybindings::KeyContext::Normal;
            }

            // Remove buffer from this split's tabs
            if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
                view_state.remove_buffer(buffer_id);
            }

            // Update the split to show the replacement buffer
            self.split_manager
                .set_split_buffer(active_split, replacement_buffer);

            self.set_status_message(t!("buffer.tab_closed").to_string());
        }
    }

    /// Close a specific tab (buffer) in a specific split.
    /// Used by mouse click handler on tab close button.
    /// Returns true if the tab was closed without needing a prompt.
    pub fn close_tab_in_split(&mut self, buffer_id: BufferId, split_id: LeafId) -> bool {
        // If closing a terminal buffer while in terminal mode, exit terminal mode
        if self.terminal_mode && self.is_terminal_buffer(buffer_id) {
            self.terminal_mode = false;
            self.key_context = crate::input::keybindings::KeyContext::Normal;
        }

        // Count how many splits have this buffer in their open_buffers
        let buffer_in_other_splits = self
            .split_view_states
            .iter()
            .filter(|(&sid, view_state)| sid != split_id && view_state.has_buffer(buffer_id))
            .count();

        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default();

        let is_last_viewport = buffer_in_other_splits == 0;

        if is_last_viewport {
            // Last viewport of this buffer - need to close buffer entirely
            if let Some(state) = self.buffers.get(&buffer_id) {
                if state.buffer.is_modified() {
                    // Buffer has unsaved changes - prompt for confirmation
                    let name = self.get_buffer_display_name(buffer_id);
                    let save_key = t!("prompt.key.save").to_string();
                    let discard_key = t!("prompt.key.discard").to_string();
                    let cancel_key = t!("prompt.key.cancel").to_string();
                    self.start_prompt(
                        t!(
                            "prompt.buffer_modified",
                            name = name,
                            save_key = save_key,
                            discard_key = discard_key,
                            cancel_key = cancel_key
                        )
                        .to_string(),
                        PromptType::ConfirmCloseBuffer { buffer_id },
                    );
                    return false;
                }
            }
            if let Err(e) = self.close_buffer(buffer_id) {
                self.set_status_message(t!("file.cannot_close", error = e.to_string()).to_string());
            } else {
                self.set_status_message(t!("buffer.tab_closed").to_string());
            }
        } else {
            // There are other viewports of this buffer - just remove from this split's tabs
            if split_tabs.len() <= 1 {
                // This is the only tab in this split - close the split
                self.handle_close_split(split_id.into());
                return true;
            }

            // Find replacement buffer for this split
            let current_idx = split_tabs
                .iter()
                .position(|&id| id == buffer_id)
                .unwrap_or(0);
            let replacement_idx = if current_idx > 0 { current_idx - 1 } else { 1 };
            let replacement_buffer = split_tabs[replacement_idx];

            // Remove buffer from this split's tabs
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                view_state.remove_buffer(buffer_id);
            }

            // Update the split to show the replacement buffer
            self.split_manager
                .set_split_buffer(split_id, replacement_buffer);

            self.set_status_message(t!("buffer.tab_closed").to_string());
        }
        true
    }

    /// Close all other tabs in a split, keeping only the specified buffer
    pub fn close_other_tabs_in_split(&mut self, keep_buffer_id: BufferId, split_id: LeafId) {
        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default();

        // Close all tabs except the one we want to keep
        let tabs_to_close: Vec<_> = split_tabs
            .iter()
            .filter(|&&id| id != keep_buffer_id)
            .copied()
            .collect();

        let mut closed = 0;
        let mut skipped_modified = 0;
        for buffer_id in tabs_to_close {
            if self.close_tab_in_split_silent(buffer_id, split_id) {
                closed += 1;
            } else {
                skipped_modified += 1;
            }
        }

        // Make sure the kept buffer is active
        self.split_manager
            .set_split_buffer(split_id, keep_buffer_id);

        self.set_batch_close_status_message(closed, skipped_modified);
    }

    /// Close tabs to the right of the specified buffer in a split
    pub fn close_tabs_to_right_in_split(&mut self, buffer_id: BufferId, split_id: LeafId) {
        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default();

        // Find the index of the target buffer
        let Some(target_idx) = split_tabs.iter().position(|&id| id == buffer_id) else {
            return;
        };

        // Close all tabs after the target
        let tabs_to_close: Vec<_> = split_tabs.iter().skip(target_idx + 1).copied().collect();

        let mut closed = 0;
        let mut skipped_modified = 0;
        for buf_id in tabs_to_close {
            if self.close_tab_in_split_silent(buf_id, split_id) {
                closed += 1;
            } else {
                skipped_modified += 1;
            }
        }

        self.set_batch_close_status_message(closed, skipped_modified);
    }

    /// Close tabs to the left of the specified buffer in a split
    pub fn close_tabs_to_left_in_split(&mut self, buffer_id: BufferId, split_id: LeafId) {
        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default();

        // Find the index of the target buffer
        let Some(target_idx) = split_tabs.iter().position(|&id| id == buffer_id) else {
            return;
        };

        // Close all tabs before the target
        let tabs_to_close: Vec<_> = split_tabs.iter().take(target_idx).copied().collect();

        let mut closed = 0;
        let mut skipped_modified = 0;
        for buf_id in tabs_to_close {
            if self.close_tab_in_split_silent(buf_id, split_id) {
                closed += 1;
            } else {
                skipped_modified += 1;
            }
        }

        self.set_batch_close_status_message(closed, skipped_modified);
    }

    /// Close all tabs in a split
    pub fn close_all_tabs_in_split(&mut self, split_id: LeafId) {
        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default();

        let mut closed = 0;
        let mut skipped_modified = 0;

        // Close all tabs (this will eventually close the split when empty)
        for buffer_id in split_tabs {
            if self.close_tab_in_split_silent(buffer_id, split_id) {
                closed += 1;
            } else {
                skipped_modified += 1;
            }
        }

        self.set_batch_close_status_message(closed, skipped_modified);
    }

    /// Set status message for batch close operations
    fn set_batch_close_status_message(&mut self, closed: usize, skipped_modified: usize) {
        let message = match (closed, skipped_modified) {
            (0, 0) => t!("buffer.no_tabs_to_close").to_string(),
            (0, n) => t!("buffer.skipped_modified", count = n).to_string(),
            (n, 0) => t!("buffer.closed_tabs", count = n).to_string(),
            (c, s) => t!("buffer.closed_tabs_skipped", closed = c, skipped = s).to_string(),
        };
        self.set_status_message(message);
    }

    /// Close a tab silently (without setting status message)
    /// Used internally by batch close operations
    /// Returns true if the tab was closed, false if it was skipped (e.g., modified buffer)
    fn close_tab_in_split_silent(&mut self, buffer_id: BufferId, split_id: LeafId) -> bool {
        // If closing a terminal buffer while in terminal mode, exit terminal mode
        if self.terminal_mode && self.is_terminal_buffer(buffer_id) {
            self.terminal_mode = false;
            self.key_context = crate::input::keybindings::KeyContext::Normal;
        }

        // Count how many splits have this buffer in their open_buffers
        let buffer_in_other_splits = self
            .split_view_states
            .iter()
            .filter(|(&sid, view_state)| sid != split_id && view_state.has_buffer(buffer_id))
            .count();

        // Get the split's open buffers
        let split_tabs = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.buffer_tab_ids_vec())
            .unwrap_or_default();

        let is_last_viewport = buffer_in_other_splits == 0;

        if is_last_viewport {
            // Last viewport of this buffer - need to close buffer entirely
            // Skip modified buffers to avoid prompting during batch operations
            if let Some(state) = self.buffers.get(&buffer_id) {
                if state.buffer.is_modified() {
                    // Skip modified buffers - don't close them
                    return false;
                }
            }
            if let Err(e) = self.close_buffer(buffer_id) {
                tracing::warn!("Failed to close buffer: {}", e);
            }
            true
        } else {
            // There are other viewports of this buffer - just remove from this split's tabs
            if split_tabs.len() <= 1 {
                // This is the only tab in this split - close the split
                self.handle_close_split(split_id.into());
                return true;
            }

            // Find replacement buffer for this split
            let current_idx = split_tabs
                .iter()
                .position(|&id| id == buffer_id)
                .unwrap_or(0);
            let replacement_idx = if current_idx > 0 { current_idx - 1 } else { 1 };
            let replacement_buffer = split_tabs.get(replacement_idx).copied();

            // Remove buffer from this split's tabs
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                view_state.remove_buffer(buffer_id);
            }

            // Update the split to show the replacement buffer
            if let Some(replacement) = replacement_buffer {
                self.split_manager.set_split_buffer(split_id, replacement);
            }
            true
        }
    }

    /// Switch to next buffer in current split's tabs
    pub fn next_buffer(&mut self) {
        self.cycle_tab(1);
    }

    /// Switch to previous buffer in current split's tabs
    pub fn prev_buffer(&mut self) {
        self.cycle_tab(-1);
    }

    /// Cycle through the active split's tab targets (buffers AND groups).
    /// Direction: +1 = next, -1 = previous.
    fn cycle_tab(&mut self, direction: i32) {
        use crate::view::split::TabTarget;

        let active_split = self.split_manager.active_split();
        let Some(view_state) = self.split_view_states.get(&active_split) else {
            return;
        };

        // Collect visible tab targets, filtering out hidden buffers.
        let targets: Vec<TabTarget> = view_state
            .open_buffers
            .iter()
            .copied()
            .filter(|t| match t {
                TabTarget::Buffer(id) => !self
                    .buffer_metadata
                    .get(id)
                    .map(|m| m.hidden_from_tabs)
                    .unwrap_or(false),
                TabTarget::Group(_) => true,
            })
            .collect();

        if targets.len() < 2 {
            return;
        }

        let current_target = view_state.active_target();
        let Some(idx) = targets.iter().position(|t| *t == current_target) else {
            return;
        };

        let next_idx = if direction > 0 {
            (idx + 1) % targets.len()
        } else if idx == 0 {
            targets.len() - 1
        } else {
            idx - 1
        };

        if targets[next_idx] == current_target {
            return;
        }

        // Save current position before switching
        self.position_history.commit_pending_movement();
        let cursors = self.active_cursors();
        let position = cursors.primary().position;
        let anchor = cursors.primary().anchor;
        self.position_history
            .record_movement(self.active_buffer(), position, anchor);
        self.position_history.commit_pending_movement();

        match targets[next_idx] {
            TabTarget::Buffer(buffer_id) => {
                self.set_active_buffer(buffer_id);
            }
            TabTarget::Group(group_leaf_id) => {
                self.activate_group_tab(group_leaf_id);
            }
        }
    }

    /// Navigate back in position history
    pub fn navigate_back(&mut self) {
        // Set flag to prevent recording this navigation movement
        self.in_navigation = true;

        // Commit any pending movement
        self.position_history.commit_pending_movement();

        // If we're at the end of history (haven't used back yet), save current position
        // so we can navigate forward to it later
        if self.position_history.can_go_back() && !self.position_history.can_go_forward() {
            let cursors = self.active_cursors();
            let position = cursors.primary().position;
            let anchor = cursors.primary().anchor;
            self.position_history
                .record_movement(self.active_buffer(), position, anchor);
            self.position_history.commit_pending_movement();
        }

        // Navigate to the previous position
        if let Some(entry) = self.position_history.back() {
            let target_buffer = entry.buffer_id;
            let target_position = entry.position;
            let target_anchor = entry.anchor;

            // Switch to the target buffer
            if self.buffers.contains_key(&target_buffer) {
                self.set_active_buffer(target_buffer);

                // Move cursor to the saved position
                let cursors = self.active_cursors();
                let cursor_id = cursors.primary_id();
                let old_position = cursors.primary().position;
                let old_anchor = cursors.primary().anchor;
                let old_sticky_column = cursors.primary().sticky_column;
                let event = Event::MoveCursor {
                    cursor_id,
                    old_position,
                    new_position: target_position,
                    old_anchor,
                    new_anchor: target_anchor,
                    old_sticky_column,
                    new_sticky_column: 0, // Reset sticky column for navigation
                };
                let split_id = self.split_manager.active_split();
                let state = self.buffers.get_mut(&target_buffer).unwrap();
                let view_state = self.split_view_states.get_mut(&split_id).unwrap();
                state.apply(&mut view_state.cursors, &event);
            }
        }

        // Clear the flag
        self.in_navigation = false;
    }

    /// Navigate forward in position history
    pub fn navigate_forward(&mut self) {
        // Set flag to prevent recording this navigation movement
        self.in_navigation = true;

        if let Some(entry) = self.position_history.forward() {
            let target_buffer = entry.buffer_id;
            let target_position = entry.position;
            let target_anchor = entry.anchor;

            // Switch to the target buffer
            if self.buffers.contains_key(&target_buffer) {
                self.set_active_buffer(target_buffer);

                // Move cursor to the saved position
                let cursors = self.active_cursors();
                let cursor_id = cursors.primary_id();
                let old_position = cursors.primary().position;
                let old_anchor = cursors.primary().anchor;
                let old_sticky_column = cursors.primary().sticky_column;
                let event = Event::MoveCursor {
                    cursor_id,
                    old_position,
                    new_position: target_position,
                    old_anchor,
                    new_anchor: target_anchor,
                    old_sticky_column,
                    new_sticky_column: 0, // Reset sticky column for navigation
                };
                let split_id = self.split_manager.active_split();
                let state = self.buffers.get_mut(&target_buffer).unwrap();
                let view_state = self.split_view_states.get_mut(&split_id).unwrap();
                state.apply(&mut view_state.cursors, &event);
            }
        }

        // Clear the flag
        self.in_navigation = false;
    }

    /// Get the current mouse hover state for testing
    /// Returns Some((byte_position, screen_x, screen_y)) if hovering over text
    pub fn get_mouse_hover_state(&self) -> Option<(usize, u16, u16)> {
        self.mouse_state
            .lsp_hover_state
            .map(|(pos, _, x, y)| (pos, x, y))
    }

    /// Check if a transient popup (hover/signature help) is currently visible
    pub fn has_transient_popup(&self) -> bool {
        self.active_state()
            .popups
            .top()
            .is_some_and(|p| p.transient)
    }

    /// Force check the mouse hover timer (for testing)
    /// This bypasses the normal 500ms delay
    pub fn force_check_mouse_hover(&mut self) -> bool {
        if let Some((byte_pos, _, screen_x, screen_y)) = self.mouse_state.lsp_hover_state {
            if !self.mouse_state.lsp_hover_request_sent {
                self.mouse_hover_screen_position = Some((screen_x, screen_y));
                match self.request_hover_at_position(byte_pos) {
                    Ok(true) => {
                        self.mouse_state.lsp_hover_request_sent = true;
                        return true;
                    }
                    Ok(false) => return false, // no server ready, retry later
                    Err(e) => {
                        tracing::debug!("Failed to request hover: {}", e);
                        return false;
                    }
                }
            }
        }
        false
    }

    /// Queue a file to be opened after the TUI starts.
    ///
    /// This is used for CLI file arguments to ensure they go through the same
    /// code path as interactive file opens, providing consistent error handling
    /// (e.g., encoding confirmation prompts are shown in the UI instead of crashing).
    /// Schedule hot exit recovery to run after the next batch of pending file opens.
    pub fn schedule_hot_exit_recovery(&mut self) {
        if self.config.editor.hot_exit {
            self.pending_hot_exit_recovery = true;
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn queue_file_open(
        &mut self,
        path: PathBuf,
        line: Option<usize>,
        column: Option<usize>,
        end_line: Option<usize>,
        end_column: Option<usize>,
        message: Option<String>,
        wait_id: Option<u64>,
    ) {
        self.pending_file_opens.push(super::PendingFileOpen {
            path,
            line,
            column,
            end_line,
            end_column,
            message,
            wait_id,
        });
    }

    /// Process pending file opens (called from the event loop).
    ///
    /// Opens files that were queued during startup, using the same error handling
    /// as interactive file opens. Returns true if any files were processed.
    pub fn process_pending_file_opens(&mut self) -> bool {
        if self.pending_file_opens.is_empty() {
            return false;
        }

        // Take all pending files to process
        let pending = std::mem::take(&mut self.pending_file_opens);
        let mut processed_any = false;

        for pending_file in pending {
            tracing::info!(
                "[SYNTAX DEBUG] Processing pending file open: {:?}",
                pending_file.path
            );

            match self.open_file(&pending_file.path) {
                Ok(_) => {
                    // Navigate to line/column or select range if specified
                    if let (Some(line), Some(end_line)) = (pending_file.line, pending_file.end_line)
                    {
                        self.select_range(
                            line,
                            pending_file.column,
                            end_line,
                            pending_file.end_column,
                        );
                    } else if let Some(line) = pending_file.line {
                        self.goto_line_col(line, pending_file.column);
                    }
                    // Show hover message popup if specified
                    let has_popup = pending_file.message.is_some();
                    if let Some(ref msg) = pending_file.message {
                        self.show_file_message_popup(msg);
                    }
                    // Track wait ID for --wait support
                    if let Some(wait_id) = pending_file.wait_id {
                        let buffer_id = self.active_buffer();
                        self.wait_tracking.insert(buffer_id, (wait_id, has_popup));
                    }
                    processed_any = true;
                }
                Err(e) => {
                    // Check if this is a large file encoding confirmation error
                    // Show prompt instead of crashing
                    if let Some(confirmation) =
                        e.downcast_ref::<crate::model::buffer::LargeFileEncodingConfirmation>()
                    {
                        self.start_large_file_encoding_confirmation(confirmation);
                    } else {
                        // For other errors, show status message (consistent with file browser)
                        self.set_status_message(
                            t!("file.error_opening", error = e.to_string()).to_string(),
                        );
                    }
                    processed_any = true;
                }
            }
        }

        // Apply hot exit recovery if flagged (one-shot after CLI files are opened)
        if processed_any && self.pending_hot_exit_recovery {
            self.pending_hot_exit_recovery = false;
            match self.apply_hot_exit_recovery() {
                Ok(count) if count > 0 => {
                    tracing::info!("Hot exit: restored unsaved changes for {} buffer(s)", count);
                }
                Ok(_) => {}
                Err(e) => {
                    tracing::warn!("Failed to apply hot exit recovery: {}", e);
                }
            }
        }

        processed_any
    }

    /// Take and return completed wait IDs (for --wait support).
    pub fn take_completed_waits(&mut self) -> Vec<u64> {
        std::mem::take(&mut self.completed_waits)
    }

    /// Remove wait tracking for a given wait_id (e.g., when waiting client disconnects).
    pub fn remove_wait_tracking(&mut self, wait_id: u64) {
        self.wait_tracking.retain(|_, (wid, _)| *wid != wait_id);
    }

    /// Start an incremental line-feed scan for the active buffer.
    ///
    /// Shared by the `Action::ScanLineIndex` command and the Go to Line scan
    /// confirmation prompt. Sets up `LineScanState` so that `process_line_scan`
    /// will advance the scan one batch per frame.
    ///
    /// When `open_goto_line` is true (Go to Line flow), the Go to Line prompt
    /// opens automatically when the scan completes.
    pub fn start_incremental_line_scan(&mut self, open_goto_line: bool) {
        let buffer_id = self.active_buffer();
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let (chunks, total_bytes) = state.buffer.prepare_line_scan();
            let leaves = state.buffer.piece_tree_leaves();
            self.line_scan_state = Some(super::LineScanState {
                buffer_id,
                leaves,
                chunks,
                next_chunk: 0,
                total_bytes,
                scanned_bytes: 0,
                updates: Vec::new(),
                open_goto_line_on_complete: open_goto_line,
            });
            self.set_status_message(t!("goto.scanning_progress", percent = 0).to_string());
        }
    }

    /// Process chunks for the incremental line-feed scan.
    /// Returns `true` if the UI should re-render (progress updated or scan finished).
    pub fn process_line_scan(&mut self) -> bool {
        let _span = tracing::info_span!("process_line_scan").entered();
        let scan = match self.line_scan_state.as_mut() {
            Some(s) => s,
            None => return false,
        };

        let buffer_id = scan.buffer_id;

        if let Err(e) = self.process_line_scan_batch(buffer_id) {
            tracing::warn!("Line scan error: {e}");
            self.finish_line_scan_with_error(e);
            return true;
        }

        let scan = self.line_scan_state.as_ref().unwrap();
        if scan.next_chunk >= scan.chunks.len() {
            self.finish_line_scan_ok();
        } else {
            let pct = if scan.total_bytes > 0 {
                (scan.scanned_bytes * 100) / scan.total_bytes
            } else {
                100
            };
            self.set_status_message(t!("goto.scanning_progress", percent = pct).to_string());
        }
        true
    }

    /// Process leaves concurrently, yielding for a render after each batch.
    ///
    /// For loaded leaves, delegates to `TextBuffer::scan_leaf` (shared counting
    /// logic). For unloaded leaves, extracts I/O parameters and runs them
    /// concurrently using `tokio::task::spawn_blocking` — each task calls
    /// `count_line_feeds_in_range` on the filesystem, which remote implementations
    /// override to count on the server without transferring data.
    fn process_line_scan_batch(&mut self, buffer_id: BufferId) -> std::io::Result<()> {
        let _span = tracing::info_span!("process_line_scan_batch").entered();
        let concurrency = self.config.editor.read_concurrency.max(1);

        let state = self.buffers.get(&buffer_id);
        let scan = self.line_scan_state.as_mut().unwrap();

        let mut results: Vec<(usize, usize)> = Vec::new();
        let mut io_work: Vec<(usize, std::path::PathBuf, u64, usize)> = Vec::new();

        while scan.next_chunk < scan.chunks.len() && (results.len() + io_work.len()) < concurrency {
            let chunk = scan.chunks[scan.next_chunk].clone();
            scan.next_chunk += 1;
            scan.scanned_bytes += chunk.byte_len;

            if chunk.already_known {
                continue;
            }

            if let Some(state) = state {
                let leaf = &scan.leaves[chunk.leaf_index];

                // Use scan_leaf for loaded buffers (shared counting logic with
                // the TextBuffer-level scan). For unloaded buffers, collect I/O
                // parameters for concurrent filesystem access.
                match state.buffer.leaf_io_params(leaf) {
                    None => {
                        // Loaded: count in-memory via scan_leaf
                        let count = state.buffer.scan_leaf(leaf)?;
                        results.push((chunk.leaf_index, count));
                    }
                    Some((path, offset, len)) => {
                        // Unloaded: batch for concurrent I/O
                        io_work.push((chunk.leaf_index, path, offset, len));
                    }
                }
            }
        }

        // Run I/O concurrently using tokio::task::spawn_blocking
        if !io_work.is_empty() {
            let fs = match state {
                Some(s) => s.buffer.filesystem().clone(),
                None => return Ok(()),
            };

            let rt = self
                .tokio_runtime
                .as_ref()
                .ok_or_else(|| std::io::Error::other("async runtime not available"))?;

            let io_results: Vec<std::io::Result<(usize, usize)>> = rt.block_on(async {
                let mut handles = Vec::with_capacity(io_work.len());
                for (leaf_idx, path, offset, len) in io_work {
                    let fs = fs.clone();
                    handles.push(tokio::task::spawn_blocking(move || {
                        let count = fs.count_line_feeds_in_range(&path, offset, len)?;
                        Ok((leaf_idx, count))
                    }));
                }

                let mut results = Vec::with_capacity(handles.len());
                for handle in handles {
                    results.push(handle.await.unwrap());
                }
                results
            });

            for result in io_results {
                results.push(result?);
            }
        }

        for (leaf_idx, count) in results {
            scan.updates.push((leaf_idx, count));
        }

        Ok(())
    }

    fn finish_line_scan_ok(&mut self) {
        let _span = tracing::info_span!("finish_line_scan_ok").entered();
        let scan = self.line_scan_state.take().unwrap();
        let open_goto = scan.open_goto_line_on_complete;
        if let Some(state) = self.buffers.get_mut(&scan.buffer_id) {
            let _span = tracing::info_span!(
                "rebuild_with_pristine_saved_root",
                updates = scan.updates.len()
            )
            .entered();
            state.buffer.rebuild_with_pristine_saved_root(&scan.updates);
        }
        self.set_status_message(t!("goto.scan_complete").to_string());
        if open_goto {
            self.open_goto_line_if_active(scan.buffer_id);
        }
    }

    fn finish_line_scan_with_error(&mut self, e: std::io::Error) {
        let scan = self.line_scan_state.take().unwrap();
        let open_goto = scan.open_goto_line_on_complete;
        self.set_status_message(t!("goto.scan_failed", error = e.to_string()).to_string());
        if open_goto {
            self.open_goto_line_if_active(scan.buffer_id);
        }
    }

    fn open_goto_line_if_active(&mut self, buffer_id: BufferId) {
        if self.active_buffer() == buffer_id {
            self.start_prompt(
                t!("file.goto_line_prompt").to_string(),
                PromptType::GotoLine,
            );
        }
    }

    // === Incremental Search Scan (for large files) ===

    /// Process chunks for the incremental search scan.
    /// Returns `true` if the UI should re-render (progress updated or scan finished).
    pub fn process_search_scan(&mut self) -> bool {
        let scan = match self.search_scan_state.as_mut() {
            Some(s) => s,
            None => return false,
        };

        let buffer_id = scan.buffer_id;

        if let Err(e) = self.process_search_scan_batch(buffer_id) {
            tracing::warn!("Search scan error: {e}");
            let _scan = self.search_scan_state.take().unwrap();
            self.set_status_message(format!("Search failed: {e}"));
            return true;
        }

        let scan = self.search_scan_state.as_ref().unwrap();
        if scan.scan.is_done() {
            self.finish_search_scan();
        } else {
            let pct = scan.scan.progress_percent();
            let match_count = scan.scan.matches.len();
            self.set_status_message(format!(
                "Searching... {}% ({} matches so far)",
                pct, match_count
            ));
        }
        true
    }

    /// Process a batch of search chunks by delegating to
    /// `TextBuffer::search_scan_next_chunk`.
    fn process_search_scan_batch(
        &mut self,
        buffer_id: crate::model::event::BufferId,
    ) -> std::io::Result<()> {
        let concurrency = self.config.editor.read_concurrency.max(1);

        for _ in 0..concurrency {
            let is_done = {
                let scan_state = match self.search_scan_state.as_ref() {
                    Some(s) => s,
                    None => return Ok(()),
                };
                scan_state.scan.is_done()
            };
            if is_done {
                break;
            }

            // Extract the ChunkedSearchState, run one chunk on the buffer,
            // then put it back.  This avoids double-mutable-borrow of self.
            let mut scan = self.search_scan_state.take().unwrap();
            let result = if let Some(state) = self.buffers.get_mut(&buffer_id) {
                state.buffer.search_scan_next_chunk(&mut scan.scan)
            } else {
                Ok(false)
            };
            self.search_scan_state = Some(scan);

            match result {
                Ok(false) => break, // scan complete
                Ok(true) => {}      // more chunks
                Err(e) => return Err(e),
            }
        }

        Ok(())
    }

    /// Finalize the incremental search scan: take the accumulated matches
    /// and hand them to `finalize_search()` which sets search_state, moves
    /// the cursor, and creates viewport overlays.
    fn finish_search_scan(&mut self) {
        let scan = self.search_scan_state.take().unwrap();
        let buffer_id = scan.buffer_id;
        let match_ranges: Vec<(usize, usize)> = scan
            .scan
            .matches
            .iter()
            .map(|m| (m.byte_offset, m.length))
            .collect();
        let capped = scan.scan.capped;
        let query = scan.query;

        // The search scan loaded chunks via chunk_split_and_load, which
        // restructures the piece tree.  Refresh saved_root so that
        // diff_since_saved() can take the fast Arc::ptr_eq path.
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer.refresh_saved_root_if_unmodified();
        }

        if match_ranges.is_empty() {
            self.search_state = None;
            self.set_status_message(format!("No matches found for '{}'", query));
            return;
        }

        self.finalize_search(&query, match_ranges, capped, None);
    }
}
