//! File-open orchestrators on `Editor`.
//!
//! The `open_file` family — open_file, open_file_no_focus,
//! open_local_file, open_file_with_encoding, reload_with_encoding,
//! open_file_large_encoding_confirmed — and supporting helpers
//! restore_global_file_state and save_file_state_on_close.
//!
//! Opening a file in this editor coordinates: detecting the file type,
//! choosing or creating a buffer, registering with the LSP, parsing
//! grammar, restoring per-file UI state (cursor position, scroll), and
//! deciding which split to focus. Each variant differs only in how it
//! handles encoding errors, focus, and "no file at this path yet" cases.

use std::path::Path;
use std::sync::Arc;

use rust_i18n::t;

use crate::model::event::BufferId;
use crate::state::EditorState;

use super::Editor;

/// How a file open treats the resulting buffer.
///
/// Threaded through the open path so the single `after_file_open` fire site
/// in [`open_file_no_focus_inner`] can defer the hook for previews. A
/// `Preview` open is "just looking" (file-explorer browse, live-grep
/// overlay): the hook is withheld until the preview is escalated to a
/// permanent buffer (see the `promote_*` methods on `Window`). `Commit` is
/// every deliberate open and fires the hook immediately. This replaces an
/// earlier ambient `opening_as_preview` flag — the intent now travels as a
/// value rather than mutable window state that could be read out of band.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum OpenKind {
    /// Deliberate open — fire `after_file_open` now.
    Commit,
    /// Ephemeral preview — defer `after_file_open` until escalation.
    Preview,
}

impl Editor {
    /// Helper to jump to a line/column position in the active buffer.
    ///
    /// Lives here (not in the plugin-gated command module) so non-plugin
    /// callers — e.g. Ctrl+Click-to-open from the terminal — can reach it in
    /// builds compiled without the `plugins` feature.
    pub(crate) fn jump_to_line_column(&mut self, line: Option<usize>, column: Option<usize>) {
        // Convert 1-indexed line/column to byte position
        let target_line = line.unwrap_or(1).saturating_sub(1); // Convert to 0-indexed
        let column_offset = column.unwrap_or(1).saturating_sub(1); // Convert to 0-indexed

        let state = self.active_state_mut();
        let mut iter = state.buffer.line_iterator(0, 80);
        let mut target_byte = 0;

        // Iterate through lines until we reach the target
        for current_line in 0..=target_line {
            if let Some((line_start, _)) = iter.next_line() {
                if current_line == target_line {
                    target_byte = line_start;
                    break;
                }
            } else {
                // Reached end of buffer before target line
                break;
            }
        }

        // Add the column offset to position within the line
        // Column offset is byte offset from line start (matching git grep --column behavior)
        let final_position = target_byte + column_offset;

        // Ensure we don't go past the buffer end
        let buffer_len = state.buffer.len();
        let clamped_position = final_position.min(buffer_len);

        // Update the cached line number so the status bar shows the correct
        // position. Without this, the status bar reads a stale value from
        // state.primary_cursor_line_number which was set before the jump.
        state.primary_cursor_line_number = crate::model::buffer::LineNumber::Absolute(target_line);

        // Funnel through the navigation primitive so the cursor is guaranteed
        // visible in the viewport (#1689 — without this, jump_to_line_column
        // could land off-screen if a prior scroll set skip_ensure_visible).
        self.active_window_mut().jump_active_cursor_to(
            clamped_position,
            super::navigation::JumpOptions::navigation(),
        );
    }

    /// Open a file (switching to an already-open buffer if any) and jump to the
    /// given 1-based line/column if specified. Used by the OpenFileAtLocation
    /// plugin command and by Ctrl+Click-to-open from the terminal.
    pub(crate) fn handle_open_file_at_location(
        &mut self,
        path: std::path::PathBuf,
        line: Option<usize>,
        column: Option<usize>,
    ) -> anyhow::Result<()> {
        // Open the file (may switch to an already-open buffer)
        if let Err(e) = self.open_file(&path) {
            tracing::error!("Failed to open file at location: {}", e);
            return Ok(());
        }

        // If line/column specified, jump to that location
        if line.is_some() || column.is_some() {
            self.jump_to_line_column(line, column);
        }
        Ok(())
    }

    /// Open a file and return its buffer ID
    ///
    /// If the file doesn't exist, creates an unsaved buffer with that filename.
    /// Saving the buffer will create the file.
    pub fn open_file(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        self.open_file_with_kind(path, OpenKind::Commit)
    }

    /// `open_file` with explicit [`OpenKind`]. `open_file_preview` uses
    /// `OpenKind::Preview` to route through all of `open_file`'s
    /// cross-cutting concerns (focus, language detection, status, the
    /// `buffer_activated` hook) while deferring only `after_file_open`.
    pub(crate) fn open_file_with_kind(
        &mut self,
        path: &Path,
        kind: OpenKind,
    ) -> anyhow::Result<BufferId> {
        // If the active leaf is a utility-dock pane (Search/Replace,
        // Quickfix, terminal-in-dock), the user almost never wants the
        // newly-opened file to land there — the dock hosts panel-style
        // content, not editor buffers. Snap the active leaf back to
        // the most recent regular editor leaf BEFORE the open path
        // runs, so both downstream routing decisions —
        // `preferred_split_for_file` (which adds the new buffer as a
        // tab) and `set_active_buffer` (which makes it the active
        // buffer) — see a non-dock active leaf and route consistently.
        self.active_window_mut()
            .redirect_active_split_away_from_dock_if_needed();

        // Check whether the active buffer had a file path before loading.
        // If it didn't, open_file_no_focus may replace the empty initial buffer
        // in-place (same buffer ID, new content), and we need to notify plugins.
        let active_had_path = self
            .buffers()
            .get(&self.active_buffer())
            .and_then(|s| s.buffer.file_path())
            .is_some();

        let buffer_id = self
            .active_window_mut()
            .open_file_no_focus_with_kind(path, kind)?;

        // Check if this was an already-open buffer or a new one
        // For already-open buffers, just switch to them
        // For new buffers, record position history before switching
        let is_new_buffer = self.active_buffer() != buffer_id;

        if is_new_buffer && !self.active_window().suppress_position_history_once {
            // Save current position before switching to new buffer
            self.active_window_mut()
                .position_history
                .commit_pending_movement();

            // Explicitly record current position before switching
            let cursors = self.active_cursors();
            let position = cursors.primary().position;
            let anchor = cursors.primary().anchor;
            let active_buffer_id = self.active_buffer();
            let ph = &mut self.active_window_mut().position_history;
            ph.record_movement(active_buffer_id, position, anchor);
            ph.commit_pending_movement();
        }

        self.set_active_buffer(buffer_id);

        // Opening a file focuses a buffer in the active split. If a
        // *different* split is maximized (most commonly the docked
        // terminal), the renderer shows only the maximized split, so the
        // freshly-focused buffer would be invisible. Restore the layout so
        // the user actually sees the file they just opened.
        self.reveal_active_split_if_hidden_by_maximize();

        // If the initial empty buffer was replaced in-place with file content,
        // set_active_buffer is a no-op (same buffer ID). Fire buffer_activated
        // explicitly so plugins see the newly loaded file.
        // Skip this when re-opening an already-active file (active_had_path),
        // as nothing changed and the extra hook would cause spurious refreshes
        // in plugins like the diagnostics panel.
        if !is_new_buffer && !active_had_path {
            #[cfg(feature = "plugins")]
            self.update_plugin_state_snapshot();

            self.plugin_manager.read().unwrap().run_hook(
                "buffer_activated",
                crate::services::plugins::hooks::HookArgs::BufferActivated { buffer_id },
            );
        }

        // Use display_name from metadata for relative path display
        let display_name = self
            .active_window()
            .buffer_metadata
            .get(&buffer_id)
            .map(|m| m.display_name.clone())
            .unwrap_or_else(|| path.display().to_string());

        // Check if buffer is binary for status message
        let is_binary = self
            .buffers()
            .get(&buffer_id)
            .map(|s| s.buffer.is_binary())
            .unwrap_or(false);

        // Show appropriate status message for binary vs regular files
        if is_binary {
            self.active_window_mut().status_message =
                Some(t!("buffer.opened_binary", name = display_name).to_string());
        } else {
            self.active_window_mut().status_message =
                Some(t!("buffer.opened", name = display_name).to_string());
        }

        Ok(buffer_id)
    }

    /// Restore the split layout when the just-focused buffer would be
    /// hidden behind a maximized split.
    ///
    /// `SplitManager::get_visible_buffers` renders *only* the maximized
    /// split. A file open focuses its buffer in the active split, which —
    /// after `redirect_active_split_away_from_dock_if_needed` — is a
    /// regular editor leaf, not the maximized dock. With nothing reset, the
    /// new buffer renders behind the maximized terminal: the user sees no
    /// change, and an embedded `fresh <file>` that forwarded the open
    /// blocks waiting for that invisible buffer to be closed, so the
    /// terminal appears to hang. Un-maximize so the focused buffer shows.
    fn reveal_active_split_if_hidden_by_maximize(&mut self) {
        let mgr = self.split_manager();
        let active: crate::model::event::SplitId = mgr.active_split().into();
        let hidden = matches!(mgr.maximized_split(), Some(maximized) if maximized != active);
        if !hidden {
            return;
        }
        // `unmaximize_split` only errors when nothing is maximized, which
        // the `hidden` guard above already excludes.
        self.split_manager_mut()
            .unmaximize_split()
            .expect("a split is maximized (checked above)");
        self.relayout();
    }

    // If the active split leaf carries `SplitRole::UtilityDock`,
    // move the active leaf back to the user's last regular editor
    // leaf. Called from the file-open path so that opening a file
    // while a utility panel holds focus doesn't turn the dock into
    // a tab strip for ordinary files.
    //
    // Routing falls back to the first non-dock leaf in tree order
    // when the user has only ever interacted with the dock — a
    // rare boot-state path.
    // `redirect_active_split_away_from_dock_if_needed` lives on
    // `impl Window` — call it via
    // `self.active_window_mut().redirect_active_split_away_from_dock_if_needed()`.

    /// Open a file without switching focus to it
    ///
    /// Creates a new buffer for the file (or returns existing buffer ID if already open)
    /// but does not change the active buffer. Useful for opening files in background tabs.
    ///
    /// If the file doesn't exist, creates an unsaved buffer with that filename.
    ///
    /// Thin delegator: the open-file core lives on `impl Window` (rooted
    /// at the window's own `root` / `resources`). The editor forwards to
    /// the active window.
    pub fn open_file_no_focus(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        self.active_window_mut().open_file_no_focus(path)
    }

    /// Open a file without switching focus AND without ever
    /// repurposing the active "no name" buffer. Thin delegator to the
    /// active window's `Window::open_file_for_preview`.
    pub(super) fn open_file_for_preview(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        self.active_window_mut().open_file_for_preview(path)
    }

    // `open_local_file` lives on `impl Window` — call it via
    // `self.active_window_mut().open_local_file(path)`.

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
        let base_dir = self.working_dir().to_path_buf();

        let resolved_path = if path.is_relative() {
            base_dir.join(path)
        } else {
            path.to_path_buf()
        };

        // Save user-visible path for language detection before canonicalizing
        let display_path = resolved_path.clone();

        // Canonicalize the path
        let canonical_path = self
            .authority()
            .filesystem
            .canonicalize(&resolved_path)
            .unwrap_or_else(|_| resolved_path.clone());
        let path = canonical_path.as_path();

        // Check if already open
        let already_open = self
            .buffers()
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(path))
            .map(|(id, _)| *id);

        if let Some(id) = already_open {
            // File is already open - update its encoding and reload
            if let Some(state) = self
                .windows
                .get_mut(&self.active_window)
                .map(|w| &mut w.buffers)
                .expect("active window present")
                .get_mut(&id)
            {
                state.buffer.set_encoding(encoding);
            }
            self.set_active_buffer(id);
            return Ok(id);
        }

        // Create new buffer with specified encoding
        let buffer_id = self.alloc_buffer_id();

        // Load buffer with the specified encoding (use canonical path for I/O)
        let buffer = crate::model::buffer::Buffer::load_from_file_with_encoding(
            path,
            encoding,
            Arc::clone(&self.authority().filesystem),
            crate::model::buffer::BufferConfig {
                estimated_line_length: self.config.editor.estimated_line_length,
            },
        )?;
        let first_line = buffer.first_line_lossy();
        // Create editor state with the buffer
        // Use display_path for language detection (glob patterns match user-visible paths)
        let detected =
            crate::primitives::detected_language::DetectedLanguage::from_path_with_fallback(
                &display_path,
                first_line.as_deref(),
                &self.grammar_registry,
                &self.config.languages,
                self.config.default_language.as_deref(),
            );

        let mut state = EditorState::from_buffer_with_language(buffer, detected);

        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);
        state.reference_highlight_overlay.enabled = self.config.editor.highlight_occurrences;

        self.windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .insert(buffer_id, state);
        self.active_window_mut()
            .event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        let metadata = super::types::BufferMetadata::with_file(
            path.to_path_buf(),
            &display_path,
            self.working_dir(),
            self.authority().path_translation.as_ref(),
            self.config.editor.auto_read_only,
        );
        self.active_window_mut()
            .buffer_metadata
            .insert(buffer_id, metadata);

        // Add to preferred split's tabs (avoids labeled splits like sidebars)
        let target_split = self.active_window().preferred_split_for_file();
        let line_wrap = self.active_window().resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self
            .active_window()
            .resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .get_mut(&target_split)
        {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
                line_numbers: self.config.editor.line_numbers,
                highlight_current_line: self.config.editor.highlight_current_line,
                line_wrap,
                wrap_indent: self.config.editor.wrap_indent,
                wrap_column,
                rulers: self.config.editor.rulers.clone(),
                scroll_offset: self.config.editor.scroll_offset,
            });
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
            .buffers()
            .get(&buffer_id)
            .and_then(|s| s.buffer.file_path().map(|p| p.to_path_buf()))
            .ok_or_else(|| anyhow::anyhow!("Buffer has no file path"))?;

        // Check for unsaved modifications
        if let Some(state) = self
            .windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .get(&buffer_id)
        {
            if state.buffer.is_modified() {
                anyhow::bail!("Cannot reload: buffer has unsaved modifications");
            }
        }

        // Reload the buffer with the new encoding
        let new_buffer = crate::model::buffer::Buffer::load_from_file_with_encoding(
            &path,
            encoding,
            Arc::clone(&self.authority().filesystem),
            crate::model::buffer::BufferConfig {
                estimated_line_length: self.config.editor.estimated_line_length,
            },
        )?;

        // Update the buffer in the editor state
        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
            state.buffer = new_buffer;
            // Invalidate highlighting
            state.highlighter.invalidate_all();
        }

        // Reset cursor to start in the split view state
        let split_id = self
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
            .get_mut(&split_id)
        {
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
        let base_dir = self.working_dir().to_path_buf();

        let resolved_path = if path.is_relative() {
            base_dir.join(path)
        } else {
            path.to_path_buf()
        };

        // Save user-visible path for language detection before canonicalizing
        let display_path = resolved_path.clone();

        // Canonicalize the path
        let canonical_path = self
            .authority()
            .filesystem
            .canonicalize(&resolved_path)
            .unwrap_or_else(|_| resolved_path.clone());
        let path = canonical_path.as_path();

        // Check if already open
        let already_open = self
            .buffers()
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(path))
            .map(|(id, _)| *id);

        if let Some(id) = already_open {
            self.set_active_buffer(id);
            return Ok(id);
        }

        // Create new buffer with forced full loading
        let buffer_id = self.alloc_buffer_id();

        // Load buffer with forced full loading (bypasses the large file encoding check)
        let buffer = crate::model::buffer::Buffer::load_large_file_confirmed(
            path,
            Arc::clone(&self.authority().filesystem),
        )?;
        let first_line = buffer.first_line_lossy();
        // Create editor state with the buffer
        // Use display_path for language detection (glob patterns match user-visible paths)
        let detected =
            crate::primitives::detected_language::DetectedLanguage::from_path_with_fallback(
                &display_path,
                first_line.as_deref(),
                &self.grammar_registry,
                &self.config.languages,
                self.config.default_language.as_deref(),
            );

        let mut state = EditorState::from_buffer_with_language(buffer, detected);

        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);
        state.reference_highlight_overlay.enabled = self.config.editor.highlight_occurrences;

        self.windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .insert(buffer_id, state);
        self.active_window_mut()
            .event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        let metadata = super::types::BufferMetadata::with_file(
            path.to_path_buf(),
            &display_path,
            self.working_dir(),
            self.authority().path_translation.as_ref(),
            self.config.editor.auto_read_only,
        );
        self.active_window_mut()
            .buffer_metadata
            .insert(buffer_id, metadata);

        // Add to preferred split's tabs (avoids labeled splits like sidebars)
        let target_split = self.active_window().preferred_split_for_file();
        let line_wrap = self.active_window().resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self
            .active_window()
            .resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .get_mut(&target_split)
        {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
                line_numbers: self.config.editor.line_numbers,
                highlight_current_line: self.config.editor.highlight_current_line,
                line_wrap,
                wrap_indent: self.config.editor.wrap_indent,
                wrap_column,
                rulers: self.config.editor.rulers.clone(),
                scroll_offset: self.config.editor.scroll_offset,
            });
        }

        self.set_active_buffer(buffer_id);

        // Use display_name from metadata for relative path display
        let display_name = self
            .active_window()
            .buffer_metadata
            .get(&buffer_id)
            .map(|m| m.display_name.clone())
            .unwrap_or_else(|| path.display().to_string());

        self.active_window_mut().status_message =
            Some(t!("buffer.opened", name = display_name).to_string());

        Ok(buffer_id)
    }

    // Restore global file state (cursor and scroll position) for a newly opened file.
    //
    // This looks up the file's saved state from the global file states store
    // and applies it to both the EditorState (cursor) and SplitViewState (viewport).
    // `restore_global_file_state` and `save_file_state_on_close` live
    // on `impl Window` — call them via
    // `self.active_window_mut().restore_global_file_state(...)` and
    // `self.active_window().save_file_state_on_close(...)`.

    /// Open the file an LSP response URI points at, handling the three
    /// cases the goto-def / references / workspace-edit handlers all
    /// have to think about:
    ///
    ///   * **on-host file** (the workspace bind mount, or a local
    ///     authority): host-translate the URI and open the host file
    ///     normally — exactly what the editor has always done.
    ///   * **container-only file** (devcontainer attach with the
    ///     target outside the workspace mount, e.g. a pip-installed
    ///     `~/.local/.../site-packages/flask/app.py`): fetch the file
    ///     bytes via the authority's process spawner
    ///     (`docker exec <id> cat <path>`) and open them as a
    ///     read-only buffer at the in-container path.
    ///   * **unreachable** (no file at the host path; container fetch
    ///     failed or no container authority): return `Err` so the
    ///     caller can surface a user-visible status message instead
    ///     of silently opening a phantom buffer.
    ///
    /// Cursor placement, focus, and any post-open hook firing are the
    /// caller's job (this method just resolves "URI → BufferId").
    pub(crate) fn open_lsp_uri_target(
        &mut self,
        uri: &crate::app::types::LspUri,
    ) -> anyhow::Result<BufferId> {
        let translation = self.authority().path_translation.clone();
        let host_path = uri
            .to_host_path(translation.as_ref())
            .ok_or_else(|| anyhow::anyhow!("URI is not a file path"))?;

        // Case 1: file is reachable on the host filesystem (either
        // local authority, or workspace-mounted on a devcontainer).
        // `open_file` focuses, which is what callers (goto-def,
        // workspace edits) expect — they want the cursor to land in
        // the destination buffer afterward.
        if self.authority().filesystem.exists(&host_path) {
            return self.open_file(&host_path);
        }

        // Case 2: container-only fetch. Only meaningful when the
        // active authority can route a `cat` through to the
        // container — `path_translation` being set is the proxy for
        // "this is a container authority". Local + SSH authorities
        // skip straight to the error case.
        if translation.is_some() {
            // The container-side path is the URI's raw path. Calling
            // `to_host_path` with `None` returns the wire-side path
            // verbatim (no translation applied) — exactly what we
            // need for `cat <path>` inside the container.
            let container_path = uri.to_host_path(None).ok_or_else(|| {
                anyhow::anyhow!("URI is not a file path (container-side decode failed)")
            })?;
            let buffer_id = self.fetch_and_open_container_file(container_path, uri.clone())?;
            // Match `open_file`'s focus behaviour so the cursor
            // assertion in callers (goto-def's `MoveCursor` event)
            // applies to the right buffer.
            self.set_active_buffer(buffer_id);
            return Ok(buffer_id);
        }

        // Case 3: nothing we can open.
        Err(anyhow::anyhow!(
            "could not open {}: file not found",
            host_path.display()
        ))
    }

    /// Run `cat <container_path>` through the active authority's
    /// process spawner and open the result as a read-only buffer
    /// tagged with the wire URI. Helper for [`Self::open_lsp_uri_target`].
    ///
    /// On `cat` exit-code 0 the bytes become the buffer's contents.
    /// On any error (no tokio runtime, spawner failure, non-zero
    /// exit) we return `Err` with a message that includes the
    /// container path and stderr's first line — enough for the
    /// caller's status-line surface.
    fn fetch_and_open_container_file(
        &mut self,
        container_path: std::path::PathBuf,
        uri: crate::app::types::LspUri,
    ) -> anyhow::Result<BufferId> {
        let runtime = self.tokio_runtime.as_ref().ok_or_else(|| {
            anyhow::anyhow!(
                "could not open {}: no tokio runtime available for container fetch",
                container_path.display()
            )
        })?;

        let spawner = self.authority().process_spawner.clone();
        let path_arg = container_path.to_string_lossy().into_owned();
        let result = runtime
            .block_on(spawner.spawn("cat".into(), vec![path_arg], None))
            .map_err(|e| {
                anyhow::anyhow!(
                    "could not open {} from container: {}",
                    container_path.display(),
                    e
                )
            })?;

        if result.exit_code != 0 {
            let first_stderr_line = result
                .stderr
                .lines()
                .next()
                .unwrap_or("(no error message)")
                .trim();
            anyhow::bail!(
                "could not open {} from container: {}",
                container_path.display(),
                first_stderr_line
            );
        }

        self.open_container_only_file(container_path, uri, result.stdout.into_bytes())
    }

    /// Build a buffer from already-fetched container content. The
    /// buffer's `file_path` is the in-container path (so further LSP
    /// requests carry the right URI) and the buffer is read-only —
    /// there is no host writeback path for files that exist only
    /// inside the container. LSP stays enabled so a follow-up
    /// goto-def from the fetched buffer works.
    pub(crate) fn open_container_only_file(
        &mut self,
        container_path: std::path::PathBuf,
        uri: crate::app::types::LspUri,
        content: Vec<u8>,
    ) -> anyhow::Result<BufferId> {
        // Don't double-open. The file_path matches by container path,
        // since that's what we set after build.
        let already_open = self
            .buffers()
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(container_path.as_path()))
            .map(|(id, _)| *id);
        if let Some(id) = already_open {
            return Ok(id);
        }

        // Build the buffer from the fetched bytes and pin its
        // file_path to the container path. The host filesystem ref
        // here is mostly cosmetic — the buffer is read-only so save
        // never runs through it.
        let mut buffer = crate::model::buffer::Buffer::from_bytes(
            content,
            Arc::clone(&self.authority().filesystem),
        );
        buffer.rename_file_path(container_path.clone());

        // Detect language from the container path (the basename's
        // extension is what matters; the directory tree is
        // container-side and won't match host-relative globs anyway).
        let first_line = buffer.first_line_lossy();
        let detected =
            crate::primitives::detected_language::DetectedLanguage::from_path_with_fallback(
                &container_path,
                first_line.as_deref(),
                &self.grammar_registry,
                &self.config.languages,
                self.config.default_language.as_deref(),
            );
        let mut state = EditorState::from_buffer_with_language(buffer, detected);
        state.editing_disabled = true;

        // Whitespace / tab settings — same shape as `open_file_no_focus`
        // so the rendered look is consistent. Container-fetched
        // buffers should obey the user's editor config like any other
        // read-only buffer.
        let mut whitespace =
            crate::config::WhitespaceVisibility::from_editor_config(&self.config.editor);
        if let Some(lang_config) = self.config.languages.get(&state.language) {
            whitespace = whitespace.with_language_tab_override(lang_config.show_whitespace_tabs);
            state.buffer_settings.use_tabs =
                lang_config.use_tabs.unwrap_or(self.config.editor.use_tabs);
            state.buffer_settings.tab_size =
                lang_config.tab_size.unwrap_or(self.config.editor.tab_size);
        } else {
            state.buffer_settings.tab_size = self.config.editor.tab_size;
            state.buffer_settings.use_tabs = self.config.editor.use_tabs;
        }
        state.buffer_settings.whitespace = whitespace;
        state
            .margins
            .configure_for_line_numbers(self.config.editor.line_numbers);
        state.reference_highlight_overlay.enabled = self.config.editor.highlight_occurrences;

        let buffer_id = self.alloc_buffer_id();
        self.windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .insert(buffer_id, state);
        self.active_window_mut()
            .event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        let mut metadata =
            super::types::BufferMetadata::with_container_file(container_path.clone(), uri);
        // Notify the LSP servers about the newly opened file so
        // hover / further goto-def in the fetched buffer works. The
        // URI we cached is already the wire-form URI, so the LSP
        // sees the right path.
        self.notify_lsp_file_opened(&container_path, buffer_id, &mut metadata);
        self.active_window_mut()
            .buffer_metadata
            .insert(buffer_id, metadata);

        // Wire the buffer into a tab on the preferred split, mirroring
        // the host-file path. Skip `watch_file` — there's no host
        // file to inotify, and the spawned-fetch is one-shot.
        let target_split = self.active_window().preferred_split_for_file();
        let line_wrap = self.active_window().resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self
            .active_window()
            .resolve_wrap_column_for_buffer(buffer_id);
        if let Some(view_state) = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .get_mut(&target_split)
        {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
                line_numbers: self.config.editor.line_numbers,
                highlight_current_line: self.config.editor.highlight_current_line,
                line_wrap,
                wrap_indent: self.config.editor.wrap_indent,
                wrap_column,
                rulers: self.config.editor.rulers.clone(),
                scroll_offset: self.config.editor.scroll_offset,
            });
        }

        Ok(buffer_id)
    }
}

impl crate::app::window::Window {
    /// Open a file without switching focus to it.
    ///
    /// Window-scoped core of the open-file path: creates a new buffer
    /// for the file (or returns the existing buffer id if already open)
    /// without changing the active buffer. Rooted at this window's own
    /// `root` / `resources` so it can open files directly into a
    /// non-active window (e.g. workspace restore) with no active-window
    /// flip. If the file doesn't exist, creates an unsaved buffer with
    /// that filename.
    pub fn open_file_no_focus(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        self.open_file_no_focus_inner(path, true, OpenKind::Commit)
    }

    /// `open_file_no_focus` with an explicit [`OpenKind`], so the Editor-side
    /// `open_file_with_kind` can route a preview open through the same core
    /// while deferring `after_file_open`.
    pub(crate) fn open_file_no_focus_with_kind(
        &mut self,
        path: &Path,
        kind: OpenKind,
    ) -> anyhow::Result<BufferId> {
        self.open_file_no_focus_inner(path, true, kind)
    }

    /// Open a file without switching focus AND without ever
    /// repurposing the active "no name" buffer.
    ///
    /// `open_file_no_focus`'s `replace_current` heuristic reuses the
    /// initial empty unnamed buffer for the *first* file the user
    /// opens — convenient for the normal "fresh launch → open file"
    /// flow. The Live Grep floating overlay's preview pane needs the
    /// opposite: the user's current buffer (often the empty unnamed
    /// scratch) must stay untouched as preview cycles through
    /// results. This variant always allocates a fresh BufferId so the
    /// background buffer never gets repurposed.
    pub(crate) fn open_file_for_preview(&mut self, path: &Path) -> anyhow::Result<BufferId> {
        // Live-grep preview is a browse, not a deliberate open: defer the
        // `after_file_open` hook so plugins don't pop UI / run side effects
        // over each result the user cycles through.
        self.open_file_no_focus_inner(path, false, OpenKind::Preview)
    }

    /// True if `path` is an internal app artifact (terminal scrollback,
    /// git-show output, etc.) that the user is inspecting rather than
    /// editing — i.e. it lives under the app data dir but outside this
    /// window's own project root. A session working tree can itself live
    /// under the data dir (conductor / orchestrator sessions); files under
    /// the window root are real working files, not artifacts.
    /// Paths are canonicalized so a symlinked home (e.g. macOS
    /// `/var` → `/private/var`) doesn't defeat the prefix checks.
    fn is_internal_data_artifact(&self, path: &Path) -> bool {
        let canonicalize = |p: &Path| {
            self.authority()
                .filesystem
                .canonicalize(p)
                .unwrap_or_else(|_| p.to_path_buf())
        };
        let canonical_data = canonicalize(&self.resources.dir_context.data_dir);
        let canonical_root = canonicalize(&self.root);
        path.starts_with(&canonical_data) && !path.starts_with(&canonical_root)
    }

    fn open_file_no_focus_inner(
        &mut self,
        path: &Path,
        allow_replace_empty: bool,
        kind: OpenKind,
    ) -> anyhow::Result<BufferId> {
        // Fail fast if the remote connection is down — don't attempt I/O that
        // would either timeout or return confusing errors.
        if !self.authority().filesystem.is_remote_connected() {
            anyhow::bail!(
                "Cannot open file: remote connection lost ({})",
                self.authority()
                    .filesystem
                    .remote_connection_info()
                    .unwrap_or("unknown host")
            );
        }

        // Resolve relative paths against appropriate base directory.
        // For remote mode, use the remote home directory; for local, use
        // this window's root.
        let base_dir = if self
            .authority()
            .filesystem
            .remote_connection_info()
            .is_some()
        {
            self.authority()
                .filesystem
                .home_dir()
                .unwrap_or_else(|_| self.root.clone())
        } else {
            self.root.clone()
        };

        let resolved_path = if path.is_relative() {
            base_dir.join(path)
        } else {
            path.to_path_buf()
        };

        // Determine if we're opening a non-existent file (for creating new files)
        // Use filesystem trait method to support remote files
        let file_exists = self.authority().filesystem.exists(&resolved_path);

        // Save the user-visible (non-canonicalized) path for language detection.
        // Glob patterns in language config should match the path as the user sees it,
        // not the canonical path (e.g., on macOS /var -> /private/var symlinks).
        let display_path = resolved_path.clone();

        // Canonicalize the path to resolve symlinks and normalize path components
        // This ensures consistent path representation throughout the editor
        // For non-existent files, we need to canonicalize the parent directory and append the filename
        let canonical_path = if file_exists {
            self.authority()
                .filesystem
                .canonicalize(&resolved_path)
                .unwrap_or_else(|_| resolved_path.clone())
        } else {
            // For non-existent files, canonicalize parent dir and append filename
            if let Some(parent) = resolved_path.parent() {
                let canonical_parent = if parent.as_os_str().is_empty() {
                    // No parent means just a filename, use base dir
                    base_dir.clone()
                } else {
                    self.authority()
                        .filesystem
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
        if self.authority().filesystem.is_dir(path).unwrap_or(false) {
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
        // Note: Don't replace composite buffers (they appear empty but are special views).
        // Suppressed when `allow_replace_empty` is false — see
        // `open_file_for_preview` for the rationale.
        let replace_current = allow_replace_empty && {
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
            self.alloc_buffer_id()
        };

        // Create the editor state - either load from file or create empty buffer
        tracing::info!(
            "[SYNTAX DEBUG] open_file_no_focus: path={:?}, extension={:?}, catalog={}",
            path,
            path.extension(),
            self.resources.grammar_registry.catalog().len(),
        );
        let mut state = if file_exists {
            // Load from canonical path (for I/O and dedup), detect language from
            // display path (for glob pattern matching against user-visible names).
            let buffer = crate::model::buffer::Buffer::load_from_file(
                &canonical_path,
                self.resources.config.editor.large_file_threshold_bytes as usize,
                Arc::clone(&self.authority().filesystem),
            )?;
            let first_line = buffer.first_line_lossy();
            let detected =
                crate::primitives::detected_language::DetectedLanguage::from_path_with_fallback(
                    &display_path,
                    first_line.as_deref(),
                    &self.resources.grammar_registry,
                    &self.resources.config.languages,
                    self.resources.config.default_language.as_deref(),
                );
            EditorState::from_buffer_with_language(buffer, detected)
        } else {
            // File doesn't exist - create empty buffer with the file path set
            EditorState::new_with_path(
                self.resources.config.editor.large_file_threshold_bytes as usize,
                Arc::clone(&self.authority().filesystem),
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

        // Internal app artifacts under the data dir (e.g. terminal scrollback
        // backing files surfaced by Universal Search) are things the user is
        // inspecting, not editing — open them read-only so an accidental
        // keystroke can't corrupt persisted state. Files inside the window's
        // own root are excluded so session working trees that live under the
        // data dir stay editable.
        if self.is_internal_data_artifact(&canonical_path) {
            state.editing_disabled = true;
        }

        // Set whitespace visibility, use_tabs, and tab_size based on language config
        // with fallback to global editor config for tab_size
        // Use the buffer's stored language (already set by from_file_with_languages)
        let mut whitespace =
            crate::config::WhitespaceVisibility::from_editor_config(&self.resources.config.editor);
        state.buffer_settings.auto_close = self.resources.config.editor.auto_close;
        state.buffer_settings.auto_surround = self.resources.config.editor.auto_surround;
        if let Some(lang_config) = self.resources.config.languages.get(&state.language) {
            whitespace = whitespace.with_language_tab_override(lang_config.show_whitespace_tabs);
            state.buffer_settings.use_tabs = lang_config
                .use_tabs
                .unwrap_or(self.resources.config.editor.use_tabs);
            // Use language-specific tab_size if set, otherwise fall back to global
            state.buffer_settings.tab_size = lang_config
                .tab_size
                .unwrap_or(self.resources.config.editor.tab_size);
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
            state.buffer_settings.tab_size = self.resources.config.editor.tab_size;
            state.buffer_settings.use_tabs = self.resources.config.editor.use_tabs;
        }
        state.buffer_settings.whitespace = whitespace;

        // Apply line_numbers default from config
        state
            .margins
            .configure_for_line_numbers(self.resources.config.editor.line_numbers);
        state.reference_highlight_overlay.enabled =
            self.resources.config.editor.highlight_occurrences;

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Create metadata for this buffer
        let mut metadata = crate::app::types::BufferMetadata::with_file(
            path.to_path_buf(),
            &display_path,
            &self.root,
            self.authority().path_translation.as_ref(),
            self.resources.config.editor.auto_read_only,
        );

        // Mark binary files in metadata and disable LSP
        if is_binary {
            metadata.binary = true;
            metadata.read_only = true;
            metadata.disable_lsp(t!("buffer.binary_file").to_string());
        }

        // Check if the file is read-only on disk (filesystem permissions),
        // unless the user opted out of automatic read-only via config
        if file_exists
            && !metadata.read_only
            && self.resources.config.editor.auto_read_only
            && !self.authority().filesystem.is_writable(path)
        {
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
        // Snapshot config values before taking the mutable view-states borrow
        // so the closure body doesn't have to re-borrow `self.resources`.
        let cfg = self.resources.config.editor.clone();
        if let Some(view_state) = self
            .split_view_states_mut()
            .expect("active window must have a populated split layout")
            .get_mut(&target_split)
        {
            view_state.add_buffer(buffer_id);
            // Initialize per-buffer view state for the new buffer with config defaults
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
                line_numbers: cfg.line_numbers,
                highlight_current_line: cfg.highlight_current_line,
                line_wrap,
                wrap_indent: cfg.wrap_indent,
                wrap_column,
                rulers: cfg.rulers,
                scroll_offset: cfg.scroll_offset,
            });
            // Auto-activate page view if configured for this language
            if let Some(page_width) = page_view {
                buf_state.activate_page_view(page_width);
            }
        }

        // Restore global file state (scroll/cursor position) if available
        // This persists file positions across projects and editor instances
        self.restore_global_file_state(buffer_id, path, target_split);

        // Emit control event
        self.resources.event_broadcaster.emit_named(
            crate::model::control_event::events::FILE_OPENED.name,
            serde_json::json!({
                "path": path.display().to_string(),
                "buffer_id": buffer_id.0
            }),
        );

        // Track file for auto-revert and conflict detection
        self.watch_file(path);

        // Fire AfterFileOpen hook for plugins — but not for preview opens
        // (file-explorer browse, live-grep overlay). A preview is "just
        // looking": firing this hook lets plugins raise intrusive UI (e.g.
        // the asm-lsp config-offer popup) or run side effects (csharp
        // `dotnet restore`) over a file the user is merely glancing at as
        // previews replace each other. For a preview the hook is deferred —
        // it fires once the buffer is escalated to a permanent tab (see
        // `Window::promote_*`). Plugins that need to react when a preview
        // becomes visible use `buffer_activated`, which still fires on every
        // preview switch.
        if kind == OpenKind::Commit {
            self.run_after_file_open_hook(buffer_id, path.to_path_buf());
        }

        Ok(buffer_id)
    }

    /// Fire the `after_file_open` plugin hook for `buffer_id`. Single site so
    /// both the commit-time open path and the escalation (promote) path raise
    /// it identically.
    pub(crate) fn run_after_file_open_hook(&self, buffer_id: BufferId, path: std::path::PathBuf) {
        self.resources.plugin_manager.read().unwrap().run_hook(
            "after_file_open",
            crate::services::plugins::hooks::HookArgs::AfterFileOpen { buffer_id, path },
        );
    }

    /// Fire the deferred `after_file_open` hook for a buffer that was opened
    /// as a preview and is now being escalated to a permanent tab. Looks up
    /// the buffer's own file path; a no-op for buffers without one.
    pub(crate) fn fire_deferred_after_file_open(&self, buffer_id: BufferId) {
        if let Some(path) = self
            .buffers
            .get(&buffer_id)
            .and_then(|s| s.buffer.file_path())
            .filter(|p| !p.as_os_str().is_empty())
            .map(|p| p.to_path_buf())
        {
            self.run_after_file_open_hook(buffer_id, path);
        }
    }
}
