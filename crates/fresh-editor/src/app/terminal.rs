//! Terminal integration for the Editor
//!
//! This module provides methods for the Editor to interact with the terminal system:
//! - Opening new terminal sessions
//! - Closing terminals
//! - Rendering terminal content
//! - Handling terminal input
//!
//! # Role in Incremental Streaming Architecture
//!
//! This module handles mode switching between terminal and scrollback modes.
//! See `crate::services::terminal` for the full architecture diagram.
//!
//! ## Mode Switching Methods
//!
//! - [`Window::sync_terminal_to_buffer`]: Terminal → Scrollback mode
//!   - Appends visible screen (~50 lines) to backing file
//!   - Loads backing file as read-only buffer
//!   - Performance: O(screen_size) ≈ 5ms
//!
//! - [`Editor::enter_terminal_mode`]: Scrollback → Terminal mode
//!   - Truncates backing file to remove visible screen tail
//!   - Resumes live terminal rendering
//!   - Performance: O(1) ≈ 1ms

use super::window::Window;
use super::{BufferId, BufferMetadata, Editor};
use crate::model::event::LeafId;
use crate::services::authority::TerminalWrapper;
use crate::services::terminal::TerminalId;
use crate::state::EditorState;
use crate::view::split::SplitViewState;
use rust_i18n::t;
use std::path::PathBuf;

impl Window {
    /// Resolve the terminal wrapper used to spawn a new integrated
    /// terminal in this window, applying the `terminal.shell` config
    /// override on top of the authority's wrapper when appropriate.
    ///
    /// See `TerminalWrapper::with_user_shell_override` for the override
    /// rules; this is just the per-window wiring that supplies the
    /// active config.
    pub(crate) fn resolved_terminal_wrapper(&self) -> TerminalWrapper {
        self.resources
            .authority
            .terminal_wrapper
            .clone()
            .with_user_shell_override(self.resources.config.terminal.shell.as_ref())
    }

    /// Get terminal dimensions appropriate for spawning a PTY in this
    /// window. Derived from the window's cached screen size minus a
    /// small constant for menu/status chrome.
    pub(crate) fn get_terminal_dimensions(&self) -> (u16, u16) {
        let cols = self.terminal_width.saturating_sub(2).max(40);
        let rows = self.terminal_height.saturating_sub(4).max(10);
        (cols, rows)
    }

    /// Spawn a new PTY-backed terminal session in this window and
    /// record its log/backing files. Returns the terminal id on
    /// success — does **not** create a buffer or attach to any
    /// split. Callers are responsible for the rest of the wiring
    /// (see `create_terminal_buffer_attached` /
    /// `create_terminal_buffer_detached`).
    ///
    /// `cwd` defaults to this window's `root` when None. `persistent`
    /// controls whether the backing files use stable names
    /// (`fresh-terminal-N.{log,txt}`) so workspace restore can find
    /// them, or per-spawn ephemeral suffixes
    /// (`fresh-terminal-eph-N-<ts>.{log,txt}`); non-persistent
    /// terminals are also added to `ephemeral_terminals` so the
    /// workspace serialiser skips them.
    ///
    /// On spawn failure the error is logged and a status message is
    /// set on this window; the caller gets `None` back.
    pub fn spawn_terminal_session(
        &mut self,
        cwd: Option<PathBuf>,
        persistent: bool,
        command_override: Option<Vec<String>>,
    ) -> Option<TerminalId> {
        let (cols, rows) = self.get_terminal_dimensions();

        // Per-window async bridge — terminal output flows back through
        // the window that owns the PTY.
        let bridge = self.bridge.clone();
        self.terminal_manager.set_async_bridge(bridge);

        let working_dir = cwd.unwrap_or_else(|| self.root.clone());
        let terminal_root = self.resources.dir_context.terminal_dir_for(&working_dir);
        if let Err(e) = self
            .resources
            .authority
            .filesystem
            .create_dir_all(&terminal_root)
        {
            tracing::warn!("Failed to create terminal directory: {}", e);
        }

        // Precompute paths using the next terminal ID so we capture
        // from the first byte. Ephemeral terminals get a per-spawn
        // suffix so there is no possibility of picking up scrollback
        // a previous run (with the same numeric terminal ID) wrote
        // to the same path.
        let predicted_terminal_id = self.terminal_manager.next_terminal_id();
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
            .insert(predicted_terminal_id, backing_path.clone());

        // When the caller supplies an explicit argv, build a wrapper
        // that runs it directly instead of the authority's shell. We
        // keep `manages_cwd: false` so the PTY's cwd is honoured by
        // the spawn (the authority's `manages_cwd` flag only applies
        // when the wrapper itself re-roots cwd, like the docker /
        // ssh paths). Empty argv falls back to the shell — there's
        // nothing for the host to run.
        let wrapper = match command_override {
            Some(argv) if !argv.is_empty() => {
                let (command, args) = argv.split_first().expect("non-empty argv");
                crate::services::authority::TerminalWrapper {
                    command: command.clone(),
                    args: args.to_vec(),
                    manages_cwd: false,
                }
            }
            _ => self.resolved_terminal_wrapper(),
        };
        match self.terminal_manager.spawn(
            cols,
            rows,
            Some(working_dir),
            Some(log_path.clone()),
            Some(backing_path),
            wrapper,
        ) {
            Ok(terminal_id) => {
                self.terminal_log_files.insert(terminal_id, log_path);
                // If the actual terminal id differs from the predicted
                // one, move the backing-file entry to the real id and
                // rename to the persistent (no-eph-suffix) form. This
                // mirrors the pre-migration behaviour exactly.
                if terminal_id != predicted_terminal_id {
                    self.terminal_backing_files.remove(&predicted_terminal_id);
                    let backing_path =
                        terminal_root.join(format!("fresh-terminal-{}.txt", terminal_id.0));
                    self.terminal_backing_files
                        .insert(terminal_id, backing_path);
                }
                if !persistent {
                    self.ephemeral_terminals.insert(terminal_id);
                }
                Some(terminal_id)
            }
            Err(e) => {
                self.set_status_message(
                    t!("terminal.failed_to_open", error = e.to_string()).to_string(),
                );
                tracing::error!("Failed to open terminal: {}", e);
                None
            }
        }
    }

    /// Create a buffer for a terminal session in this window, attached
    /// to the specified split. Mirrors the pre-migration body of
    /// `Editor::create_terminal_buffer_attached`.
    pub fn create_terminal_buffer_attached(
        &mut self,
        terminal_id: TerminalId,
        split_id: LeafId,
    ) -> BufferId {
        let buffer_id = self.alloc_buffer_id();
        let large_file_threshold = self.resources.config.editor.large_file_threshold_bytes as usize;

        // Rendered backing file for scrollback view (reuse if already
        // recorded by `spawn_terminal_session`).
        let backing_file = self
            .terminal_backing_files
            .get(&terminal_id)
            .cloned()
            .unwrap_or_else(|| {
                let root = self.resources.dir_context.terminal_dir_for(&self.root);
                if let Err(e) = self.resources.authority.filesystem.create_dir_all(&root) {
                    tracing::warn!("Failed to create terminal directory: {}", e);
                }
                root.join(format!("fresh-terminal-{}.txt", terminal_id.0))
            });

        // Ensure the file exists — but DON'T truncate if it already has
        // content. The PTY read loop may have already started writing
        // scrollback.
        if !self.resources.authority.filesystem.exists(&backing_file) {
            if let Err(e) = self
                .resources
                .authority
                .filesystem
                .write_file(&backing_file, &[])
            {
                tracing::warn!("Failed to create terminal backing file: {}", e);
            }
        }

        self.terminal_backing_files
            .insert(terminal_id, backing_file.clone());

        let mut state = EditorState::new_with_path(
            large_file_threshold,
            std::sync::Arc::clone(&self.resources.authority.filesystem),
            backing_file.clone(),
        );
        state.margins.configure_for_line_numbers(false);
        self.buffers.insert(buffer_id, state);

        // Virtual metadata so the tab shows "*Terminal N*" and LSP
        // stays off.
        let metadata = BufferMetadata::virtual_buffer(
            format!("*Terminal {}*", terminal_id.0),
            "terminal".into(),
            false,
        );
        self.buffer_metadata.insert(buffer_id, metadata);
        self.terminal_buffers.insert(buffer_id, terminal_id);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        if let Some(view_states) = self.split_view_states_mut() {
            if let Some(view_state) = view_states.get_mut(&split_id) {
                view_state.add_buffer(buffer_id);
                // Terminal buffers should not wrap lines so escape
                // sequences stay intact.
                view_state.viewport.line_wrap_enabled = false;
                // Disable line numbers + current-line highlight for the
                // terminal buffer's per-buffer view state so exiting
                // terminal mode doesn't suddenly add a gutter / row
                // highlight. The render path overwrites the buffer's
                // margin config every frame from this view-state flag,
                // so setting it here is required even though
                // `state.margins.configure_for_line_numbers(false)` was
                // already called above.
                let buf_state = view_state.ensure_buffer_state(buffer_id);
                buf_state.show_line_numbers = false;
                buf_state.highlight_current_line = false;
                buf_state.viewport.line_wrap_enabled = false;
            }
        }

        buffer_id
    }

    /// Plugin-facing terminal creation in this window. Handles all
    /// the variants the JS `editor.createTerminal` API exposes:
    ///
    /// - `direction = None`: attach the terminal as a new tab in the
    ///   window's active split (or seed a fresh split layout rooted
    ///   at the terminal if the window has never been activated and
    ///   therefore has no layout yet).
    /// - `direction = Some(dir)`: create a new horizontal/vertical
    ///   split off the active split and place the terminal there.
    ///   `ratio` controls the split's size (default 0.5). `focus`
    ///   controls whether the new split becomes the window's active
    ///   split.
    ///
    /// In all cases the leader pid is registered with the window's
    /// `process_groups` tracker so cross-window signal operations
    /// (Stop / Archive / Delete) can reach the spawned process group.
    ///
    /// Returns `(terminal_id, buffer_id, created_split_id)` on
    /// success. `created_split_id` is `Some` when a split was created
    /// (either explicitly via `direction = Some` or implicitly when
    /// seeding a fresh layout in a never-activated window).
    pub fn create_plugin_terminal(
        &mut self,
        cwd: Option<PathBuf>,
        direction: Option<crate::model::event::SplitDirection>,
        ratio: Option<f32>,
        focus: bool,
        persistent: bool,
        command: Option<Vec<String>>,
        title: Option<String>,
    ) -> Result<(TerminalId, BufferId, Option<LeafId>), String> {
        // Derive the auto-title from the command's executable name
        // (basename of argv[0]). The host writes this into the
        // terminal buffer's `BufferMetadata::name` so the tab reads
        // e.g. "python3" instead of "*Terminal N*" when the plugin
        // runs python3 directly. Explicit `title` overrides.
        let auto_title = command.as_ref().and_then(|argv| {
            argv.first().map(|cmd| {
                std::path::Path::new(cmd)
                    .file_name()
                    .and_then(|os| os.to_str())
                    .unwrap_or(cmd.as_str())
                    .to_string()
            })
        });
        let resolved_title = title.or(auto_title);
        let terminal_id = self
            .spawn_terminal_session(cwd, persistent, command)
            .ok_or_else(|| "Failed to spawn terminal".to_string())?;

        // Register the leader pid with this window's process_groups
        // so window-level signal operations reach the spawned group.
        if let Some(pid) = self.terminal_manager.get(terminal_id).and_then(|h| h.pid()) {
            let label = format!("terminal #{}", terminal_id.0);
            self.process_groups.register(pid, label);
        }

        // Compute split-creation behaviour. The two cases (with /
        // without direction) diverge in whether we attach to the
        // active split as a new tab or create a fresh split off it.
        // The "never-activated, no layout yet" case is handled in
        // both branches by seeding a SplitManager rooted at the new
        // terminal buffer.
        let active_split = self.buffers.splits().map(|(mgr, _)| mgr.active_split());

        let (buffer_id, created_split_id) = if let Some(split_dir) = direction {
            let buffer_id = self.create_terminal_buffer_detached(terminal_id);
            match active_split {
                Some(parent) => {
                    let split_ratio = ratio.unwrap_or(0.5);
                    let line_numbers = self.resources.config.editor.line_numbers;
                    let highlight_current_line =
                        self.resources.config.editor.highlight_current_line;
                    let rulers = self.resources.config.editor.rulers.clone();
                    let terminal_width = self.terminal_width;
                    let terminal_height = self.terminal_height;
                    let split_result = self
                        .split_manager_mut()
                        .expect("active split implies populated layout")
                        .split_active(split_dir, buffer_id, split_ratio);
                    match split_result {
                        Ok(new_split_id) => {
                            let mut view_state = SplitViewState::with_buffer(
                                terminal_width,
                                terminal_height,
                                buffer_id,
                            );
                            // Terminal-dedicated splits never show
                            // line numbers or current-line highlight
                            // — the buffer is a PTY scrollback view,
                            // not source code. (Pre-fix the config
                            // default was applied, so a default-on
                            // line-numbers user saw `1 │ Python …`
                            // in every orchestrator agent split.)
                            // Other splits in the window aren't
                            // affected because each `SplitViewState`
                            // is independent.
                            let _ = line_numbers;
                            let _ = highlight_current_line;
                            view_state
                                .apply_config_defaults(false, false, false, false, None, rulers, 0);
                            // Terminal output is ANSI-sequenced and
                            // assumes a fixed column count; wrapping
                            // would mangle cursor positioning.
                            view_state.viewport.line_wrap_enabled = false;
                            self.split_view_states_mut()
                                .expect("active split implies populated layout")
                                .insert(new_split_id, view_state);
                            if focus {
                                self.split_manager_mut()
                                    .expect("active split implies populated layout")
                                    .set_active_split(new_split_id);
                            }
                            (buffer_id, Some(new_split_id))
                        }
                        Err(e) => {
                            tracing::error!(
                                "Failed to create split for terminal: {e}; \
                                 falling back to attaching to active split"
                            );
                            // Graceful fallback: attach to the active
                            // split so the buffer isn't orphaned.
                            if let Some(view_state) = self
                                .split_view_states_mut()
                                .and_then(|m| m.get_mut(&parent))
                            {
                                view_state.add_buffer(buffer_id);
                                view_state.viewport.line_wrap_enabled = false;
                            }
                            self.set_active_buffer(buffer_id);
                            (buffer_id, None)
                        }
                    }
                }
                None => {
                    // Never-activated window with no layout — seed
                    // one rooted at the terminal buffer. First dive
                    // picks it up and the terminal is the active leaf.
                    let manager = crate::view::split::SplitManager::new(buffer_id);
                    let active_leaf = manager.active_split();
                    let mut view_states = std::collections::HashMap::new();
                    let mut vs = SplitViewState::with_buffer(
                        self.terminal_width,
                        self.terminal_height,
                        buffer_id,
                    );
                    vs.viewport.line_wrap_enabled = false;
                    view_states.insert(active_leaf, vs);
                    self.buffers.set_splits((manager, view_states));
                    (buffer_id, Some(active_leaf))
                }
            }
        } else {
            match active_split {
                Some(split_id) => {
                    let buffer_id = self.create_terminal_buffer_attached(terminal_id, split_id);
                    // Switch tabs to the terminal. Window-side
                    // mutation only — the editor-wide
                    // `buffer_activated` hook is fired by the
                    // Editor wrapper iff this window is the
                    // editor-active one.
                    self.set_active_buffer(buffer_id);
                    (buffer_id, None)
                }
                None => {
                    let buffer_id = self.create_terminal_buffer_detached(terminal_id);
                    let manager = crate::view::split::SplitManager::new(buffer_id);
                    let active_leaf = manager.active_split();
                    let mut view_states = std::collections::HashMap::new();
                    let mut vs = SplitViewState::with_buffer(
                        self.terminal_width,
                        self.terminal_height,
                        buffer_id,
                    );
                    vs.viewport.line_wrap_enabled = false;
                    view_states.insert(active_leaf, vs);
                    self.buffers.set_splits((manager, view_states));
                    (buffer_id, Some(active_leaf))
                }
            }
        };

        // Override the auto-generated `*Terminal N*` display name
        // when the plugin requested an explicit title (or one was
        // derived from `command[0]`). Disambiguates against other
        // terminals in this window using a `name (k)` suffix so two
        // simultaneous python3 sessions read as "python3" and
        // "python3 (2)" instead of colliding.
        if let Some(title) = resolved_title {
            let final_name = self.disambiguate_terminal_title(&title, buffer_id);
            if let Some(meta) = self.buffer_metadata.get_mut(&buffer_id) {
                meta.display_name = final_name;
            }
        }

        // When the new terminal ended up as this window's active
        // buffer, switch the window into terminal mode so the live
        // grid renders immediately. Without this, the renderer
        // skips the grid (see `render_terminal_splits` — it defers
        // to the file-backed scrollback view whenever the active
        // tab is a terminal buffer but the window is not in
        // terminal mode) and the user sees a blank tab until the
        // next event flips `terminal_mode` — typically the next
        // printable keystroke via `should_enter_terminal_mode`.
        // Mirrors `open_terminal_in_window`'s post-spawn flip.
        if self.active_buffer() == buffer_id {
            self.terminal_mode = true;
            self.key_context = crate::input::keybindings::KeyContext::Terminal;
        }

        self.resize_visible_terminals();
        Ok((terminal_id, buffer_id, created_split_id))
    }

    /// Pick the next free `name (k)` variant of `desired` for this
    /// window's set of terminal buffers. `for_buffer` is the
    /// freshly-created buffer being titled — its own metadata is
    /// excluded from the scan so we don't collide with ourselves
    /// when callers pre-set it.
    ///
    /// Returns `desired` verbatim when no collision exists, otherwise
    /// `desired (2)`, `desired (3)`, … as needed.
    fn disambiguate_terminal_title(&self, desired: &str, for_buffer: BufferId) -> String {
        // Collect existing terminal-buffer display names that share
        // the desired prefix. Only inspect buffers that are actually
        // terminals — non-terminal buffers happen to use the same
        // metadata map but their names don't collide semantically.
        let used: std::collections::HashSet<&str> = self
            .terminal_buffers
            .keys()
            .filter(|bid| **bid != for_buffer)
            .filter_map(|bid| {
                self.buffer_metadata
                    .get(bid)
                    .map(|m| m.display_name.as_str())
            })
            .collect();
        if !used.contains(desired) {
            return desired.to_string();
        }
        // Linear scan from k=2 upward. Two simultaneous duplicates is
        // already rare; ten is unheard of, so the loop bound is fine.
        for k in 2..=1024 {
            let candidate = format!("{} ({})", desired, k);
            if !used.contains(candidate.as_str()) {
                return candidate;
            }
        }
        // Fall back to `desired (∞)` if for some reason 1024 names
        // are taken — still unique because the loop exhausted the
        // numeric variants we considered. Practically unreachable.
        format!("{} (n)", desired)
    }

    /// Update terminal buffers' tab titles from the title the running
    /// program set via OSC escape codes (OSC 0/1/2). Intended to run once
    /// per frame. When a terminal hasn't set a title (or only ever set an
    /// empty one), its buffer keeps whatever name it already had — the
    /// default `*Terminal N*` or a plugin-provided title.
    ///
    /// Titles are sanitized (control characters stripped, length capped)
    /// the same way as the host window title. OSC titles are applied
    /// verbatim without the `name (k)` disambiguation used for plugin
    /// titles: programs rewrite these frequently (e.g. to show the cwd or
    /// the running command) and re-disambiguating on every change would
    /// make the suffix flicker.
    pub fn sync_terminal_titles(&mut self) {
        // Snapshot (buffer, title) pairs first so the mutable
        // `buffer_metadata` borrow below doesn't overlap the immutable
        // `terminal_manager` / `terminal_buffers` borrows. Each terminal
        // state lock is taken and released within the closure.
        let updates: Vec<(BufferId, String)> = self
            .terminal_buffers
            .iter()
            .filter_map(|(buffer_id, terminal_id)| {
                let handle = self.terminal_manager.get(*terminal_id)?;
                let title = handle.state.lock().ok()?.title().to_string();
                if title.is_empty() {
                    return None;
                }
                let sanitized = crate::services::terminal_title::sanitize_title(&title);
                if sanitized.is_empty() {
                    return None;
                }
                Some((*buffer_id, sanitized))
            })
            .collect();

        for (buffer_id, title) in updates {
            if let Some(meta) = self.buffer_metadata.get_mut(&buffer_id) {
                if meta.display_name != title {
                    meta.display_name = title;
                }
            }
        }
    }

    /// Open a new terminal in this window: spawn the PTY, create
    /// the buffer, attach to the active split, switch this window's
    /// active buffer to it, enable terminal mode, and resize the PTY
    /// to match the split's content area. Returns `(terminal_id,
    /// buffer_id)` on success.
    ///
    /// Editor-wide effects (the `buffer_activated` plugin hook, the
    /// status-bar exit-key message) are NOT fired here — that's the
    /// caller's responsibility, gated on whether this window is the
    /// editor-active one. See `Editor::open_terminal` for the
    /// active-window wrapper that does both.
    pub fn open_terminal_in_window(&mut self) -> Option<(TerminalId, BufferId)> {
        // `None` command override — `Open Terminal` always spawns the
        // user's shell, never a one-off command. Plugin-driven
        // terminals route through `create_plugin_terminal` instead.
        let terminal_id = self.spawn_terminal_session(None, true, None)?;
        let split_id = self
            .buffers
            .splits()
            .map(|(mgr, _)| mgr.active_split())
            .expect("window must have a populated split layout");
        let buffer_id = self.create_terminal_buffer_attached(terminal_id, split_id);
        // Window-side activation: per-window mutation only — the
        // editor-wide plugin hook fires in the Editor wrapper.
        self.set_active_buffer(buffer_id);
        self.terminal_mode = true;
        self.key_context = crate::input::keybindings::KeyContext::Terminal;
        self.resize_visible_terminals();
        Some((terminal_id, buffer_id))
    }

    /// Create a buffer for a terminal session in this window without
    /// attaching to any split (used during session restore).
    pub fn create_terminal_buffer_detached(&mut self, terminal_id: TerminalId) -> BufferId {
        let buffer_id = self.alloc_buffer_id();
        let large_file_threshold = self.resources.config.editor.large_file_threshold_bytes as usize;

        let backing_file = self
            .terminal_backing_files
            .get(&terminal_id)
            .cloned()
            .unwrap_or_else(|| {
                let root = self.resources.dir_context.terminal_dir_for(&self.root);
                if let Err(e) = self.resources.authority.filesystem.create_dir_all(&root) {
                    tracing::warn!("Failed to create terminal directory: {}", e);
                }
                root.join(format!("fresh-terminal-{}.txt", terminal_id.0))
            });

        if !self.resources.authority.filesystem.exists(&backing_file) {
            if let Err(e) = self
                .resources
                .authority
                .filesystem
                .write_file(&backing_file, &[])
            {
                tracing::warn!("Failed to create terminal backing file: {}", e);
            }
        }

        let mut state = EditorState::new_with_path(
            large_file_threshold,
            std::sync::Arc::clone(&self.resources.authority.filesystem),
            backing_file.clone(),
        );
        state.margins.configure_for_line_numbers(false);
        self.buffers.insert(buffer_id, state);

        let metadata = BufferMetadata::virtual_buffer(
            format!("*Terminal {}*", terminal_id.0),
            "terminal".into(),
            false,
        );
        self.buffer_metadata.insert(buffer_id, metadata);
        self.terminal_buffers.insert(buffer_id, terminal_id);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        buffer_id
    }
}

impl Editor {
    /// Spawn a new PTY-backed terminal session in the active window
    /// using its `root` as cwd. Editor-side thin wrapper; per-window
    /// body lives in `Window::spawn_terminal_session`.
    ///
    /// Used by `open_terminal` (regular spawn into the active split)
    /// and by `Action::OpenTerminalInDock` (which needs the buffer
    /// id *before* it has a split to attach to, so the dock leaf can
    /// be seeded with the terminal directly rather than with a
    /// placeholder buffer that would linger as a phantom tab).
    pub(crate) fn spawn_terminal_session(&mut self) -> Option<TerminalId> {
        // No command override — see comment on `Window::open_terminal_in_window`.
        self.active_window_mut()
            .spawn_terminal_session(None, true, None)
    }

    /// Open a new terminal in the active window's current split, fire
    /// the editor-wide `buffer_activated` plugin hook, and post a
    /// status-bar message with the terminal-mode exit key.
    ///
    /// Window-side body lives in `Window::open_terminal_in_window`;
    /// this router adds only the cross-cutting effects that require
    /// editor-level state (the plugin hook + status message).
    pub fn open_terminal(&mut self) {
        let Some((terminal_id, buffer_id)) = self.active_window_mut().open_terminal_in_window()
        else {
            return;
        };

        // Editor-wide: refresh the plugin-state snapshot so plugin
        // hooks see the new active buffer, then fire `buffer_activated`.
        #[cfg(feature = "plugins")]
        self.update_plugin_state_snapshot();
        #[cfg(feature = "plugins")]
        self.plugin_manager.read().unwrap().run_hook(
            "buffer_activated",
            crate::services::plugins::hooks::HookArgs::BufferActivated { buffer_id },
        );

        // Status bar with the terminal-mode exit key. Looked up here
        // (not in Window) because the keybinding resolver is shared
        // editor state read through the `Arc<RwLock<…>>`.
        let exit_key = self
            .keybindings
            .read()
            .unwrap()
            .find_keybinding_for_action(
                "terminal_escape",
                crate::input::keybindings::KeyContext::Terminal,
            )
            .unwrap_or_else(|| "Ctrl+Space".to_string());
        self.set_status_message(
            t!("terminal.opened", id = terminal_id.0, exit_key = exit_key).to_string(),
        );
        tracing::info!(
            "Opened terminal {:?} with buffer {:?}",
            terminal_id,
            buffer_id
        );
    }

    /// Editor-side thin wrapper. Delegates to the active window's
    /// `Window::create_terminal_buffer_detached` (used during session
    /// restore by `input.rs`).
    pub(crate) fn create_terminal_buffer_detached(&mut self, terminal_id: TerminalId) -> BufferId {
        self.active_window_mut()
            .create_terminal_buffer_detached(terminal_id)
    }

    /// Close the current terminal (if viewing a terminal buffer)
    pub fn close_terminal(&mut self) {
        let buffer_id = self.active_buffer();

        if let Some(&terminal_id) = self.active_window().terminal_buffers.get(&buffer_id) {
            // Close the terminal
            self.active_window_mut().terminal_manager.close(terminal_id);
            self.active_window_mut().terminal_buffers.remove(&buffer_id);
            self.active_window_mut()
                .ephemeral_terminals
                .remove(&terminal_id);

            // Clean up backing/rendering file
            let backing_file = self
                .active_window_mut()
                .terminal_backing_files
                .remove(&terminal_id);
            if let Some(ref path) = backing_file {
                // Best-effort cleanup of temporary terminal files.
                #[allow(clippy::let_underscore_must_use)]
                let _ = self.authority.filesystem.remove_file(path);
            }
            // Clean up raw log file
            if let Some(log_file) = self
                .active_window_mut()
                .terminal_log_files
                .remove(&terminal_id)
            {
                if backing_file.as_ref() != Some(&log_file) {
                    // Best-effort cleanup of temporary terminal files.
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = self.authority.filesystem.remove_file(&log_file);
                }
            }

            // Exit terminal mode
            self.active_window_mut().terminal_mode = false;
            self.active_window_mut().key_context = crate::input::keybindings::KeyContext::Normal;

            // Close the buffer
            if let Err(e) = self.close_buffer(buffer_id) {
                tracing::warn!("Failed to close terminal buffer: {}", e);
            }

            self.set_status_message(t!("terminal.closed", id = terminal_id.0).to_string());
        } else {
            self.set_status_message(t!("status.not_viewing_terminal").to_string());
        }
    }

    // `is_terminal_buffer` and `get_terminal_id` moved to `impl Window`
    // (in `window.rs`). Editor callers reach them via
    // `self.active_window().is_terminal_buffer(...)` /
    // `.get_terminal_id(...)`.

    // `get_active_terminal_state`, `send_terminal_input`,
    // `send_terminal_key`, `send_terminal_mouse`, and
    // `is_terminal_in_alternate_screen` live on `impl Window` — they
    // only touch this window's `terminal_buffers` + `terminal_manager`.
    // Call them via `self.active_window()` / `self.active_window_mut()`.

    /// Handle terminal input when in terminal mode
    pub fn handle_terminal_key(
        &mut self,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> bool {
        // Check for escape sequences to exit terminal mode
        // Ctrl+Space, Ctrl+], or Ctrl+` to exit (Ctrl+\ sends SIGQUIT on Unix)
        if modifiers.contains(crossterm::event::KeyModifiers::CONTROL) {
            match code {
                crossterm::event::KeyCode::Char(' ')
                | crossterm::event::KeyCode::Char(']')
                | crossterm::event::KeyCode::Char('`') => {
                    // Exit terminal mode and sync buffer
                    self.active_window_mut().terminal_mode = false;
                    self.active_window_mut().key_context =
                        crate::input::keybindings::KeyContext::Normal;
                    {
                        let __b = self.active_buffer();
                        self.active_window_mut().sync_terminal_to_buffer(__b);
                    };
                    self.set_status_message(
                        "Terminal mode disabled - read only (Ctrl+Space to resume)".to_string(),
                    );
                    return true;
                }
                _ => {}
            }
        }

        // Send the key to the terminal
        self.active_window_mut().send_terminal_key(code, modifiers);
        true
    }

    /// Re-enter terminal mode from read-only buffer view
    ///
    /// This truncates the backing file to remove the visible screen tail
    /// that was appended when we exited terminal mode, leaving only the
    /// incrementally-streamed scrollback history.
    pub fn enter_terminal_mode(&mut self) {
        if self
            .active_window()
            .is_terminal_buffer(self.active_buffer())
        {
            self.active_window_mut().terminal_mode = true;
            self.active_window_mut().key_context = crate::input::keybindings::KeyContext::Terminal;

            // Re-enable editing when in terminal mode (input goes to PTY)
            let __buffer_id = self.active_buffer();
            if let Some(state) = self
                .windows
                .get_mut(&self.active_window)
                .map(|w| &mut w.buffers)
                .expect("active window present")
                .get_mut(&__buffer_id)
            {
                state.editing_disabled = false;
                state.margins.configure_for_line_numbers(false);
            }
            let __active_split = self.split_manager().active_split();
            if let Some(view_state) = self.split_view_states_mut().get_mut(&__active_split) {
                view_state.viewport.line_wrap_enabled = false;
            }

            // Truncate backing file to remove visible screen tail and scroll to bottom
            if let Some(&terminal_id) = self
                .active_window()
                .terminal_buffers
                .get(&self.active_buffer())
            {
                // Truncate backing file to remove visible screen that was appended
                if let Some(backing_path) = self
                    .active_window()
                    .terminal_backing_files
                    .get(&terminal_id)
                {
                    if let Some(handle) = self.active_window().terminal_manager.get(terminal_id) {
                        if let Ok(state) = handle.state.lock() {
                            let truncate_pos = state.backing_file_history_end();
                            // Always truncate to remove appended visible screen
                            // (even if truncate_pos is 0, meaning no scrollback yet)
                            if let Err(e) = self
                                .authority
                                .filesystem
                                .set_file_length(backing_path, truncate_pos)
                            {
                                tracing::warn!("Failed to truncate terminal backing file: {}", e);
                            }
                        }
                    }
                }

                // Scroll terminal to bottom when re-entering
                if let Some(handle) = self.active_window().terminal_manager.get(terminal_id) {
                    if let Ok(mut state) = handle.state.lock() {
                        state.scroll_to_bottom();
                    }
                }
            }

            // Ensure terminal PTY is sized correctly for current split dimensions
            self.active_window_mut().resize_visible_terminals();

            self.set_status_message(t!("status.terminal_mode_enabled").to_string());
        }
    }

    /// Get terminal content for rendering
    pub fn get_terminal_content(
        &self,
        buffer_id: BufferId,
    ) -> Option<Vec<Vec<crate::services::terminal::TerminalCell>>> {
        let terminal_id = self.active_window().terminal_buffers.get(&buffer_id)?;
        let handle = self.active_window().terminal_manager.get(*terminal_id)?;
        let state = handle.state.lock().ok()?;

        let (_, rows) = state.size();
        let mut content = Vec::with_capacity(rows as usize);

        for row in 0..rows {
            content.push(state.get_line(row));
        }

        Some(content)
    }
}

impl Window {
    /// Get the terminal state for the active buffer (if it's a terminal buffer).
    pub fn get_active_terminal_state(
        &self,
    ) -> Option<std::sync::MutexGuard<'_, crate::services::terminal::TerminalState>> {
        let terminal_id = self.terminal_buffers.get(&self.active_buffer())?;
        let handle = self.terminal_manager.get(*terminal_id)?;
        handle.state.lock().ok()
    }

    /// Send input bytes to this window's active terminal (no-op if the
    /// active buffer is not a terminal).
    pub fn send_terminal_input(&mut self, data: &[u8]) {
        if let Some(&terminal_id) = self.terminal_buffers.get(&self.active_buffer()) {
            if let Some(handle) = self.terminal_manager.get(terminal_id) {
                handle.write(data);
            }
        }
    }

    /// Send a key event to this window's active terminal. Picks
    /// "application cursor" vs "normal cursor" escape sequences
    /// based on the terminal's current state.
    pub fn send_terminal_key(
        &mut self,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) {
        let app_cursor = self
            .get_active_terminal_state()
            .map(|s| s.is_app_cursor())
            .unwrap_or(false);
        if let Some(bytes) =
            crate::services::terminal::pty::key_to_pty_bytes(code, modifiers, app_cursor)
        {
            self.send_terminal_input(&bytes);
        }
    }

    /// Send a mouse event to this window's active terminal.
    pub fn send_terminal_mouse(
        &mut self,
        col: u16,
        row: u16,
        kind: crate::input::handler::TerminalMouseEventKind,
        modifiers: crossterm::event::KeyModifiers,
    ) {
        use crate::input::handler::TerminalMouseEventKind;

        // Check if terminal uses SGR mouse encoding.
        let use_sgr = self
            .get_active_terminal_state()
            .map(|s| s.uses_sgr_mouse())
            .unwrap_or(true);

        // For alternate scroll mode, convert scroll to arrow keys.
        let uses_alt_scroll = self
            .get_active_terminal_state()
            .map(|s| s.uses_alternate_scroll())
            .unwrap_or(false);

        if uses_alt_scroll {
            match kind {
                TerminalMouseEventKind::ScrollUp => {
                    for _ in 0..3 {
                        self.send_terminal_input(b"\x1b[A");
                    }
                    return;
                }
                TerminalMouseEventKind::ScrollDown => {
                    for _ in 0..3 {
                        self.send_terminal_input(b"\x1b[B");
                    }
                    return;
                }
                _ => {}
            }
        }

        let bytes = if use_sgr {
            encode_sgr_mouse(col, row, kind, modifiers)
        } else {
            encode_x10_mouse(col, row, kind, modifiers)
        };

        if let Some(bytes) = bytes {
            self.send_terminal_input(&bytes);
        }
    }

    /// Check if the given terminal buffer in this window is in
    /// alternate-screen mode (vim/less/htop etc.).
    pub fn is_terminal_in_alternate_screen(&self, buffer_id: BufferId) -> bool {
        if let Some(&terminal_id) = self.terminal_buffers.get(&buffer_id) {
            if let Some(handle) = self.terminal_manager.get(terminal_id) {
                if let Ok(state) = handle.state.lock() {
                    return state.is_alternate_screen();
                }
            }
        }
        false
    }

    /// Resize a single terminal buffer's PTY (only if `buffer_id`
    /// belongs to this window's terminal_buffers map).
    pub fn resize_terminal(&mut self, buffer_id: BufferId, cols: u16, rows: u16) {
        if let Some(&terminal_id) = self.terminal_buffers.get(&buffer_id) {
            if let Some(handle) = self.terminal_manager.get_mut(terminal_id) {
                handle.resize(cols, rows);
            }
        }
    }

    /// The rect the editor splits lay out into, mirroring the renderer
    /// (`render.rs::compute_dock_split` + the file-explorer split): the
    /// editor-global dock claims the leftmost `dock_cols`, then the file
    /// explorer claims a slice of the remaining chrome, and the splits get
    /// what's left. `dock_cols` is pushed down by `Editor::relayout`.
    /// Computing the file-explorer width against the post-dock chrome
    /// width (not the full screen) matches the renderer exactly, so split
    /// geometry derived from this lines up with the cells actually drawn.
    pub(crate) fn editor_content_area(&self) -> ratatui::layout::Rect {
        let chrome_width = self.terminal_width.saturating_sub(self.dock_cols);
        let file_explorer_width = if self.file_explorer_visible {
            self.file_explorer_width.to_cols(chrome_width)
        } else {
            0
        };
        let editor_x = match self.file_explorer_side {
            crate::config::FileExplorerSide::Left => {
                self.dock_cols.saturating_add(file_explorer_width)
            }
            crate::config::FileExplorerSide::Right => self.dock_cols,
        };
        let editor_width = chrome_width.saturating_sub(file_explorer_width);
        ratatui::layout::Rect::new(
            editor_x,
            1, // menu bar
            editor_width,
            self.terminal_height.saturating_sub(2), // menu bar + status bar
        )
    }

    /// Resize all this window's visible terminal PTYs to match their
    /// current split dimensions. Reads the window's cached
    /// `terminal_width` / `terminal_height` for the screen size.
    pub fn resize_visible_terminals(&mut self) {
        let editor_area = self.editor_content_area();

        let Some((mgr, _)) = self.buffers.splits() else {
            return;
        };
        let visible_buffers = mgr.get_visible_buffers(editor_area);

        for (_split_id, buffer_id, split_area) in visible_buffers {
            if self.terminal_buffers.contains_key(&buffer_id) {
                // Tab bar takes 1 row, scrollbar takes 1 column on the right.
                let content_height = split_area.height.saturating_sub(2);
                let content_width = split_area.width.saturating_sub(2);

                if content_width > 0 && content_height > 0 {
                    self.resize_terminal(buffer_id, content_width, content_height);
                }
            }
        }
    }

    /// Sync terminal content to the active terminal buffer's text view
    /// for read-only viewing / selection.
    ///
    /// Incremental streaming architecture:
    /// 1. Scrollback has already been streamed to the backing file during PTY reads.
    /// 2. We append the visible screen (~50 lines) to the backing file.
    /// 3. Reload the buffer from the backing file (lazy load for large files).
    ///
    /// Performance: O(screen_size) instead of O(total_history).
    pub fn sync_terminal_to_buffer(&mut self, buffer_id: BufferId) {
        let Some(&terminal_id) = self.terminal_buffers.get(&buffer_id) else {
            return;
        };
        // Get the backing file path
        let backing_file = match self.terminal_backing_files.get(&terminal_id) {
            Some(path) => path.clone(),
            None => return,
        };

        // Append visible screen to backing file
        // The scrollback has already been incrementally streamed by the PTY read loop.
        // Capture the file size *just before* the append so the viewport
        // can anchor to it below — that byte offset is the first byte of
        // the visible screen we're about to append, which is exactly
        // where the live PTY grid drew its row 0.
        let mut history_end_byte: Option<u64> = None;
        if let Some(handle) = self.terminal_manager.get(terminal_id) {
            if let Ok(mut state) = handle.state.lock() {
                // Record the current file size as the history end point
                // (before appending visible screen) so we can truncate back to it
                if let Ok(metadata) = self.resources.authority.filesystem.metadata(&backing_file) {
                    state.set_backing_file_history_end(metadata.size);
                    history_end_byte = Some(metadata.size);
                }

                // Open backing file in append mode to add visible screen
                if let Ok(mut file) = self
                    .resources
                    .authority
                    .filesystem
                    .open_file_for_append(&backing_file)
                {
                    use std::io::BufWriter;
                    let mut writer = BufWriter::new(&mut *file);
                    if let Err(e) = state.append_visible_screen(&mut writer) {
                        tracing::error!("Failed to append visible screen to backing file: {}", e);
                    }
                }
            }
        }

        // Reload buffer from the backing file (reusing existing file loading)
        let large_file_threshold = self.resources.config.editor.large_file_threshold_bytes as usize;
        if let Ok(new_state) = EditorState::from_file_with_languages(
            &backing_file,
            self.terminal_width,
            self.terminal_height,
            large_file_threshold,
            &self.resources.grammar_registry,
            &self.resources.config.languages,
            std::sync::Arc::clone(&self.resources.authority.filesystem),
        ) {
            let total_bytes = new_state.buffer.total_bytes();
            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                *state = new_state;
                // Terminal buffers should never be considered "modified"
                state.buffer.set_modified(false);
            }
            // Anchor the viewport at the first byte of the appended
            // visible screen and place the cursor there too. The scroll-
            // back view now opens with the just-appended PTY rows at the
            // top — exactly where the live grid drew them — so exit is
            // pixel-identical to the last terminal-mode tick even when
            // most of the screen is blank (post-`clear` / `reset`). The
            // old `cursor = total_bytes` + `ensure_cursor_visible` path
            // anchored the bottom row instead, which pulled older
            // scrollback into rows the PTY had drawn blank.
            let anchor_byte = history_end_byte
                .map(|h| (h as usize).min(total_bytes))
                .unwrap_or(total_bytes);
            if let Some((mgr, view_states)) = self.buffers.splits_mut() {
                let active_split = mgr.active_split();
                if let Some(view_state) = view_states.get_mut(&active_split) {
                    view_state.cursors.primary_mut().position = anchor_byte;
                    view_state.viewport.top_byte = anchor_byte;
                    view_state.viewport.top_view_line_offset = 0;
                    view_state.viewport.left_column = 0;
                }
            }
        }

        // Mark buffer as editing-disabled while in non-terminal mode
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.editing_disabled = true;
            state.margins.configure_for_line_numbers(false);
        }

        // Refresh line-wrap state for the scroll-back view and arm the
        // skip_ensure_visible flag so the next render does *not* run
        // `Viewport::ensure_visible` against the cursor we just pinned.
        // Without this the renderer would notice that the cursor sits
        // on the viewport's top row, treat that as "above the scroll
        // margin", and scroll `top_byte` up by `scroll_offset` lines —
        // pulling pre-existing scrollback above the appended visible
        // screen and undoing the anchor. The flag is consumed
        // (cleared) by the first navigation / scroll action, so normal
        // scrolling still works after that.
        //
        // Also force the per-buffer gutter / current-line-highlight off
        // here as the exit-path's last line of defense. Spawn /
        // workspace-restore code paths each have their own setup, and a
        // single missed spot leaks a gutter pop-in on exit — pinning
        // them on this path covers any terminal regardless of how its
        // view state was created.
        if let Some((mgr, view_states)) = self.buffers.splits_mut() {
            let active_split = mgr.active_split();
            // The active split's view state may not yet have a keyed
            // entry for the terminal buffer (e.g. user just pressed
            // Alt+] into a split that has the terminal as a tab but
            // never displayed it before). ensure_buffer_state will
            // create one with defaults (show_line_numbers=true) the
            // very first time — so we have to *immediately* override
            // those defaults here, otherwise the next render flashes
            // a gutter for restored terminals.
            //
            // Also force the gutter / current-line-highlight off on
            // every other split that has this terminal as a tab. A
            // single missed BufferViewState (e.g. created lazily by
            // workspace restore + Alt+]) leaks a gutter pop-in.
            for vs in view_states.values_mut() {
                if vs.has_buffer(buffer_id) {
                    let buf_state = vs.ensure_buffer_state(buffer_id);
                    buf_state.show_line_numbers = false;
                    buf_state.highlight_current_line = false;
                    buf_state.viewport.line_wrap_enabled = false;
                }
            }
            if let Some(view_state) = view_states.get_mut(&active_split) {
                view_state.viewport.line_wrap_enabled = false;
                view_state.viewport.set_skip_ensure_visible();
                let buf_state = view_state.ensure_buffer_state(buffer_id);
                buf_state.show_line_numbers = false;
                buf_state.highlight_current_line = false;
            }
        }
    }

    /// Render terminal content for terminal buffers in this window's
    /// split areas. Overlays the live PTY grid (colors, attributes,
    /// optional cursor) on top of the buffer's regular text content
    /// inside `content_rect`.
    ///
    /// `cursor_visible_if_active` controls whether the cursor is
    /// painted at all. The active-window render passes `true` so a
    /// focused terminal in `terminal_mode` blinks normally; the
    /// preview path passes `false` so the picker preview stays
    /// read-only.
    ///
    /// Window-local in every respect — reads `terminal_buffers`,
    /// `terminal_manager`, `terminal_mode`, `active_buffer()`, and
    /// `resources.theme` from `self`. The caller picks the window
    /// (active vs previewed); this method never reaches back to an
    /// `Editor` or to any other window.
    pub fn render_terminal_splits(
        &self,
        frame: &mut ratatui::Frame,
        split_areas: &[(
            crate::model::event::LeafId,
            BufferId,
            ratatui::layout::Rect,
            ratatui::layout::Rect,
            usize,
            usize,
        )],
        cursor_visible_if_active: bool,
    ) {
        for (_split_id, buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            split_areas
        {
            let Some(&terminal_id) = self.terminal_buffers.get(buffer_id) else {
                continue;
            };
            // When the user's current tab is a terminal but they're
            // *not* in terminal mode, the buffer is showing the
            // synced scrollback view — defer to the normal text
            // rendering so the user can scroll. The live grid only
            // overlays when terminal mode is active, or when the
            // tab isn't the active one (so a split's hidden tab
            // still gets live updates).
            let is_active = *buffer_id == self.active_buffer();
            if is_active && !self.terminal_mode {
                continue;
            }
            let Some(handle) = self.terminal_manager.get(terminal_id) else {
                continue;
            };
            let Ok(state) = handle.state.lock() else {
                continue;
            };
            let cursor_pos = state.cursor_position();
            let cursor_visible = state.cursor_visible()
                && is_active
                && self.terminal_mode
                && cursor_visible_if_active;
            let (_, rows) = state.size();
            let mut content = Vec::with_capacity(rows as usize);
            for row in 0..rows {
                content.push(state.get_line(row));
            }
            frame.render_widget(ratatui::widgets::Clear, *content_rect);
            let theme = self.resources.theme.read().unwrap();
            render::render_terminal_content(
                &content,
                cursor_pos,
                cursor_visible,
                *content_rect,
                frame.buffer_mut(),
                theme.terminal_fg,
                theme.terminal_bg,
            );
        }
    }
}

impl Editor {
    /// Check if terminal mode is active (for testing)
    pub fn is_terminal_mode(&self) -> bool {
        self.active_window().terminal_mode
    }

    /// Check if a buffer is in terminal_mode_resume set (for testing/debugging)
    pub fn is_in_terminal_mode_resume(&self, buffer_id: BufferId) -> bool {
        self.active_window()
            .terminal_mode_resume
            .contains(&buffer_id)
    }

    /// Check if keyboard capture is enabled in terminal mode (for testing)
    pub fn is_keyboard_capture(&self) -> bool {
        self.active_window().keyboard_capture
    }

    /// Set terminal jump_to_end_on_output config option (for testing)
    pub fn set_terminal_jump_to_end_on_output(&mut self, value: bool) {
        self.config_mut().terminal.jump_to_end_on_output = value;
    }

    /// Get read-only access to the active window's terminal manager
    /// (for testing). After Step 0d, terminal state lives on each
    /// window — this routes to the active one.
    pub fn terminal_manager(&self) -> &crate::services::terminal::TerminalManager {
        &self
            .windows
            .get(&self.active_window)
            .expect("active window must exist")
            .terminal_manager
    }

    /// Get read-only access to the active window's terminal backing
    /// files map (for testing).
    pub fn terminal_backing_files(
        &self,
    ) -> &std::collections::HashMap<crate::services::terminal::TerminalId, std::path::PathBuf> {
        &self
            .windows
            .get(&self.active_window)
            .expect("active window must exist")
            .terminal_backing_files
    }

    /// Get the currently active buffer ID
    pub fn active_buffer_id(&self) -> BufferId {
        self.active_buffer()
    }

    /// Get buffer content as a string (for testing)
    pub fn get_buffer_content(&self, buffer_id: BufferId) -> Option<String> {
        self.windows
            .get(&self.active_window)
            .map(|w| &w.buffers)
            .expect("active window present")
            .get(&buffer_id)
            .and_then(|state| state.buffer.to_string())
    }

    /// Get cursor position for a buffer (for testing)
    pub fn get_cursor_position(&self, buffer_id: BufferId) -> Option<usize> {
        // Find cursor from any split view state that has this buffer
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .values()
            .find_map(|vs| {
                if vs.keyed_states.contains_key(&buffer_id) {
                    Some(vs.keyed_states.get(&buffer_id)?.cursors.primary().position)
                } else {
                    None
                }
            })
            .or_else(|| {
                // Fallback: check active cursors
                self.windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .values()
                    .map(|vs| vs.cursors.primary().position)
                    .next()
            })
    }

    // `render_terminal_splits` moved to `impl Window`. Active-window
    // callers reach it via `self.active_window().render_terminal_splits(...)`;
    // the picker preview path reaches it via the previewed window
    // directly, so the live PTY grid renders into the preview embed
    // without going through the active-window state.
}

/// Terminal rendering utilities
pub mod render {
    use crate::services::terminal::TerminalCell;
    use ratatui::buffer::Buffer;
    use ratatui::layout::Rect;
    use ratatui::style::{Color, Modifier, Style};

    /// Render terminal content to a ratatui buffer
    pub fn render_terminal_content(
        content: &[Vec<TerminalCell>],
        cursor_pos: (u16, u16),
        cursor_visible: bool,
        area: Rect,
        buf: &mut Buffer,
        default_fg: Color,
        default_bg: Color,
    ) {
        // Fill the rendered area with the theme's terminal bg first so any
        // cells past the PTY grid (e.g. transiently smaller than the rect
        // mid-resize) show the theme background rather than leaking the
        // host terminal's default bg. Issue #1890.
        buf.set_style(area, Style::default().fg(default_fg).bg(default_bg));

        for (row_idx, row) in content.iter().enumerate() {
            if row_idx as u16 >= area.height {
                break;
            }

            let y = area.y + row_idx as u16;

            for (col_idx, cell) in row.iter().enumerate() {
                if col_idx as u16 >= area.width {
                    break;
                }

                let x = area.x + col_idx as u16;

                // Build style from cell attributes, using theme defaults
                let mut style = Style::default().fg(default_fg).bg(default_bg);

                // Override with cell-specific colors if present
                if let Some((r, g, b)) = cell.fg {
                    style = style.fg(Color::Rgb(r, g, b));
                }

                if let Some((r, g, b)) = cell.bg {
                    style = style.bg(Color::Rgb(r, g, b));
                }

                // Apply modifiers
                if cell.bold {
                    style = style.add_modifier(Modifier::BOLD);
                }
                if cell.italic {
                    style = style.add_modifier(Modifier::ITALIC);
                }
                if cell.underline {
                    style = style.add_modifier(Modifier::UNDERLINED);
                }
                if cell.inverse {
                    style = style.add_modifier(Modifier::REVERSED);
                }

                // Check if this is the cursor position
                if cursor_visible
                    && row_idx as u16 == cursor_pos.1
                    && col_idx as u16 == cursor_pos.0
                {
                    style = style.add_modifier(Modifier::REVERSED);
                }

                buf.set_string(x, y, cell.c.to_string(), style);
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use crate::services::terminal::TerminalCell;

        #[test]
        fn cells_past_pty_grid_get_theme_bg() {
            // PTY grid is 2x2, render area is 4x3 — the cells outside
            // the grid must still carry the theme's terminal_bg so the
            // nostalgia theme's blue fully covers the terminal pane
            // (issue #1890).
            let area = Rect::new(0, 0, 4, 3);
            let mut buf = Buffer::empty(area);
            let row = vec![TerminalCell::default(), TerminalCell::default()];
            let content = vec![row.clone(), row];

            let default_bg = Color::Rgb(0, 0, 170);
            let default_fg = Color::Rgb(255, 255, 85);

            render_terminal_content(
                &content,
                (0, 0),
                false,
                area,
                &mut buf,
                default_fg,
                default_bg,
            );

            for y in area.top()..area.bottom() {
                for x in area.left()..area.right() {
                    assert_eq!(
                        buf[(x, y)].bg,
                        default_bg,
                        "cell ({x}, {y}) bg should be the theme terminal_bg",
                    );
                }
            }
        }
    }
}

/// Encode a mouse event in SGR format (modern protocol).
/// Format: CSI < Cb ; Cx ; Cy M (press) or CSI < Cb ; Cx ; Cy m (release)
fn encode_sgr_mouse(
    col: u16,
    row: u16,
    kind: crate::input::handler::TerminalMouseEventKind,
    modifiers: crossterm::event::KeyModifiers,
) -> Option<Vec<u8>> {
    use crate::input::handler::{TerminalMouseButton, TerminalMouseEventKind};

    // SGR uses 1-based coordinates
    let cx = col + 1;
    let cy = row + 1;

    // Build button code
    let (button_code, is_release) = match kind {
        TerminalMouseEventKind::Down(btn) => {
            let code = match btn {
                TerminalMouseButton::Left => 0,
                TerminalMouseButton::Middle => 1,
                TerminalMouseButton::Right => 2,
            };
            (code, false)
        }
        TerminalMouseEventKind::Up(btn) => {
            let code = match btn {
                TerminalMouseButton::Left => 0,
                TerminalMouseButton::Middle => 1,
                TerminalMouseButton::Right => 2,
            };
            (code, true)
        }
        TerminalMouseEventKind::Drag(btn) => {
            let code = match btn {
                TerminalMouseButton::Left => 32,   // 0 + 32 (motion flag)
                TerminalMouseButton::Middle => 33, // 1 + 32
                TerminalMouseButton::Right => 34,  // 2 + 32
            };
            (code, false)
        }
        TerminalMouseEventKind::Moved => (35, false), // 3 + 32 (no button + motion)
        TerminalMouseEventKind::ScrollUp => (64, false),
        TerminalMouseEventKind::ScrollDown => (65, false),
    };

    // Add modifier flags
    let mut cb = button_code;
    if modifiers.contains(crossterm::event::KeyModifiers::SHIFT) {
        cb += 4;
    }
    if modifiers.contains(crossterm::event::KeyModifiers::ALT) {
        cb += 8;
    }
    if modifiers.contains(crossterm::event::KeyModifiers::CONTROL) {
        cb += 16;
    }

    // Build escape sequence
    let terminator = if is_release { 'm' } else { 'M' };
    Some(format!("\x1b[<{};{};{}{}", cb, cx, cy, terminator).into_bytes())
}

/// Encode a mouse event in X10/normal format (legacy protocol).
/// Format: CSI M Cb Cx Cy (with 32 added to all values for ASCII safety)
fn encode_x10_mouse(
    col: u16,
    row: u16,
    kind: crate::input::handler::TerminalMouseEventKind,
    modifiers: crossterm::event::KeyModifiers,
) -> Option<Vec<u8>> {
    use crate::input::handler::{TerminalMouseButton, TerminalMouseEventKind};

    // X10 uses 1-based coordinates with 32 offset for ASCII safety
    // Maximum coordinate is 223 (255 - 32)
    let cx = (col.min(222) + 1 + 32) as u8;
    let cy = (row.min(222) + 1 + 32) as u8;

    // Build button code
    let button_code: u8 = match kind {
        TerminalMouseEventKind::Down(btn) | TerminalMouseEventKind::Drag(btn) => match btn {
            TerminalMouseButton::Left => 0,
            TerminalMouseButton::Middle => 1,
            TerminalMouseButton::Right => 2,
        },
        TerminalMouseEventKind::Up(_) => 3, // Release is button 3 in X10
        TerminalMouseEventKind::Moved => 3 + 32,
        TerminalMouseEventKind::ScrollUp => 64,
        TerminalMouseEventKind::ScrollDown => 65,
    };

    // Add modifier flags and motion flag for drag
    let mut cb = button_code;
    if matches!(kind, TerminalMouseEventKind::Drag(_)) {
        cb += 32; // Motion flag
    }
    if modifiers.contains(crossterm::event::KeyModifiers::SHIFT) {
        cb += 4;
    }
    if modifiers.contains(crossterm::event::KeyModifiers::ALT) {
        cb += 8;
    }
    if modifiers.contains(crossterm::event::KeyModifiers::CONTROL) {
        cb += 16;
    }

    // Add 32 offset for ASCII safety
    let cb = cb + 32;

    Some(vec![0x1b, b'[', b'M', cb, cx, cy])
}
