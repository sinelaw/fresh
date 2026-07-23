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

use super::window::{TerminalBuffer, Window};
use super::{BufferId, BufferMetadata, Editor};
use crate::model::event::LeafId;
use crate::services::authority::TerminalWrapper;
use crate::services::terminal::TerminalId;
use crate::state::EditorState;
use crate::view::split::SplitViewState;
use rust_i18n::t;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

/// Filesystem for terminal scrollback backing/log files.
///
/// The integrated terminal's PTY is always spawned on the **local** host —
/// even an SSH terminal runs `ssh` as a *local* child process — and the PTY
/// read loop renders scrollback into the backing file on the local disk via
/// `std::fs` (`services/terminal/manager.rs`). Those files therefore always
/// live on the local machine, at a path under the local `data_dir`,
/// independent of the session's (possibly remote) authority filesystem.
///
/// Routing their create / append / truncate / read through the *remote*
/// authority filesystem (issue #2424) made every scrollback-mode toggle do a
/// blocking SSH round-trip against a path that only exists locally: it hung
/// the UI for the round-trip and failed with "Failed to truncate terminal
/// backing file", leaving the scrollback view empty. Always use this local
/// handle for terminal backing/log files so it stays consistent with the read
/// loop. This honours the "use the `FileSystem` trait" rule (it returns a
/// trait object, never raw `std::fs` at the call site) while pinning the
/// backend to local — the correct backend for a local artifact.
pub(crate) fn terminal_backing_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
    Arc::new(crate::model::filesystem::StdFileSystem)
}

/// How often [`Window::sync_terminal_titles`] polls each terminal's
/// foreground process group for tmux-style tab auto-naming. Frequent enough
/// to feel responsive when a command starts/exits, infrequent enough that
/// the per-terminal `tcgetpgrp` + `/proc` read is negligible. Also drives
/// the editor's periodic-redraw deadline so the tab refreshes while idle.
pub(crate) const FG_POLL_INTERVAL: std::time::Duration = std::time::Duration::from_millis(1000);

/// Combine the foreground process name with the program's OSC title into one
/// tab label. The command leads (short, answers "what's running"); the OSC
/// title follows as context, e.g. `python3 — root@host: ~/proj`.
///
/// Returns `None` only when both are absent, so the caller falls back to the
/// default name. When the OSC title already names the command (e.g. vim's
/// `file - VIM`), the command isn't prepended again to avoid `vim — … VIM`.
pub(crate) fn combine_terminal_title(pty: Option<&str>, osc: Option<&str>) -> Option<String> {
    match (pty, osc) {
        (Some(p), Some(o)) => {
            if o.to_lowercase().contains(&p.to_lowercase()) {
                Some(o.to_string())
            } else {
                Some(format!("{p} \u{2014} {o}"))
            }
        }
        (Some(p), None) => Some(p.to_string()),
        (None, Some(o)) => Some(o.to_string()),
        (None, None) => None,
    }
}

/// Spawn options for [`Window::create_plugin_terminal`], grouped so the
/// call takes one argument instead of seven.
pub struct PluginTerminalSpec {
    pub cwd: Option<PathBuf>,
    pub direction: Option<crate::model::event::SplitDirection>,
    pub ratio: Option<f32>,
    /// Split focus: whether the new terminal becomes the active leaf.
    pub focus: bool,
    pub persistent: bool,
    pub command: Option<Vec<String>>,
    pub title: Option<String>,
    /// Extra environment variables applied to the terminal's child
    /// process, on top of the inherited + activated env. Applied after
    /// the control vars (`TERM`, `FRESH_SESSION`). Empty adds nothing.
    pub env: HashMap<String, String>,
}

/// Assemble the extra env for a terminal that hosts an agent which may drive
/// the editor from its shell.
///
/// Always advertises `FRESH_BIN` — this editor's own executable path — so a
/// nested `fresh` or a Fresh-CLI-taught agent invokes the EXACT build running
/// here, never some other `fresh` earlier on PATH (its `--cmd` verbs then match
/// this build). This duplicates what the terminal manager already injects
/// universally, on purpose: it guarantees `FRESH_BIN` rides the child's env
/// regardless of the manager's own cwd/socket state.
///
/// When `command_allowlist` is `Some`, mints an unforgeable capability token
/// bound to `window` plus that allowlist and injects it as `FRESH_CMD_TOKEN`,
/// alongside `FRESH_SESSION` — ensuring the control socket is listening first
/// so a token never ships without a session to talk to. Token minting is
/// deliberately caller-driven (opt-in via the allowlist), never blanket: a
/// standing editor-driving capability belongs only in terminals a caller
/// explicitly grants it to, not in every spawned subprocess.
///
/// Shared by `create_window_with_terminal` (agents born in a new window) and
/// `handle_create_terminal` (agents spawned into an existing window) so both
/// paths mint and inject identically. `base_env` seeds the map (plugin-supplied
/// env, empty when omitted).
pub(crate) fn agent_command_env(
    window: fresh_core::WindowId,
    base_env: Option<HashMap<String, String>>,
    command_allowlist: Option<Vec<String>>,
) -> HashMap<String, String> {
    let mut env = base_env.unwrap_or_default();
    if let Ok(exe) = std::env::current_exe() {
        if let Some(exe) = exe.to_str() {
            env.insert("FRESH_BIN".to_string(), exe.to_string());
        }
    }
    if let Some(allowlist) = command_allowlist {
        if let Ok(session_id) = crate::server::local_control::start() {
            env.insert("FRESH_SESSION".to_string(), session_id.to_string());
        }
        let token = crate::server::command_access::mint(crate::server::command_access::Grant::new(
            Some(window.0),
            allowlist,
        ));
        env.insert("FRESH_CMD_TOKEN".to_string(), token);
    }
    env
}

impl Window {
    /// Resolve the terminal wrapper used to spawn a new integrated
    /// terminal in this window, applying the `terminal.shell` config
    /// override on top of the authority's wrapper when appropriate.
    ///
    /// See `TerminalWrapper::with_user_shell_override` for the override
    /// rules; this is just the per-window wiring that supplies the
    /// active config.
    pub(crate) fn resolved_terminal_wrapper(&self) -> TerminalWrapper {
        self.authority()
            .terminal_wrapper
            .clone()
            .with_user_shell_override(self.resources.config.terminal.shell.as_ref())
    }

    /// The activated-environment delta (venv/direnv/mise) to apply to a newly
    /// spawned terminal, so it inherits the same env that LSP servers and
    /// `spawnProcess` already get (issue #2355; see
    /// docs/internal/uniform-env-activation-design.md). Captured only for a
    /// **local** host shell: `manages_cwd` marks docker/ssh-style wrappers whose
    /// inner shell runs on another host, where this locally-captured delta would
    /// be both wrong and unreachable (the env this `CommandBuilder` sets lands on
    /// the `docker`/`ssh` client process, not the remote shell). Those backends
    /// apply their own delta in the wrapper (the per-backend apply paths in the
    /// design doc). Empty when no env is active or capture fails — the terminal
    /// degrades to the inherited env exactly as before.
    pub(crate) fn terminal_env_delta(
        &self,
        wrapper: &TerminalWrapper,
    ) -> crate::services::env_provider::EnvDelta {
        if wrapper.manages_cwd {
            return crate::services::env_provider::EnvDelta::default();
        }
        self.authority().env_provider.current_local_delta_blocking()
    }

    /// Apply the activated environment to a *re-parented* terminal wrapper
    /// (SSH / container), the remote counterpart of [`Self::terminal_env_delta`]
    /// (which handles the local host shell via `CommandBuilder.env`). For SSH,
    /// rewrite the remote login-shell `exec` into a python3 launcher that
    /// captures + applies the activation on the remote before handing off to the
    /// user's shell, so the SSH terminal sees the same env LSP/`spawnProcess`
    /// already get (issue #2355). Returns the wrapper unchanged when no env is
    /// active or the wrapper isn't an SSH re-parent. (Container backends apply
    /// their captured env through their own wrapper flags; see the design doc.)
    pub(crate) fn apply_remote_terminal_env(
        &self,
        mut wrapper: TerminalWrapper,
    ) -> TerminalWrapper {
        use crate::services::remote::{ssh_remote_env_launcher, SSH_EXEC_LOGIN_SHELL};

        if wrapper.command == "ssh" && self.authority().env_provider.is_active() {
            let recipe = self.authority().env_provider.snippet();
            if let Some(last) = wrapper.args.last_mut() {
                if last.contains(SSH_EXEC_LOGIN_SHELL) {
                    // The SSH terminal command is `exec sh -c '<script>'`
                    // (see `build_ssh_remote_args`); the exec tail we're
                    // replacing lives *inside* that single-quoted literal, so
                    // the launcher's own single quotes must be re-quoted as
                    // `'\''` to stay within the literal instead of closing it.
                    let launcher = ssh_remote_env_launcher(&recipe).replace('\'', "'\\''");
                    *last = last.replace(SSH_EXEC_LOGIN_SHELL, &launcher);
                }
            }
        }
        wrapper
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
        extra_env: HashMap<String, String>,
    ) -> Option<TerminalId> {
        let (cols, rows) = self.get_terminal_dimensions();

        // Per-window async bridge — terminal output flows back through
        // the window that owns the PTY.
        let bridge = self.bridge.clone();
        self.terminal_manager.set_async_bridge(bridge);

        let working_dir = cwd.unwrap_or_else(|| self.root.clone());
        let terminal_root = self.resources.dir_context.terminal_dir_for(&working_dir);
        if let Err(e) = terminal_backing_fs().create_dir_all(&terminal_root) {
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
        // that runs it *inside this session's backend* via the authority:
        // local runs it directly as the PTY child; a container authority
        // prepends `docker exec -it … <id>` so an agent terminal runs in the
        // container rather than on the host (see `Authority::terminal_command`).
        // Empty argv falls back to the interactive shell.
        let wrapper = match command_override {
            Some(argv) if !argv.is_empty() => self.authority().terminal_command(&argv),
            _ => self.resolved_terminal_wrapper(),
        };
        let wrapper = self.apply_remote_terminal_env(wrapper);
        let env_delta = self.terminal_env_delta(&wrapper);
        match self.terminal_manager.spawn(
            cols,
            rows,
            Some(working_dir),
            Some(log_path.clone()),
            Some(backing_path),
            wrapper,
            env_delta,
            extra_env,
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
                if let Err(e) = terminal_backing_fs().create_dir_all(&root) {
                    tracing::warn!("Failed to create terminal directory: {}", e);
                }
                root.join(format!("fresh-terminal-{}.txt", terminal_id.0))
            });

        // Ensure the file exists — but DON'T truncate if it already has
        // content. The PTY read loop may have already started writing
        // scrollback.
        if !terminal_backing_fs().exists(&backing_file) {
            if let Err(e) = terminal_backing_fs().write_file(&backing_file, &[]) {
                tracing::warn!("Failed to create terminal backing file: {}", e);
            }
        }

        self.terminal_backing_files
            .insert(terminal_id, backing_file.clone());

        let mut state = EditorState::new_with_path(
            large_file_threshold,
            terminal_backing_fs(),
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
        self.terminal_buffers
            .insert(buffer_id, TerminalBuffer::new_live(terminal_id));
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
        spec: PluginTerminalSpec,
    ) -> Result<(TerminalId, BufferId, Option<LeafId>), String> {
        let PluginTerminalSpec {
            cwd,
            direction,
            ratio,
            focus,
            persistent,
            command,
            title,
            env,
        } = spec;
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
            .spawn_terminal_session(cwd, persistent, command, env)
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
                            view_state.apply_config_defaults(
                                crate::view::split::ViewConfigDefaults {
                                    line_numbers: false,
                                    highlight_current_line: false,
                                    line_wrap: false,
                                    wrap_indent: false,
                                    wrap_column: None,
                                    rulers,
                                    scroll_offset: 0,
                                },
                            );
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
            // Mark this tab as explicitly titled so foreground-process
            // auto-naming leaves it alone (an OSC title still overrides).
            self.terminal_explicit_titles.insert(buffer_id);
        }

        // When the new terminal ended up as this window's active buffer, focus
        // it as a live terminal so the grid renders immediately. The buffer was
        // inserted with `TerminalBuffer::new_live` (empty scrollback set), so
        // every split showing it is already live; we only need to put the
        // editor pane into the Terminal key context. Without this the renderer
        // would defer to the file-backed scrollback view (see
        // `render_terminal_splits`) until the next printable keystroke.
        // Mirrors `open_terminal_in_window`'s post-spawn focus.
        if self.active_buffer() == buffer_id {
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

    /// Refresh terminal buffers' tab titles, tmux-style. Runs every frame,
    /// but the expensive part — reading each terminal's foreground process
    /// group (`tcgetpgrp` + `/proc`) — is throttled to [`FG_POLL_INTERVAL`]
    /// and cached; the cached name is re-applied to the tab on every frame
    /// so the title is responsive to renders without re-running the syscall.
    ///
    /// The tab label **combines** two sources (see [`combine_terminal_title`]):
    ///
    /// - **Foreground process name** — the command currently in the
    ///   terminal's foreground process group (e.g. `python3` while a REPL
    ///   runs, `bash` at the prompt). Mirrors tmux's
    ///   `#{pane_current_command}`; read on Linux, `None` elsewhere.
    /// - **OSC title** — what a program set via OSC 0/1/2 (e.g. a shell's
    ///   `user@host: ~/dir` prompt title, or vim's `file - VIM`).
    ///
    /// e.g. `python3 — root@host: ~/proj`. When only one is present that one
    /// is used; when neither is, the default `*Terminal N*` stands.
    ///
    /// Terminals with an explicit (plugin-/command-derived) title are left
    /// untouched — like a tmux manual rename, an intentional name opts out
    /// of auto-naming.
    ///
    /// Both parts are sanitized (control characters stripped, length capped)
    /// the same way as the host window title, and applied without the
    /// `name (k)` disambiguation used for plugin titles.
    pub fn sync_terminal_titles(&mut self) {
        // Gated by config: when off, tabs keep their static `*Terminal N*`
        // (or plugin) names. Clearing the cache lets a later enable start
        // fresh.
        if !self.config().editor.terminal_auto_title {
            self.terminal_fg_cache.clear();
            return;
        }

        // Refresh the foreground-name cache. A terminal is re-read when the
        // poll interval has elapsed, or eagerly while it has no cached name
        // yet (its first prompt may not have a foreground pgid the instant
        // it spawns, and renders are event-driven — so keep trying until it
        // resolves rather than waiting a full interval).
        let now = std::time::Instant::now();
        let interval_due = self
            .terminal_fg_poll_at
            .is_none_or(|last| now.duration_since(last) >= FG_POLL_INTERVAL);
        if interval_due {
            self.terminal_fg_poll_at = Some(now);
        }
        for (buffer_id, tb) in self.terminal_buffers.iter() {
            if self.terminal_explicit_titles.contains(buffer_id) {
                continue;
            }
            if !interval_due && self.terminal_fg_cache.contains_key(buffer_id) {
                continue;
            }
            let name = self
                .terminal_manager
                .get(tb.terminal_id)
                .and_then(|h| h.foreground_process_name())
                .map(|n| crate::services::terminal_title::sanitize_title(&n))
                .filter(|n| !n.is_empty());
            match name {
                Some(n) => {
                    self.terminal_fg_cache.insert(*buffer_id, n);
                }
                None => {
                    self.terminal_fg_cache.remove(buffer_id);
                }
            }
        }

        // Apply a title to every (non-explicit) terminal tab every frame,
        // combining the cached foreground name with the current OSC title.
        // Snapshot first so the mutable `buffer_metadata` borrow doesn't
        // overlap the immutable reads above.
        let mut updates: Vec<(BufferId, String)> = Vec::new();
        for (buffer_id, tb) in self.terminal_buffers.iter() {
            if self.terminal_explicit_titles.contains(buffer_id) {
                continue;
            }
            let pty = self.terminal_fg_cache.get(buffer_id).cloned();
            let osc = self
                .terminal_manager
                .get(tb.terminal_id)
                .and_then(|handle| {
                    let osc = handle.state.lock().ok()?.title().to_string();
                    let sanitized = crate::services::terminal_title::sanitize_title(&osc);
                    (!sanitized.is_empty()).then_some(sanitized)
                });
            let name = combine_terminal_title(pty.as_deref(), osc.as_deref())
                .unwrap_or_else(|| format!("*Terminal {}*", tb.terminal_id.0));
            updates.push((*buffer_id, name));
        }

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
        let terminal_id = self.spawn_terminal_session(None, true, None, HashMap::new())?;
        let split_id = self
            .buffers
            .splits()
            .map(|(mgr, _)| mgr.active_split())
            .expect("window must have a populated split layout");
        let buffer_id = self.create_terminal_buffer_attached(terminal_id, split_id);
        // Window-side activation: per-window mutation only — the
        // editor-wide plugin hook fires in the Editor wrapper.
        self.set_active_buffer(buffer_id);
        // Live by default (empty scrollback set); focus the terminal pane.
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
                if let Err(e) = terminal_backing_fs().create_dir_all(&root) {
                    tracing::warn!("Failed to create terminal directory: {}", e);
                }
                root.join(format!("fresh-terminal-{}.txt", terminal_id.0))
            });

        if !terminal_backing_fs().exists(&backing_file) {
            if let Err(e) = terminal_backing_fs().write_file(&backing_file, &[]) {
                tracing::warn!("Failed to create terminal backing file: {}", e);
            }
        }

        let mut state = EditorState::new_with_path(
            large_file_threshold,
            terminal_backing_fs(),
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
        self.terminal_buffers
            .insert(buffer_id, TerminalBuffer::new_live(terminal_id));
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        buffer_id
    }

    /// The terminal the user interacted with most recently: the latest
    /// split in the focus LRU whose current buffer is a terminal. Falls
    /// back to the newest open terminal when no split currently shows
    /// one (e.g. the terminal sits in a background tab), and `None`
    /// when the window has no terminals at all.
    pub fn last_focused_terminal(&self) -> Option<TerminalId> {
        if let Some((mgr, _)) = self.buffers.splits() {
            let terminal_of_leaf = |leaf: LeafId| {
                mgr.get_buffer_id(leaf.into())
                    .and_then(|buffer_id| self.terminal_buffers.get(&buffer_id))
                    .map(|tb| tb.terminal_id)
            };
            if let Some(leaf) = mgr.last_focused_where(|leaf| terminal_of_leaf(leaf).is_some()) {
                return terminal_of_leaf(leaf);
            }
        }
        self.terminal_buffers
            .values()
            .map(|tb| tb.terminal_id)
            .max_by_key(|t| t.0)
    }

    /// Respawn this window's dead embedded terminals through its *current*
    /// authority, reusing each terminal's backing/log files so scrollback
    /// continues across the gap.
    ///
    /// Called after a live remote reconnect re-points the window's authority
    /// (`Editor::set_session_authority`): the embedded `ssh -t` PTYs died with
    /// the carrier (a separate channel from the agent connection, which has its
    /// own auto-reconnect), so without this they'd sit dead until manually
    /// reopened. Each respawn re-runs the terminal's stored launch/resume argv
    /// through `Authority::terminal_command`, so the new PTY runs on the remote
    /// backend by construction — never the local host.
    ///
    /// Only terminals whose handle is missing or no longer alive are respawned;
    /// a still-live terminal is left untouched (respawning it would orphan its
    /// PTY). Terminal ids change on respawn — the manager allocates fresh ones —
    /// so every terminal-id-keyed entry (buffer→terminal binding, backing/log
    /// files, launch/resume commands, ephemeral marker) is remapped to the new
    /// id and the dead handle is torn down.
    ///
    /// Returns the number of terminals actually revived (dead handles that were
    /// respawned), so callers can tailor a status message and skip it when the
    /// window had no terminals to restore.
    pub fn respawn_terminals_through_authority(&mut self) -> usize {
        // Snapshot the (buffer, old terminal id) pairs up front — the loop
        // mutates `terminal_buffers` as it remaps ids.
        let bindings: Vec<(BufferId, TerminalId)> = self
            .terminal_buffers
            .iter()
            .map(|(b, tb)| (*b, tb.terminal_id))
            .collect();

        let mut revived = 0usize;
        for (buffer_id, old_id) in bindings {
            // Leave a still-live terminal alone; only revive the dead ones.
            let handle = self.terminal_manager.get(old_id);
            if handle.is_some_and(|h| h.is_alive()) {
                continue;
            }

            // Size + cwd carry over from the dead handle (so the reborn PTY
            // matches the split), falling back to the window's dimensions.
            let (cols, rows) = handle
                .map(|h| h.size())
                .unwrap_or_else(|| self.get_terminal_dimensions());
            let cwd = handle.and_then(|h| h.cwd());

            // Reuse the same backing/log files so the new PTY appends to the
            // existing scrollback rather than starting blank.
            let backing_path = self.terminal_backing_files.get(&old_id).cloned();
            let log_path = self.terminal_log_files.get(&old_id).cloned();

            // Same argv precedence as workspace restore: an agent-resume argv
            // first (rejoin the conversation), then the launch command, else
            // the plain interactive shell.
            let resume_argv = self
                .terminal_resume_commands
                .get(&old_id)
                .filter(|argv| !argv.is_empty() && self.resources.config.terminal.resume_agents)
                .cloned();
            let launch_argv = self
                .terminal_commands
                .get(&old_id)
                .filter(|argv| !argv.is_empty())
                .cloned();
            let spawn_argv = resume_argv.or(launch_argv);
            let wrapper = match spawn_argv.as_deref() {
                Some(argv) => self.authority().terminal_command(argv),
                None => self.resolved_terminal_wrapper(),
            };
            let wrapper = self.apply_remote_terminal_env(wrapper);
            let env_delta = self.terminal_env_delta(&wrapper);

            let new_id = match self.terminal_manager.spawn(
                cols,
                rows,
                cwd,
                log_path,
                backing_path,
                wrapper,
                env_delta,
                HashMap::new(),
            ) {
                Ok(id) => id,
                Err(e) => {
                    tracing::warn!("reconnect: failed to respawn terminal {:?}: {}", old_id, e);
                    continue;
                }
            };

            // The dead PTY's handle is now superseded — tear it down.
            self.terminal_manager.close(old_id);

            // Remap every terminal-id-keyed entry from old_id → new_id.
            if new_id != old_id {
                // Remap the PTY id but preserve the buffer's remembered mode.
                if let Some(tb) = self.terminal_buffers.get_mut(&buffer_id) {
                    tb.terminal_id = new_id;
                }
                if let Some(p) = self.terminal_backing_files.remove(&old_id) {
                    self.terminal_backing_files.insert(new_id, p);
                }
                if let Some(p) = self.terminal_log_files.remove(&old_id) {
                    self.terminal_log_files.insert(new_id, p);
                }
                if let Some(c) = self.terminal_commands.remove(&old_id) {
                    self.terminal_commands.insert(new_id, c);
                }
                if let Some(c) = self.terminal_resume_commands.remove(&old_id) {
                    self.terminal_resume_commands.insert(new_id, c);
                }
                if self.ephemeral_terminals.remove(&old_id) {
                    self.ephemeral_terminals.insert(new_id);
                }
            }

            // Register the reborn leader pid so window-level signal operations
            // (Stop / Archive / Delete) reach the new process group.
            if let Some(pid) = self.terminal_manager.get(new_id).and_then(|h| h.pid()) {
                self.process_groups
                    .register(pid, format!("terminal #{}", new_id.0));
            }

            revived += 1;
        }

        // Size the freshly-spawned PTYs to their splits' content areas.
        self.resize_visible_terminals();

        revived
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
            .spawn_terminal_session(None, true, None, HashMap::new())
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

    /// Open a new terminal in a fresh split created from the active pane.
    ///
    /// `SplitDirection::Vertical` places the terminal in a pane to the
    /// right; `SplitDirection::Horizontal` places it below. Unlike
    /// `open_terminal` (which attaches a terminal tab to the *current*
    /// split), this seeds a brand-new split leaf with the terminal buffer
    /// directly — mirroring `Action::OpenTerminalInDock` — so the new pane
    /// shows only the terminal, with no phantom tab carrying the
    /// previously-active buffer.
    pub fn open_terminal_split(&mut self, direction: crate::model::event::SplitDirection) {
        // Splitting the layout is a commitment gesture for any preview tab.
        // Promote before touching the split tree so the "preview is anchored
        // to a single split" invariant holds across the operation (mirrors
        // `split_pane_impl`).
        self.active_window_mut().promote_current_preview();

        // Spawn the PTY first so we have a real terminal buffer to seed the
        // new leaf with — otherwise the leaf would carry the user's
        // previously-active buffer as a placeholder that would linger as a
        // phantom tab.
        let Some(terminal_id) = self.spawn_terminal_session() else {
            return;
        };
        let buffer_id = self.create_terminal_buffer_detached(terminal_id);

        // Split the active pane, placing the new terminal leaf after
        // (right for Vertical, below for Horizontal).
        let new_leaf = self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .split_active(direction, buffer_id, 0.5);
        let new_leaf = match new_leaf {
            Ok(leaf) => leaf,
            Err(e) => {
                self.set_status_message(t!("split.error", error = e.to_string()).to_string());
                return;
            }
        };

        let mut view_state =
            SplitViewState::with_buffer(self.terminal_width, self.terminal_height, buffer_id);
        // Terminal-dedicated splits never show line numbers or current-line
        // highlight (mirrors the dock + plugin-terminal split setup).
        view_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
            line_numbers: false,
            highlight_current_line: false,
            line_wrap: self.active_window().resolve_line_wrap_for_buffer(buffer_id),
            wrap_indent: self.config.editor.wrap_indent,
            wrap_column: self
                .active_window()
                .resolve_wrap_column_for_buffer(buffer_id),
            rulers: self.config.editor.rulers.clone(),
            scroll_offset: 0,
        });
        // Terminals don't wrap — keep escape sequences intact.
        view_state.viewport.line_wrap_enabled = false;

        self.windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .insert(new_leaf, view_state);
        self.windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .set_active_split(new_leaf);

        // Mirror open_terminal's post-attach bookkeeping. The new terminal was
        // inserted with `TerminalBuffer::new_live` (empty scrollback set), so it
        // is live in this new split; other splits keep their own per-split
        // mode, so closing this split later restores them correctly (#2485).
        self.active_window_mut().key_context = crate::input::keybindings::KeyContext::Terminal;
        self.active_window_mut().resize_visible_terminals();

        // A new split changes every sibling pane's size. Reflow through the
        // single layout funnel so existing terminals fit their new panes.
        self.relayout();

        // Editor-wide: refresh the plugin-state snapshot so plugin hooks see
        // the new active buffer, then fire `buffer_activated`.
        #[cfg(feature = "plugins")]
        self.update_plugin_state_snapshot();
        #[cfg(feature = "plugins")]
        self.plugin_manager.read().unwrap().run_hook(
            "buffer_activated",
            crate::services::plugins::hooks::HookArgs::BufferActivated { buffer_id },
        );

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
            "Opened terminal {:?} into new split leaf {:?} (buffer {:?})",
            terminal_id,
            new_leaf,
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

        if let Some(terminal_id) = self.active_window().get_terminal_id(buffer_id) {
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
                let _ = terminal_backing_fs().remove_file(path);
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
                    let _ = terminal_backing_fs().remove_file(&log_file);
                }
            }

            // Leave the terminal key context; closing the buffer re-syncs the
            // context from whatever becomes active next.
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

    /// Send the current selection (or the cursor's line when nothing is
    /// selected) to the most recently focused terminal, terminated with
    /// a newline so shells/REPLs execute it — the "Run Selected Text In
    /// Active Terminal" workflow from VS Code (issue #1871). The
    /// terminal is then focused (jumping to its split or bringing its
    /// tab forward) in terminal mode, so the user lands at the prompt.
    pub fn send_selection_to_terminal(&mut self) {
        // Only meaningful from an editor buffer; a terminal buffer has
        // no text selection to send.
        if self
            .active_window()
            .is_terminal_buffer(self.active_buffer())
        {
            return;
        }

        let Some(terminal_id) = self.active_window().last_focused_terminal() else {
            self.set_status_message(t!("terminal.no_terminal_open").to_string());
            return;
        };

        let text = self.selection_or_cursor_line_text();

        // Same normalization as the terminal paste path (CRLF/CR →
        // LF), plus a terminating newline so the last line runs.
        let mut normalized = text.replace("\r\n", "\n").replace('\r', "\n");
        if !normalized.ends_with('\n') {
            normalized.push('\n');
        }

        if let Some(handle) = self.active_window().terminal_manager.get(terminal_id) {
            handle.write(normalized.as_bytes());
            self.focus_terminal_buffer(terminal_id);
            // After `enter_terminal_mode`'s generic message — the send
            // destination is the more useful thing to surface.
            self.set_status_message(t!("terminal.sent_selection", id = terminal_id.0).to_string());
        }
    }

    /// Focus the buffer of the given terminal: jump to the split that
    /// shows it, or — when it sits in a background tab — focus its host
    /// split and bring the tab forward; then enable terminal mode so
    /// keystrokes go to the prompt.
    fn focus_terminal_buffer(&mut self, terminal_id: TerminalId) {
        let Some(buffer_id) = self
            .active_window()
            .terminal_buffers
            .iter()
            .find_map(|(buffer, tb)| (tb.terminal_id == terminal_id).then_some(*buffer))
        else {
            return;
        };

        // Prefer a split currently showing the terminal; otherwise the
        // split holding it as a background tab. `focus_split` handles
        // both (it delegates to the tab-switch path when the target is
        // the active split).
        let target_split = self.active_window().buffers.splits().and_then(|(mgr, vs)| {
            mgr.splits_for_buffer(buffer_id)
                .into_iter()
                .next()
                .or_else(|| {
                    vs.iter()
                        .find(|(_, view_state)| view_state.has_buffer(buffer_id))
                        .map(|(split_id, _)| *split_id)
                })
        });
        if let Some(split_id) = target_split {
            self.focus_split(split_id, buffer_id);
        } else {
            self.switch_buffer(buffer_id);
        }

        // `focus_split` enables terminal mode for the cross-split case,
        // but a tab switch resumes it only when the terminal was left in
        // terminal mode. Enter it explicitly — this also re-enables
        // editing and scrolls a previously-synced scrollback view back
        // to the live prompt.
        self.enter_terminal_mode();
    }

    /// Text that "send to terminal" operates on, mirroring
    /// `copy_selection`'s precedence: block selection first, then
    /// regular selections (joined by newline), else each cursor's
    /// current line (without its line ending).
    fn selection_or_cursor_line_text(&mut self) -> String {
        if self
            .active_cursors()
            .iter()
            .any(|(_, cursor)| cursor.has_block_selection())
        {
            return self.copy_block_selection_text();
        }

        let ranges: Vec<_> = self
            .active_cursors()
            .iter()
            .filter_map(|(_, cursor)| cursor.selection_range())
            .collect();
        if !ranges.is_empty() {
            let state = self.active_state_mut();
            let mut text = String::new();
            for range in ranges {
                if !text.is_empty() {
                    text.push('\n');
                }
                text.push_str(&state.get_text_range(range.start, range.end));
            }
            return text;
        }

        let estimated_line_length = 80;
        let positions: Vec<_> = self
            .active_cursors()
            .iter()
            .map(|(_, cursor)| cursor.position)
            .collect();
        let state = self.active_state_mut();
        let mut text = String::new();
        for pos in positions {
            let mut iter = state.buffer.line_iterator(pos, estimated_line_length);
            if let Some((_start, content)) = iter.next_line() {
                if !text.is_empty() {
                    text.push('\n');
                }
                text.push_str(content.trim_end_matches(['\n', '\r']));
            }
        }
        text
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
                    // The user dropped the focused split into read-only
                    // scrollback (recorded per-split so re-focusing keeps it).
                    self.enter_terminal_scrollback();
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
            // Resuming into live mode: clear the focused split's scrollback
            // edge (so this split streams the live grid again) and focus the
            // terminal pane. Other splits keep their own per-split mode.
            let __active = self.active_buffer();
            let __leaf = self.active_window().effective_active_split();
            self.active_window_mut()
                .set_split_terminal_scrollback(__leaf, __active, false);
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
                // A selection made in the scrollback view must not outlive
                // the visit: the anchor would otherwise re-materialize as a
                // phantom selection on the next scrollback entry (the sync
                // pins only the cursor *position*) and re-suppress the
                // output-driven auto-resume in `handle_terminal_output`.
                view_state.cursors.map(|c| c.clear_selection());
            }

            // Truncate backing file to remove visible screen tail and scroll to bottom
            if let Some(terminal_id) = self.active_window().get_terminal_id(self.active_buffer()) {
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
                            if let Err(e) =
                                terminal_backing_fs().set_file_length(backing_path, truncate_pos)
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

    /// Drop the focused split into read-only scrollback for its terminal —
    /// the inverse of [`Editor::enter_terminal_mode`]. Records the edge
    /// per-split (so other splits on the same terminal keep their own state)
    /// and lets the single derivation project the key context and refresh the
    /// read-only buffer view. No-op if the active buffer isn't a terminal.
    pub fn enter_terminal_scrollback(&mut self) {
        if self
            .active_window()
            .is_terminal_buffer(self.active_buffer())
        {
            let buf = self.active_buffer();
            let leaf = self.active_window().effective_active_split();
            self.active_window_mut()
                .set_split_terminal_scrollback(leaf, buf, true);
            self.active_window_mut().sync_terminal_mode_flags();
        }
    }

    /// Get terminal content for rendering
    pub fn get_terminal_content(
        &self,
        buffer_id: BufferId,
    ) -> Option<Vec<Vec<crate::services::terminal::TerminalCell>>> {
        let terminal_id = self.active_window().get_terminal_id(buffer_id)?;
        let handle = self.active_window().terminal_manager.get(terminal_id)?;
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
        let terminal_id = self.get_terminal_id(self.active_buffer())?;
        let handle = self.terminal_manager.get(terminal_id)?;
        handle.state.lock().ok()
    }

    /// Send input bytes to this window's active terminal (no-op if the
    /// active buffer is not a terminal).
    pub fn send_terminal_input(&mut self, data: &[u8]) {
        if let Some(terminal_id) = self.get_terminal_id(self.active_buffer()) {
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

        // Alternate-scroll mode converts the wheel into arrow keys so the
        // wheel scrolls pagers like `less`/`man` that don't track the mouse.
        // It must be suppressed whenever the program is itself tracking the
        // mouse: such a program (e.g. Claude Code in its full-screen
        // "no-flicker" mode) requested mouse reporting precisely so it can
        // scroll its own viewport from wheel events. Forwarding synthesized
        // Up/Down arrows instead leaks them into the program's input — for
        // Claude Code that cycles prompt/message history rather than
        // scrolling. This mirrors xterm/alacritty, where alternate scroll is
        // inactive while any mouse-tracking mode is on.
        //
        // Note `ALTERNATE_SCROLL` is on by default in alacritty_terminal, so
        // this branch would otherwise fire for every wheel event forwarded to
        // an alternate-screen program — the `wants_mouse` guard is what keeps
        // mouse-aware programs receiving real wheel reports.
        let wants_mouse = self
            .get_active_terminal_state()
            .map(|s| s.wants_mouse_events())
            .unwrap_or(false);
        let uses_alt_scroll = !wants_mouse
            && self
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
        if let Some(terminal_id) = self.get_terminal_id(buffer_id) {
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
        if let Some(terminal_id) = self.get_terminal_id(buffer_id) {
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

        for (split_id, buffer_id, split_area) in visible_buffers {
            if self.terminal_buffers.contains_key(&buffer_id) {
                // A split hides its scrollbar (grid reclaims the column)
                // whenever it shows the live PTY grid: every terminal split
                // except one that is in read-only scrollback. Mirror the
                // renderer's `terminal_showing_live_grid` gate so the PTY width
                // matches the rendered `content_rect`.
                let showing_live_grid = !self.split_terminal_scrollback(split_id, buffer_id);
                let scrollbar_cols = if showing_live_grid { 0 } else { 1 };
                // Tab bar takes 1 row; reserve 1 row for chrome and the
                // scrollbar column (when shown) on the right.
                let content_height = split_area.height.saturating_sub(2);
                let content_width = split_area.width.saturating_sub(1 + scrollbar_cols);

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
        let Some(terminal_id) = self.get_terminal_id(buffer_id) else {
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
                use std::io::BufWriter;

                // Flush any scrollback that has scrolled off but isn't in the
                // file yet — in particular the lines a resize spilled from the
                // screen into history. The PTY read loop also flushes on output,
                // but an idle terminal that was only resized has pending lines;
                // capturing them here guarantees the scroll-back view is complete.
                if let Ok(mut file) = terminal_backing_fs().open_file_for_append(&backing_file) {
                    let mut writer = BufWriter::new(&mut *file);
                    if let Err(e) = state.flush_new_scrollback(&mut writer) {
                        tracing::error!("Failed to flush terminal scrollback: {}", e);
                    }
                }

                // Record the current file size as the history end point
                // (before appending visible screen) so we can truncate back to it
                if let Ok(metadata) = terminal_backing_fs().metadata(&backing_file) {
                    state.set_backing_file_history_end(metadata.size);
                    history_end_byte = Some(metadata.size);
                }

                // Open backing file in append mode to add visible screen
                if let Ok(mut file) = terminal_backing_fs().open_file_for_append(&backing_file) {
                    let mut writer = BufWriter::new(&mut *file);
                    if let Err(e) = state.append_visible_screen(&mut writer) {
                        tracing::error!("Failed to append visible screen to backing file: {}", e);
                    }
                }
            }
        }

        // Reload buffer from the backing file (reusing existing file loading).
        // Force text mode: raw PTY scrollback can contain control bytes that
        // would otherwise trip binary detection, dropping ANSI colors and
        // showing escape-code fragments in scrollback mode (#2449).
        let large_file_threshold = self.resources.config.editor.large_file_threshold_bytes as usize;
        if let Ok(new_state) = EditorState::from_file_with_languages_force_text(
            &backing_file,
            self.terminal_width,
            self.terminal_height,
            large_file_threshold,
            &self.resources.grammar_registry,
            &self.resources.config.languages,
            terminal_backing_fs(),
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
                    // Scrollback is stored as unwrapped logical lines, so soft-wrap
                    // the read-only view to reflow long lines to the current width.
                    // (Visible-screen rows are ≤ the view width and so never wrap,
                    // keeping the exit frame aligned with the live grid.)
                    buf_state.viewport.line_wrap_enabled = true;
                }
            }
            if let Some(view_state) = view_states.get_mut(&active_split) {
                view_state.viewport.line_wrap_enabled = true;
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
        let focused_split = self.effective_active_split();
        for (split_id, buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            split_areas
        {
            let Some(terminal_id) = self.get_terminal_id(*buffer_id) else {
                continue;
            };
            // The live PTY grid overlays every split showing a terminal EXCEPT
            // one that is in read-only scrollback — there we defer to normal
            // text rendering so the user can scroll. Keying this on the split
            // (not the buffer, not the single window flag) is what lets the
            // same terminal be scrolled back in one split while another keeps
            // streaming the live grid, independently, even off-focus
            // (fresh#2595).
            if self.split_terminal_scrollback(*split_id, *buffer_id) {
                continue;
            }
            let Some(handle) = self.terminal_manager.get(terminal_id) else {
                continue;
            };
            let Ok(state) = handle.state.lock() else {
                continue;
            };
            let cursor_pos = state.cursor_position();
            // The block cursor belongs to the focused split's live terminal
            // only — other live splits mirror the same PTY but aren't the
            // input target.
            let cursor_visible = state.cursor_visible()
                && *split_id == focused_split
                && self.focused_terminal_live()
                && cursor_visible_if_active;
            let (_, rows) = state.size();
            let mut content = Vec::with_capacity(rows as usize);
            for row in 0..rows {
                content.push(state.get_line(row));
            }
            // Ctrl+hover underline: highlight the link span when it's in this
            // terminal buffer.
            let link_highlight = self
                .terminal_link_hover
                .as_ref()
                .and_then(|h| (h.buffer_id == *buffer_id).then(|| (h.row, h.cols.clone())));
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
                link_highlight,
            );
        }
    }
}

impl Editor {
    /// Whether the focused split is a live terminal (input goes to a PTY).
    /// Derived from the per-split scrollback source of truth; primarily used by
    /// tests and status rendering.
    pub fn is_terminal_mode(&self) -> bool {
        self.active_window().focused_terminal_live()
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
    #[allow(clippy::too_many_arguments)]
    pub fn render_terminal_content(
        content: &[Vec<TerminalCell>],
        cursor_pos: (u16, u16),
        cursor_visible: bool,
        area: Rect,
        buf: &mut Buffer,
        default_fg: Color,
        default_bg: Color,
        link_highlight: Option<(u16, std::ops::Range<usize>)>,
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

                // Ctrl+hover link highlight: underline the link span so it
                // reads as clickable.
                if let Some((link_row, ref cols)) = link_highlight {
                    if row_idx as u16 == link_row && cols.contains(&col_idx) {
                        style = style.add_modifier(Modifier::UNDERLINED);
                    }
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
                None,
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

        /// The Ctrl+hover link highlight underlines exactly the cells in the
        /// given (row, col-range) span and leaves the rest untouched.
        #[test]
        fn link_highlight_underlines_only_its_span() {
            // One 6-wide row of text "abcdef".
            let area = Rect::new(0, 0, 6, 1);
            let mut buf = Buffer::empty(area);
            let row: Vec<TerminalCell> = "abcdef"
                .chars()
                .map(|c| TerminalCell {
                    c,
                    ..Default::default()
                })
                .collect();
            let content = vec![row];

            render_terminal_content(
                &content,
                (0, 0),
                false,
                area,
                &mut buf,
                Color::White,
                Color::Black,
                Some((0, 2..5)), // underline columns 2,3,4
            );

            for x in 0..area.width {
                let underlined = buf[(x, 0)].modifier.contains(Modifier::UNDERLINED);
                let expected = (2..5).contains(&(x as usize));
                assert_eq!(
                    underlined, expected,
                    "cell col {x} underline = {underlined}, expected {expected}",
                );
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

#[cfg(test)]
mod title_tests {
    use super::combine_terminal_title;

    #[test]
    fn combines_command_and_osc_title() {
        assert_eq!(
            combine_terminal_title(Some("python3"), Some("root@host: ~/proj")).as_deref(),
            Some("python3 \u{2014} root@host: ~/proj")
        );
    }

    #[test]
    fn uses_single_source_when_only_one_present() {
        assert_eq!(
            combine_terminal_title(Some("bash"), None).as_deref(),
            Some("bash")
        );
        assert_eq!(
            combine_terminal_title(None, Some("root@host: ~/proj")).as_deref(),
            Some("root@host: ~/proj")
        );
    }

    #[test]
    fn does_not_duplicate_command_already_in_osc_title() {
        // vim sets its own OSC title; don't prepend "vim — … VIM".
        assert_eq!(
            combine_terminal_title(Some("vim"), Some("README.md (~/proj) - VIM")).as_deref(),
            Some("README.md (~/proj) - VIM")
        );
    }

    #[test]
    fn none_when_neither_present() {
        assert_eq!(combine_terminal_title(None, None), None);
    }
}
