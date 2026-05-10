//! Editor `Window` — a project-rooted unit of editor state.
//!
//! A `Window` bundles the state that is logically scoped to one
//! project root: the file tree, ignore matcher, LSP client set,
//! file watchers, split layout, and buffer membership. Switching the
//! active window re-targets the entire editor UI (file explorer,
//! quick-open, LSP roots) without recreating buffers, terminals, or
//! plugin state — those live on the `Editor` and survive switches.
//!
//! See `docs/internal/conductor-sessions-design.md` for the full
//! design rationale.
//!
//! ## Naming
//!
//! Internally we call these "windows" (modelled on VS Code windows)
//! to disambiguate from Fresh's pre-existing workspace-recovery and
//! config-layer "session" concepts. Conductor presents windows as
//! "agent sessions" in its UX, since the parallel-agents domain
//! language is what users see — but the editor types are `Window`,
//! `WindowId`, etc.
//!
//! ## Migration status
//!
//! Steps 0a–0f, 0j, 0k phases 1–3, and 0l shipped. Per-subsystem
//! state that used to warm-swap on `setActiveWindow` —
//! `panel_ids`, `file_mod_times`, `file_explorer`, `lsp`, the
//! `splits` pair, `buffers`, `buffer_metadata`, the terminal
//! subsystem (`terminal_manager` + `terminal_buffers` +
//! `terminal_backing_files` + `terminal_log_files`),
//! `event_logs`, `position_history` (with its `in_navigation` /
//! `suppress_position_history_once` companion flags),
//! `bookmarks`, `grouped_subtrees`, `composite_buffers`,
//! `composite_view_states`, all 23 LSP-request-tracking maps
//! (pending-/in-flight/applied, debounce timers,
//! `next_lsp_request_id`, `completion_items`, `dabbrev_state`,
//! code-action attribution), the per-window async `bridge`, and
//! the chrome surfaces (`status_message`, `plugin_status_message`,
//! `prompt`) — all live directly on `Window`. `set_active_window`
//! is a pointer write (plus first-dive seed allocation for
//! windows that have never been activated).

use crate::app::types::WindowLayoutCache;
use crate::app::window_resources::WindowResources;
use crate::model::event::LeafId;
use crate::services::lsp::manager::LspManager;
use crate::view::file_tree::FileTreeView;
use crate::view::split::{SplitManager, SplitViewState};
use fresh_core::{BufferId, WindowId};
use std::collections::HashMap;
use std::path::PathBuf;

/// A project-rooted unit of editor state.
///
/// After Step 0b every per-subsystem field listed below is owned
/// outright by the window — there are no warm-swap stashes.
/// `setActiveWindow` is a pointer write; reads of the active
/// window's state route through Editor accessors
/// (`active_layout()`, `split_manager()`, `file_explorer()`, `lsp()`,
/// `panel_ids()`, `file_mod_times()`, …). Cross-window access goes
/// through `Editor.windows.get(&id)` directly.
pub struct Window {
    /// Stable identifier. The base window is always `WindowId(1)`.
    pub id: WindowId,

    /// User-visible label. Defaults to the basename of `root` (or
    /// "main" when the root is the original process cwd). Not
    /// required to be unique.
    pub label: String,

    /// Canonical absolute path of the project root. Read-only after
    /// construction; closing a window and creating a new one is the
    /// way to "rename" the root.
    pub root: PathBuf,

    /// File-explorer view (expansion, scroll, selection). `None`
    /// means "never opened" — the caller rebuilds at `root` on first
    /// toggle. Each window has its own view; switching windows shows
    /// the new window's tree (or none, if it hasn't been opened yet).
    pub file_explorer: Option<FileTreeView>,

    /// Split-tree layout (split tree + per-leaf view state — scroll,
    /// cursor positions, focused buffer in each leaf). `None` means
    /// "this window has never been activated and so has no layout
    /// yet"; the dive code creates a fresh layout rooted at a new
    /// empty unnamed buffer for that window. The base window has
    /// this populated at editor init.
    pub splits: Option<(SplitManager, HashMap<LeafId, SplitViewState>)>,

    /// Polling-based mtime cache for auto-revert. Auto-revert only
    /// fires for the active window's files; inactive windows' mtimes
    /// stay frozen at dive-out time and resync on dive-back —
    /// matching the user's mental model that a dormant window "is
    /// paused".
    pub file_mod_times: HashMap<PathBuf, std::time::SystemTime>,

    /// LSP manager (running language servers, configs, per-language
    /// root URIs). Each window owns its own LSP set, rooted at its
    /// project root; inactive windows' servers remain running in the
    /// background — that's the warm-LSP property the design's
    /// trade-off discussion calls out as a memory cost worth paying
    /// so dive-back is instant.
    ///
    /// `None` means "this window has never spawned any LSP"; the
    /// next LSP feature trigger will lazily create one.
    pub lsp: Option<LspManager>,

    /// Utility-dock panel-id → buffer-id occupancy. Each window
    /// gets its own dock — when one window has the search panel
    /// claimed and the user dives elsewhere, the new window starts
    /// with an empty dock and rebuilds on demand.
    pub panel_ids: HashMap<String, BufferId>,

    /// Buffers attached to this window. Each window owns the
    /// `EditorState` for its buffers outright; closing the window
    /// drops them. Opening the same file in two windows produces
    /// two independent buffers.
    pub buffers: HashMap<BufferId, crate::state::EditorState>,

    /// Per-buffer metadata (display name, file path / LSP URI,
    /// virtual-buffer mode, read-only flag, LSP-opened set, preview
    /// flag, etc.) for the buffers in `Window.buffers`. Lives next
    /// to the buffer storage it describes; closing a window drops
    /// every metadata entry along with the buffers themselves.
    pub buffer_metadata: HashMap<BufferId, crate::app::types::BufferMetadata>,

    /// Per-buffer undo/redo event log. Lives next to `buffers`
    /// because undo history is buffer-scoped — closing a window
    /// drops the buffer and its log together.
    pub event_logs: HashMap<BufferId, crate::model::event::EventLog>,

    /// Status message (shown in this window's status bar). Per-window
    /// because each window has its own context — a save in window A
    /// shouldn't flash a status message into window B's UI. Only the
    /// active window's chrome renders, so background-window status
    /// messages are naturally invisible.
    pub status_message: Option<String>,

    /// Plugin-provided status message (displayed alongside the core
    /// status, also per-window).
    pub plugin_status_message: Option<String>,

    /// Active prompt (minibuffer) for this window. Each window can
    /// have its own prompt mid-flight; switching windows preserves
    /// each window's prompt state independently.
    pub prompt: Option<crate::view::prompt::Prompt>,

    /// Per-window async bridge — the (Sender, Receiver) pair the
    /// LSP manager (and per-window terminal/file-explorer tasks
    /// once they migrate) uses to deliver async responses back to
    /// the main loop. Each window owns its own channel so cleanup
    /// on `closeWindow` is automatic (the receiver drops, senders
    /// error and stop). Editor-global async messages (plugin
    /// runtime callbacks, file-open dialog) flow through
    /// `Editor.async_bridge` instead.
    pub bridge: crate::services::async_bridge::AsyncBridge,

    // ---- LSP request-tracking state (moved from Editor in Step 0k) ----
    /// Per-window LSP request-id allocator. Each window's LspManager
    /// talks to its own server connections, and each connection only
    /// requires per-connection request-id uniqueness — no global
    /// namespace needed. Starts at 0 per window.
    pub next_lsp_request_id: u64,

    /// Pending LSP completion request ids (multi-server).
    pub pending_completion_requests: std::collections::HashSet<u64>,

    /// Original LSP completion items (for type-to-filter).
    pub completion_items: Option<Vec<lsp_types::CompletionItem>>,

    /// Scheduled completion-trigger time (debounced quick-suggestions).
    pub scheduled_completion_trigger: Option<std::time::Instant>,

    /// Dabbrev cycling state (Alt+/ session).
    pub dabbrev_state: Option<crate::app::DabbrevCycleState>,

    /// Pending LSP go-to-definition request id.
    pub pending_goto_definition_request: Option<u64>,

    /// Pending LSP find-references request id and the symbol name.
    pub pending_references_request: Option<u64>,
    pub pending_references_symbol: String,

    /// Pending LSP signature-help request id.
    pub pending_signature_help_request: Option<u64>,

    /// Pending LSP code-actions request ids and per-request server-name
    /// attribution + the selected-from list.
    pub pending_code_actions_requests: std::collections::HashSet<u64>,
    pub pending_code_actions_server_names: std::collections::HashMap<u64, String>,
    pub pending_code_actions: Option<Vec<(String, lsp_types::CodeActionOrCommand)>>,

    /// Pending inlay-hints requests keyed by request id.
    pub pending_inlay_hints_requests: std::collections::HashMap<u64, crate::app::InlayHintsRequest>,

    /// Pending folding-range requests + per-buffer in-flight tracking + debounce.
    pub pending_folding_range_requests:
        std::collections::HashMap<u64, crate::app::FoldingRangeRequest>,
    pub folding_ranges_in_flight: std::collections::HashMap<BufferId, (u64, u64)>,
    pub folding_ranges_debounce: std::collections::HashMap<BufferId, std::time::Instant>,

    /// Pending semantic-tokens-full requests + per-buffer in-flight tracking +
    /// the next-allowed-refresh debounce.
    pub pending_semantic_token_requests:
        std::collections::HashMap<u64, crate::app::SemanticTokenFullRequest>,
    pub semantic_tokens_in_flight:
        std::collections::HashMap<BufferId, (u64, u64, crate::app::SemanticTokensFullRequestKind)>,
    pub semantic_tokens_full_debounce: std::collections::HashMap<BufferId, std::time::Instant>,

    /// Pending semantic-tokens-range requests + per-buffer in-flight,
    /// last-request, and last-applied tracking.
    pub pending_semantic_token_range_requests:
        std::collections::HashMap<u64, crate::app::SemanticTokenRangeRequest>,
    pub semantic_tokens_range_in_flight:
        std::collections::HashMap<BufferId, (u64, usize, usize, u64)>,
    pub semantic_tokens_range_last_request:
        std::collections::HashMap<BufferId, (usize, usize, u64, std::time::Instant)>,
    pub semantic_tokens_range_applied: std::collections::HashMap<BufferId, (usize, usize, u64)>,

    /// Back/forward navigation stack (cursor jumps, file switches)
    /// scoped to this window. Each window has its own history so
    /// switching windows doesn't pollute the other window's
    /// back-stack — diving back into a window resumes navigation
    /// where you left it.
    pub position_history: crate::input::position_history::PositionHistory,

    /// `true` while a back/forward jump is in progress. Suppresses
    /// `track_cursor_movement` from recording the jump itself as a
    /// new entry. Per-window so windows don't fight over the flag
    /// during cross-window orchestration.
    pub in_navigation: bool,

    /// One-shot suppression of position-history recording for the
    /// next buffer-switch (used by file-open paths that don't want
    /// to leave a trail entry for the about-to-be-loaded file).
    pub suppress_position_history_once: bool,

    /// Bookmarks (single-char register → buffer + byte position) for
    /// this window. Bookmarks point at this window's buffers and
    /// follow the window across `setActiveWindow` switches — every
    /// window has its own register set.
    pub(crate) bookmarks: crate::app::bookmarks::BookmarkState,

    /// Composite buffers in this window (separate from regular
    /// buffers). These display multiple source buffers in a single
    /// tab — Live Grep results, References, Diagnostics list,
    /// etc. Owned per-window so the panel state follows the window
    /// that opened it.
    pub composite_buffers: HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,

    /// Per-split view state for composite buffers in this window.
    /// Keyed by (split_id, buffer_id) — each split that hosts a
    /// composite buffer gets its own scroll-row tracking.
    pub composite_view_states:
        HashMap<(LeafId, BufferId), crate::view::composite_view::CompositeViewState>,

    /// Grouped `SplitNode` subtrees for this window, keyed by their
    /// `LeafId` (which is what `TabTarget::Group(leaf_id)`
    /// references). Each entry is a `SplitNode::Grouped` node
    /// holding the layout for one buffer group (Live Grep, References,
    /// Diagnostics, etc.). These subtrees are NOT part of the main
    /// split tree — they live here and are dispatched to at render
    /// time when the current split's active target is a `Group`.
    /// Per-window because a buffer-group panel belongs to the window
    /// that opened it.
    pub grouped_subtrees: HashMap<LeafId, crate::view::split::SplitNode>,

    /// Terminal subsystem (PTY processes + render-state grids) for
    /// this window. Owned per-window so closing a window joins its
    /// PTY threads — no orphan agents survive a `closeWindow`.
    pub terminal_manager: crate::services::terminal::TerminalManager,

    /// Maps a terminal-buffer id to its PTY id, scoped to this window.
    pub terminal_buffers: HashMap<BufferId, crate::services::terminal::TerminalId>,

    /// Backing files for terminal buffers (the rendered visible-screen
    /// + scrollback content the buffer actually displays).
    pub terminal_backing_files: HashMap<crate::services::terminal::TerminalId, std::path::PathBuf>,

    /// Raw log files for terminal buffers (the unfiltered byte stream
    /// from the PTY, used for replay / save-history).
    pub terminal_log_files: HashMap<crate::services::terminal::TerminalId, std::path::PathBuf>,

    /// Plugin-managed per-window state. Outer key is plugin name,
    /// inner is the plugin-defined key. Read via
    /// `editor.getWindowState(key)` and written via
    /// `editor.setWindowState(key, value)`. Persisted to
    /// `.fresh/windows.json` so it survives editor restarts.
    pub plugin_state: HashMap<String, HashMap<String, serde_json::Value>>,

    /// Window-scoped layout hit-test cache: split-leaf rects, tab
    /// rects, the file-explorer rect, separators, scrollbars, and
    /// per-leaf `view_line_mappings` that mouse positioning and
    /// visual-line motion read. Repopulated by the renderer on every
    /// frame; stale until the next render after a window switch (the
    /// post-switch render fills it in before any input handling).
    /// Editor-chrome rects (status bar, menu, popups, prompt overlay)
    /// live on `Editor::chrome_layout` instead.
    pub(crate) layout_cache: WindowLayoutCache,

    /// Editor-global resources shared by `Arc` clone (config, theme
    /// registry, keybindings, command registry, filesystem authority,
    /// the buffer-id allocator, …). See [`WindowResources`] for the
    /// full inventory and rationale.
    pub(crate) resources: WindowResources,

    /// Buffer currently opened in "preview" (ephemeral) mode, together
    /// with the split (pane) it lives in. At most one preview exists
    /// per window. Pre Step-0 this lived on `Editor`; moved here so
    /// preview tracking follows the window's other view-state.
    ///
    /// Invariants:
    /// - The `is_preview` flag on the referenced buffer's metadata is
    ///   true iff this tuple is `Some` and points at that buffer.
    /// - The preview is anchored to the split it was opened in.
    /// - Cleared when the buffer is closed or promoted.
    pub preview: Option<(LeafId, BufferId)>,

    /// Whether terminal mode is active in this window (input goes to
    /// the active terminal buffer). Per-window because each window
    /// has its own terminal set + active buffer.
    pub terminal_mode: bool,

    /// Set of terminal buffer ids that should auto-resume terminal
    /// mode when switched back to. Per-window because terminal
    /// buffers are per-window (Step 0d).
    pub terminal_mode_resume: std::collections::HashSet<BufferId>,

    /// Track which byte ranges have been seen per buffer (for the
    /// `lines_changed` plugin-hook optimisation). Keyed by `BufferId`,
    /// follows the buffers onto Window.
    pub seen_byte_ranges: HashMap<BufferId, std::collections::HashSet<(usize, usize)>>,

    /// Previous viewport states for `viewport_changed` hook detection.
    /// Stores `(top_byte, width, height)` from the end of the last
    /// render frame. Keyed by `LeafId`, per-window because the splits
    /// it tracks are per-window.
    pub previous_viewports: HashMap<LeafId, (usize, u16, u16)>,

    /// Whether scroll syncing applies to splits showing the same
    /// buffer. Per-window UX toggle.
    pub same_buffer_scroll_sync: bool,

    /// Per-window interactive search-and-replace session state.
    /// Drives the F+y/n/!/q UX during `replace_in_buffer` /
    /// `replace_all`. Per-window because the search target buffer
    /// and the visible matches are window-scoped.
    pub interactive_replace_state: Option<crate::app::types::InteractiveReplaceState>,

    /// Cross-split scroll-sync manager for side-by-side diff views.
    /// Per-window because the splits it pairs are per-window.
    pub scroll_sync_manager: crate::view::scroll_sync::ScrollSyncManager,

    /// Whether the file-explorer panel is visible in this window.
    pub file_explorer_visible: bool,

    /// Whether a file-explorer rebuild is in flight (debounce flag).
    pub file_explorer_sync_in_progress: bool,

    /// Width of the file-explorer panel.
    pub file_explorer_width: crate::config::ExplorerWidth,

    /// Side (left/right) the file-explorer panel docks on.
    pub file_explorer_side: crate::config::FileExplorerSide,

    /// Pending toggles for show-hidden/show-gitignored that apply on
    /// the next file-explorer rebuild.
    pub pending_file_explorer_show_hidden: Option<bool>,
    pub pending_file_explorer_show_gitignored: Option<bool>,

    /// Decorations supplied by plugins for the file explorer (badges,
    /// status icons, etc.) keyed by absolute path.
    pub file_explorer_decorations:
        HashMap<String, Vec<crate::view::file_tree::FileExplorerDecoration>>,

    /// Compiled decoration lookup cache invalidated when
    /// `file_explorer_decorations` changes.
    pub file_explorer_decoration_cache: crate::view::file_tree::FileExplorerDecorationCache,
}

impl Window {
    /// Apply LSP folding ranges to the named buffer's `folding_ranges`
    /// store. Pure window mutation — no editor-global state touched.
    /// Used by the LSP folding-ranges response dispatcher after the
    /// editor-global URI-keyed map has been updated.
    pub fn apply_folding_ranges_response(
        &mut self,
        buffer_id: BufferId,
        lsp_ranges: Vec<lsp_types::FoldingRange>,
    ) {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return;
        };
        state
            .folding_ranges
            .set_from_lsp(&state.buffer, &mut state.marker_list, lsp_ranges);
    }

    /// Allocate a fresh per-window LSP request id and return it. The
    /// counter is per-window because each window's `LspManager` talks
    /// to its own server connections — no global namespace needed.
    pub fn alloc_lsp_request_id(&mut self) -> u64 {
        let id = self.next_lsp_request_id;
        self.next_lsp_request_id += 1;
        id
    }

    /// Apply an event to a buffer + the cursors of a split inside this
    /// window. Window-level method (not Editor-level) so the borrow
    /// checker can split-borrow `self.buffers` and `self.splits`
    /// cleanly without inline `self.windows.get_mut(...)` boilerplate
    /// at the call site. No-op if the buffer or split is missing.
    pub fn apply_event_to_buffer(
        &mut self,
        buffer_id: BufferId,
        split_id: LeafId,
        event: &crate::model::event::Event,
    ) {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return;
        };
        let Some((_, vs_map)) = self.splits.as_mut() else {
            return;
        };
        let Some(vs) = vs_map.get_mut(&split_id) else {
            return;
        };
        state.apply(&mut vs.cursors, event);
    }

    /// Same as [`apply_event_to_buffer`] but operates on a buffer-group
    /// panel's keyed cursor (the `keyed_states[buffer_id].cursors`
    /// inside the host split's view state, not the host's own cursors).
    /// Used by event-apply paths that target a focused inner panel of
    /// a Grouped split rather than the outer split's leaf buffer.
    pub fn apply_event_to_keyed_buffer(
        &mut self,
        buffer_id: BufferId,
        split_id: LeafId,
        event: &crate::model::event::Event,
    ) {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return;
        };
        let Some((_, vs_map)) = self.splits.as_mut() else {
            return;
        };
        let Some(vs) = vs_map.get_mut(&split_id) else {
            return;
        };
        let Some(keyed) = vs.keyed_states.get_mut(&buffer_id) else {
            return;
        };
        state.apply(&mut keyed.cursors, event);
    }

    /// Scroll the named split's viewport so the buffer's primary cursor
    /// is visible. Calls into `SplitViewState::ensure_cursor_visible`
    /// with the buffer's text + marker list. No-op if buffer/split is
    /// missing.
    pub fn ensure_cursor_visible_for_split(&mut self, buffer_id: BufferId, split_id: LeafId) {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return;
        };
        let Some((_, vs_map)) = self.splits.as_mut() else {
            return;
        };
        let Some(vs) = vs_map.get_mut(&split_id) else {
            return;
        };
        vs.ensure_cursor_visible(&mut state.buffer, &state.marker_list);
    }

    /// Scroll a split's viewport to the given line, given a buffer to
    /// resolve the line→byte offset. No-op if buffer/split is missing.
    /// `lock_against_ensure_visible`: when true, sets the
    /// skip-ensure-visible flag so the next render's cursor-visibility
    /// pass doesn't undo this scroll. Plugin-driven jumps want true;
    /// scroll-sync-from-active-to-other-splits wants false.
    pub fn scroll_split_viewport_to(
        &mut self,
        buffer_id: BufferId,
        split_id: LeafId,
        target_line: usize,
        lock_against_ensure_visible: bool,
    ) {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return;
        };
        let Some((_, vs_map)) = self.splits.as_mut() else {
            return;
        };
        let Some(vs) = vs_map.get_mut(&split_id) else {
            return;
        };
        vs.viewport.scroll_to(&mut state.buffer, target_line);
        if lock_against_ensure_visible {
            vs.viewport.set_skip_ensure_visible();
        }
    }

    /// Add a collapsed fold range on `buffer_id`'s marker list and on
    /// every view state hosting the buffer. Returns `true` when the
    /// buffer was found (so the caller knows to flag a render). No-op
    /// when the buffer is missing.
    pub fn add_fold(
        &mut self,
        buffer_id: BufferId,
        start: usize,
        end: usize,
        placeholder: Option<String>,
    ) -> bool {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return false;
        };
        let Some((_, vs_map)) = self.splits.as_mut() else {
            return false;
        };
        for vs in vs_map.values_mut() {
            if vs.keyed_states.contains_key(&buffer_id) {
                let buf_state = vs.ensure_buffer_state(buffer_id);
                buf_state
                    .folds
                    .add(&mut state.marker_list, start, end, placeholder.clone());
            }
        }
        true
    }

    /// Clear every fold range on `buffer_id` across the window's view
    /// states. Returns `true` when the buffer was found.
    pub fn clear_folds(&mut self, buffer_id: BufferId) -> bool {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return false;
        };
        let Some((_, vs_map)) = self.splits.as_mut() else {
            return false;
        };
        for vs in vs_map.values_mut() {
            if vs.keyed_states.contains_key(&buffer_id) {
                let buf_state = vs.ensure_buffer_state(buffer_id);
                buf_state.folds.clear(&mut state.marker_list);
            }
        }
        true
    }

    /// Move every supplied split's primary cursor to `position` in
    /// `buffer_id` and re-anchor the viewport to keep it visible.
    /// Caller is responsible for computing `splits` (typically by
    /// walking the split tree plus any grouped subtrees on the
    /// editor — those live outside the window). No-op for missing
    /// buffer/splits.
    pub fn set_buffer_cursor_in_splits(
        &mut self,
        buffer_id: BufferId,
        position: usize,
        splits: &[LeafId],
    ) {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return;
        };
        let Some((_, vs_map)) = self.splits.as_mut() else {
            return;
        };
        for leaf_id in splits {
            let Some(view_state) = vs_map.get_mut(leaf_id) else {
                continue;
            };
            view_state.cursors.primary_mut().move_to(position, false);
            view_state.ensure_cursor_visible(&mut state.buffer, &state.marker_list);
        }
    }

    /// Scroll `leaf_id`'s viewport so the byte position `top_byte` is
    /// the new top line, using `buffer_id` to resolve byte→line. Sets
    /// `skip_ensure_visible` so the next render's cursor-visibility
    /// pass doesn't undo the plugin-driven scroll. No-op for missing
    /// buffer/split.
    pub fn set_split_scroll_to_byte(
        &mut self,
        buffer_id: BufferId,
        leaf_id: LeafId,
        top_byte: usize,
    ) {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return;
        };
        let Some((_, vs_map)) = self.splits.as_mut() else {
            return;
        };
        let Some(view_state) = vs_map.get_mut(&leaf_id) else {
            return;
        };
        let total_bytes = state.buffer.len();
        let clamped_byte = top_byte.min(total_bytes);
        let target_line = state
            .buffer
            .offset_to_position(clamped_byte)
            .map(|p| p.line)
            .unwrap_or(0);
        view_state
            .viewport
            .scroll_to(&mut state.buffer, target_line);
        view_state.viewport.top_byte = clamped_byte;
        view_state.viewport.top_view_line_offset = 0;
        view_state.viewport.set_skip_ensure_visible();
    }

    /// Scroll every supplied split so `line` is roughly a third
    /// from the top of the viewport, using `buffer_id` for line
    /// resolution. Used for plugin-driven "scroll buffer to line"
    /// where the caller has already collected target leaves
    /// (including those from grouped subtrees).
    pub fn scroll_buffer_to_line_in_splits(
        &mut self,
        buffer_id: BufferId,
        target_leaves: &[LeafId],
        line: usize,
    ) {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return;
        };
        let Some((_, vs_map)) = self.splits.as_mut() else {
            return;
        };
        for leaf_id in target_leaves {
            let Some(view_state) = vs_map.get_mut(leaf_id) else {
                continue;
            };
            let viewport_height = view_state.viewport.height as usize;
            let lines_above = viewport_height / 3;
            let target = line.saturating_sub(lines_above);
            view_state.viewport.scroll_to(&mut state.buffer, target);
            view_state.viewport.set_skip_ensure_visible();
        }
    }

    /// Apply a previously-saved cursor + scroll position to a
    /// specific buffer's keyed view state inside a specific split.
    /// Restoration must NOT go through `view_state.viewport` /
    /// `view_state.cursors` — those Deref to the split's *active*
    /// buffer's view, which for `open_file_no_focus` is still the
    /// previously-active buffer; writing through the Deref would
    /// scroll the unrelated active buffer. After restoring the
    /// fields, reconciles cursor visibility against viewport
    /// (#1689 follow-up). No-op if buffer/split is missing.
    pub fn restore_buffer_state_in_split(
        &mut self,
        buffer_id: BufferId,
        split_id: LeafId,
        file_state: &crate::workspace::SerializedFileState,
    ) {
        let buffer_state = self.buffers.get_mut(&buffer_id);
        let view_state = self
            .splits
            .as_mut()
            .and_then(|(_, vs_map)| vs_map.get_mut(&split_id));
        let (Some(view_state), Some(buffer_state)) = (view_state, buffer_state) else {
            return;
        };
        let max_pos = buffer_state.buffer.len();
        let Some(buf_state) = view_state.keyed_states.get_mut(&buffer_id) else {
            return;
        };
        let cursor_pos = file_state.cursor.position.min(max_pos);
        buf_state.cursors.primary_mut().position = cursor_pos;
        buf_state.cursors.primary_mut().anchor = file_state.cursor.anchor.map(|a| a.min(max_pos));
        buf_state.viewport.top_byte = file_state.scroll.top_byte;
        buf_state.viewport.left_column = file_state.scroll.left_column;
        crate::app::navigation::reconcile_restored_buffer_view(buf_state, &mut buffer_state.buffer);
    }

    /// Configure `leaf_id`'s viewport for a terminal-buffer
    /// scrollback view: disable line wrap, clear any pending
    /// skip-ensure-visible flag, then scroll so the buffer's primary
    /// cursor (positioned at end-of-buffer when entering scrollback)
    /// is visible. No-op if the buffer or split is missing.
    pub fn enter_terminal_scrollback_view(&mut self, buffer_id: BufferId, leaf_id: LeafId) {
        let Some((_, vs_map)) = self.splits.as_mut() else {
            return;
        };
        let Some(view_state) = vs_map.get_mut(&leaf_id) else {
            return;
        };
        view_state.viewport.line_wrap_enabled = false;
        view_state.viewport.clear_skip_ensure_visible();
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            view_state.ensure_cursor_visible(&mut state.buffer, &state.marker_list);
        }
    }

    /// Install a freshly-loaded `EditorState` for a terminal buffer:
    /// replace the slot's state, push every per-split cursor showing
    /// the buffer to end-of-buffer (scrollback start), clear the
    /// modified flag (terminals are never user-modified), disable
    /// editing (scrollback mode), and turn off line-number margins.
    /// Used by workspace restore when re-loading the on-disk
    /// rendering of a previously-running terminal.
    pub fn install_terminal_buffer_state(
        &mut self,
        buffer_id: BufferId,
        new_state: crate::state::EditorState,
    ) {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return;
        };
        *state = new_state;
        let total = state.buffer.total_bytes();
        if let Some((_, vs_map)) = self.splits.as_mut() {
            for vs in vs_map.values_mut() {
                if vs.has_buffer(buffer_id) {
                    vs.cursors.primary_mut().position = total;
                }
            }
        }
        state.buffer.set_modified(false);
        state.editing_disabled = true;
        state.margins.configure_for_line_numbers(false);
    }

    /// Scroll `leaf_id`'s viewport by `delta` lines (negative = up,
    /// positive = down). Honours `view_transform_tokens` when present
    /// (uses view-aware scrolling) and falls back to buffer-based
    /// `scroll_up` / `scroll_down`. After scrolling, skips
    /// ensure_visible and snaps the viewport top to a fold boundary
    /// if the new top byte landed inside a collapsed fold.
    /// `tab_size` is needed for view-line tokenization.
    pub fn scroll_split_by_lines(
        &mut self,
        buffer_id: BufferId,
        leaf_id: LeafId,
        delta: i32,
        view_transform_tokens: Option<Vec<fresh_core::api::ViewTokenWire>>,
        tab_size: usize,
    ) {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return;
        };
        let Some((_, vs_map)) = self.splits.as_mut() else {
            return;
        };
        let Some(view_state) = vs_map.get_mut(&leaf_id) else {
            return;
        };

        let soft_breaks = state.collect_soft_break_positions();
        let virtual_lines = state.collect_virtual_line_positions();
        let buffer = &mut state.buffer;
        let top_byte_before = view_state.viewport.top_byte;
        if let Some(tokens) = view_transform_tokens {
            use crate::view::ui::view_pipeline::ViewLineIterator;
            let view_lines: Vec<_> =
                ViewLineIterator::new(&tokens, false, false, tab_size, false).collect();
            view_state
                .viewport
                .scroll_view_lines(&view_lines, delta as isize);
        } else if delta < 0 {
            let lines_to_scroll = delta.unsigned_abs() as usize;
            view_state
                .viewport
                .scroll_up(buffer, &soft_breaks, &virtual_lines, lines_to_scroll);
        } else {
            let lines_to_scroll = delta as usize;
            view_state
                .viewport
                .scroll_down(buffer, &soft_breaks, &virtual_lines, lines_to_scroll);
        }
        view_state.viewport.set_skip_ensure_visible();

        if let Some(folds) = view_state.keyed_states.get(&buffer_id).map(|bs| &bs.folds) {
            if !folds.is_empty() {
                let top_line = buffer.get_line_number(view_state.viewport.top_byte);
                if let Some(range) = folds
                    .resolved_ranges(buffer, &state.marker_list)
                    .iter()
                    .find(|r| top_line >= r.start_line && top_line <= r.end_line)
                {
                    let target_line = if delta >= 0 {
                        range.end_line.saturating_add(1)
                    } else {
                        range.header_line
                    };
                    let target_byte = buffer
                        .line_start_offset(target_line)
                        .unwrap_or_else(|| buffer.len());
                    view_state.viewport.top_byte = target_byte;
                    view_state.viewport.top_view_line_offset = 0;
                }
            }
        }
        tracing::trace!(
            "scroll_split_by_lines: delta={}, top_byte {} -> {}",
            delta,
            top_byte_before,
            view_state.viewport.top_byte
        );
    }

    /// Clear LSP-related overlays (diagnostics, virtual texts,
    /// folding ranges, and folds) for `buffer_id`, used when LSP is
    /// being disabled for the buffer. Pure window-state mutation.
    pub fn clear_lsp_overlays_for_buffer(
        &mut self,
        buffer_id: BufferId,
        diagnostic_namespace: &crate::model::event::OverlayNamespace,
    ) {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return;
        };
        state
            .overlays
            .clear_namespace(diagnostic_namespace, &mut state.marker_list);
        state.virtual_texts.clear(&mut state.marker_list);
        state.folding_ranges.clear(&mut state.marker_list);
        let Some((_, vs_map)) = self.splits.as_mut() else {
            return;
        };
        for view_state in vs_map.values_mut() {
            if let Some(buf_state) = view_state.keyed_states.get_mut(&buffer_id) {
                buf_state.folds.clear(&mut state.marker_list);
            }
        }
    }

    /// Mutable handle to this window's split tree (or `None` when
    /// the layout hasn't been seeded yet). Useful at sites where
    /// the caller already has a `&mut Window` from a direct
    /// `self.windows.get_mut(&id)` and wants the split layout
    /// without going back through Editor's accessor.
    pub fn split_manager_mut(&mut self) -> Option<&mut SplitManager> {
        self.splits.as_mut().map(|(mgr, _)| mgr)
    }

    /// Mutable handle to this window's per-leaf view state map.
    pub fn split_view_states_mut(&mut self) -> Option<&mut HashMap<LeafId, SplitViewState>> {
        self.splits.as_mut().map(|(_, vs)| vs)
    }

    /// Both halves of the split layout at once. Returns `None` if
    /// the layout hasn't been seeded yet.
    pub fn splits_mut(
        &mut self,
    ) -> Option<(&mut SplitManager, &mut HashMap<LeafId, SplitViewState>)> {
        self.splits.as_mut().map(|(m, vs)| (m, vs))
    }

    /// Construct a window.
    ///
    /// `root` is taken as-is (the caller is responsible for
    /// canonicalisation). `label` defaults to the basename of
    /// `root` when empty. `resources` is the editor-global service
    /// bundle every window holds an `Arc`-cloned reference to — see
    /// [`WindowResources`] for the rationale.
    pub fn new(
        id: WindowId,
        label: impl Into<String>,
        root: PathBuf,
        resources: WindowResources,
    ) -> Self {
        let mut label = label.into();
        if label.is_empty() {
            label = root
                .file_name()
                .and_then(|n| n.to_str())
                .map(str::to_owned)
                .unwrap_or_else(|| "main".to_owned());
        }
        Self {
            id,
            label,
            root,
            file_explorer: None,
            file_mod_times: HashMap::new(),
            plugin_state: HashMap::new(),
            lsp: None,
            panel_ids: HashMap::new(),
            splits: None,
            buffers: HashMap::new(),
            buffer_metadata: HashMap::new(),
            terminal_manager: crate::services::terminal::TerminalManager::new(),
            terminal_buffers: HashMap::new(),
            terminal_backing_files: HashMap::new(),
            terminal_log_files: HashMap::new(),
            event_logs: HashMap::new(),
            status_message: None,
            plugin_status_message: None,
            prompt: None,
            bridge: crate::services::async_bridge::AsyncBridge::new(),
            next_lsp_request_id: 0,
            pending_completion_requests: std::collections::HashSet::new(),
            completion_items: None,
            scheduled_completion_trigger: None,
            dabbrev_state: None,
            pending_goto_definition_request: None,
            pending_references_request: None,
            pending_references_symbol: String::new(),
            pending_signature_help_request: None,
            pending_code_actions_requests: std::collections::HashSet::new(),
            pending_code_actions_server_names: std::collections::HashMap::new(),
            pending_code_actions: None,
            pending_inlay_hints_requests: std::collections::HashMap::new(),
            pending_folding_range_requests: std::collections::HashMap::new(),
            folding_ranges_in_flight: std::collections::HashMap::new(),
            folding_ranges_debounce: std::collections::HashMap::new(),
            pending_semantic_token_requests: std::collections::HashMap::new(),
            semantic_tokens_in_flight: std::collections::HashMap::new(),
            semantic_tokens_full_debounce: std::collections::HashMap::new(),
            pending_semantic_token_range_requests: std::collections::HashMap::new(),
            semantic_tokens_range_in_flight: std::collections::HashMap::new(),
            semantic_tokens_range_last_request: std::collections::HashMap::new(),
            semantic_tokens_range_applied: std::collections::HashMap::new(),
            position_history: crate::input::position_history::PositionHistory::new(),
            in_navigation: false,
            suppress_position_history_once: false,
            bookmarks: crate::app::bookmarks::BookmarkState::default(),
            grouped_subtrees: HashMap::new(),
            composite_buffers: HashMap::new(),
            composite_view_states: HashMap::new(),
            layout_cache: WindowLayoutCache::default(),
            preview: None,
            terminal_mode: false,
            terminal_mode_resume: std::collections::HashSet::new(),
            seen_byte_ranges: HashMap::new(),
            previous_viewports: HashMap::new(),
            same_buffer_scroll_sync: false,
            interactive_replace_state: None,
            scroll_sync_manager: crate::view::scroll_sync::ScrollSyncManager::new(),
            file_explorer_visible: false,
            file_explorer_sync_in_progress: false,
            file_explorer_width: resources.config.file_explorer.width,
            file_explorer_side: resources.config.file_explorer.side,
            pending_file_explorer_show_hidden: None,
            pending_file_explorer_show_gitignored: None,
            file_explorer_decorations: HashMap::new(),
            file_explorer_decoration_cache:
                crate::view::file_tree::FileExplorerDecorationCache::default(),
            resources,
        }
    }

    // ---- Resource accessors (canonical reading API) ----
    //
    // These are thin wrappers around `self.resources.X` for the most
    // commonly-read resources. Use them at sites where the borrow
    // checker is happy with a method call; fall back to direct
    // `self.resources.X` field access at sites that need to split-borrow
    // alongside other Window sub-fields.

    /// Read-only handle to editor configuration.
    pub fn config(&self) -> &crate::config::Config {
        &self.resources.config
    }

    /// Active filesystem authority (local / devcontainer / remote).
    pub fn authority(&self) -> &crate::services::authority::Authority {
        &self.resources.authority
    }

    /// Allocate the next globally-unique `BufferId`.
    pub fn alloc_buffer_id(&self) -> BufferId {
        self.resources.buffer_id_alloc.next()
    }

    /// Set this window's status-bar message. Mirrors
    /// `Editor::set_status_message` — moved here so handlers on
    /// `impl Window` can post status without an `Editor` reference.
    /// Clears any plugin-supplied status (matches Editor behaviour).
    pub fn set_status_message(&mut self, message: String) {
        tracing::info!(target: "status", "{}", message);
        self.plugin_status_message = None;
        self.status_message = Some(message);
    }

    /// Clear this window's status-bar message.
    pub fn clear_status_message(&mut self) {
        self.status_message = None;
    }

    /// Resolve the effective (split, buffer) pair for the currently-
    /// focused target inside this window. Returned invariant: the split
    /// id is in `splits.1` (view_states), its `active_buffer` equals
    /// the returned buffer id, `self.buffers` contains the buffer id,
    /// and the split's `keyed_states` contains an entry for the buffer.
    ///
    /// Falls back to the outer split when a buffer-group panel is
    /// focused but any of those invariants doesn't hold for the inner
    /// leaf. Mirrors `Editor::effective_active_pair`.
    pub fn effective_active_pair(&self) -> (LeafId, BufferId) {
        let (mgr, vs_map) = self
            .splits
            .as_ref()
            .expect("active window must have a populated split layout");
        let active_split = mgr.active_split();
        if let Some(vs) = vs_map.get(&active_split) {
            if vs.active_group_tab.is_some() {
                if let Some(inner_leaf) = vs.focused_group_leaf {
                    if let Some(inner_vs) = vs_map.get(&inner_leaf) {
                        let inner_buf = inner_vs.active_buffer;
                        if self.buffers.contains_key(&inner_buf)
                            && inner_vs.keyed_states.contains_key(&inner_buf)
                        {
                            return (inner_leaf, inner_buf);
                        }
                    }
                }
            }
        }
        let outer_buf = mgr
            .active_buffer_id()
            .expect("Editor always has at least one buffer");
        (active_split, outer_buf)
    }

    /// The id of the buffer currently focused in this window.
    #[inline]
    pub fn active_buffer(&self) -> BufferId {
        let (_, buf) = self.effective_active_pair();
        buf
    }

    /// The split id whose `SplitViewState` owns the currently-focused
    /// cursors/viewport for this window.
    #[inline]
    pub fn effective_active_split(&self) -> LeafId {
        let (split, _) = self.effective_active_pair();
        split
    }

    /// Read-only handle to this window's active buffer state. Panics
    /// if the active buffer is missing — the invariants on
    /// `effective_active_pair` guarantee it's present.
    pub fn active_state(&self) -> &crate::state::EditorState {
        let buf = self.active_buffer();
        self.buffers
            .get(&buf)
            .expect("active buffer must be present in window")
    }

    /// Mutable handle to this window's active buffer state.
    pub fn active_state_mut(&mut self) -> &mut crate::state::EditorState {
        let buf = self.active_buffer();
        self.buffers
            .get_mut(&buf)
            .expect("active buffer must be present in window")
    }

    /// Read-only cursor set for the active buffer in the active split.
    /// Group panels return their own cursors, not the outer split's
    /// stale ones.
    pub fn active_cursors(&self) -> &crate::model::cursor::Cursors {
        let split_id = self.effective_active_split();
        &self
            .splits
            .as_ref()
            .expect("active window must have a populated split layout")
            .1
            .get(&split_id)
            .expect("active split must be in view-state map")
            .cursors
    }

    /// Mutable cursor set for the active buffer in the active split.
    pub fn active_cursors_mut(&mut self) -> &mut crate::model::cursor::Cursors {
        let split_id = self.effective_active_split();
        &mut self
            .splits
            .as_mut()
            .expect("active window must have a populated split layout")
            .1
            .get_mut(&split_id)
            .expect("active split must be in view-state map")
            .cursors
    }

    /// Read-only event log for the active buffer.
    pub fn active_event_log(&self) -> &crate::model::event::EventLog {
        let buf = self.active_buffer();
        self.event_logs
            .get(&buf)
            .expect("active buffer must have an event log")
    }

    /// Mutable event log for the active buffer.
    pub fn active_event_log_mut(&mut self) -> &mut crate::model::event::EventLog {
        let buf = self.active_buffer();
        self.event_logs
            .get_mut(&buf)
            .expect("active buffer must have an event log")
    }

    // ---- Preview-tab methods ----

    /// Promote a specific buffer from preview to permanent, if it was
    /// in preview mode. No-op if the buffer is not currently a preview.
    pub fn promote_buffer_from_preview(&mut self, buffer_id: BufferId) {
        if let Some(m) = self.buffer_metadata.get_mut(&buffer_id) {
            m.is_preview = false;
        }
        if let Some((_, id)) = self.preview {
            if id == buffer_id {
                self.preview = None;
            }
        }
    }

    /// Promote the active buffer from preview to permanent. Called on
    /// any buffer mutation so touching a preview buffer commits it.
    pub fn promote_active_buffer_from_preview(&mut self) {
        let id = self.active_buffer();
        self.promote_buffer_from_preview(id);
    }

    /// Promote the current preview, regardless of which buffer it
    /// points at. Used before layout changes (split, close-split,
    /// move-tab) where the preview invariant ("anchored to a specific
    /// split") would otherwise be broken by the operation itself.
    pub fn promote_current_preview(&mut self) {
        if let Some((_, id)) = self.preview.take() {
            if let Some(m) = self.buffer_metadata.get_mut(&id) {
                m.is_preview = false;
            }
        }
    }

    /// Promote the current preview if it belongs to a split other
    /// than `new_split`. Called from split-focus-change paths so
    /// that moving focus away from the preview's pane commits it.
    pub fn promote_preview_if_not_in_split(&mut self, new_split: LeafId) {
        if let Some((preview_split, _)) = self.preview {
            if preview_split != new_split {
                self.promote_current_preview();
            }
        }
    }

    /// Whether the given buffer is currently in preview (ephemeral)
    /// mode. Primarily for tests; production code reads
    /// `self.preview` or relies on the `is_preview` flag in the
    /// buffer's metadata.
    pub fn is_buffer_preview(&self, buffer_id: BufferId) -> bool {
        self.buffer_metadata
            .get(&buffer_id)
            .map(|m| m.is_preview)
            .unwrap_or(false)
    }

    /// The (split, buffer) tuple of the current preview tab, if any.
    /// Intended for tests that verify preview anchoring semantics.
    pub fn current_preview(&self) -> Option<(LeafId, BufferId)> {
        self.preview
    }
}

// Label-defaulting unit tests (`empty_label_defaults_to_root_basename`,
// `explicit_label_is_kept`, `empty_label_with_rootless_path_falls_back_to_main`)
// were removed when `Window::new` started taking a `WindowResources`
// argument — stubbing every editor-global service for a 3-line label
// assertion isn't worth the maintenance, and the same behaviour is
// already exercised by every `EditorTestHarness::create` path that
// names a window.
